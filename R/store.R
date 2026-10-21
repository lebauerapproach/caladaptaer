# Read coordinate arrays once; climate values are read only for requested windows.
.store_metadata_cache <- new.env(parent = emptyenv())

.store_path <- function(path) {
  if (startsWith(path, "s3://")) .s3_to_vsi(path) else path
}

.store_metadata <- function(path, variable) {
  key <- paste(path, variable, sep = "|")
  if (exists(key, .store_metadata_cache, inherits = FALSE)) {
    return(get(key, .store_metadata_cache, inherits = FALSE))
  }
  .setup_s3_env()
  if (startsWith(path, "s3://") &&
    utils::compareVersion(sf::sf_extSoftVersion()[["GDAL"]], "3.9.0") < 0) {
    stop("Remote Zarr reads require R-linked GDAL >= 3.9.0", call. = FALSE)
  }
  meta <- sf::gdal_read_mdim(.store_path(path), variable, proxy = TRUE)
  if (!"time" %in% names(meta$dimensions)) stop("Store has no time axis")
  if (is.na(meta$srs) && all(c("lon", "lat") %in% names(meta$dimensions)) &&
      identical(attr(meta$dimensions$lon$values[[1]], "units"), "degrees_east") &&
      identical(attr(meta$dimensions$lat$values[[1]], "units"), "degrees_north")) {
    meta$srs <- sf::st_crs(4326)$wkt
  }
  axis <- meta$dimensions$time$values[[1]]
  encoding <- attr(axis, "units")
  attrs <- attr(axis, "attributes")
  calendar <- if ("calendar" %in% names(attrs)) attrs[["calendar"]] else "standard"
  if (calendar %in% c("standard", "gregorian", "proleptic_gregorian")) {
    enc <- strsplit(encoding, " since ", fixed = TRUE)[[1]]
    seconds <- switch(enc[1],
      seconds = 1,
      second = 1,
      hours = 3600,
      hour = 3600,
      days = 86400,
      day = 86400,
      stop("Unsupported time units: ", encoding)
    )
    times <- .parse_time(enc[2]) + as.numeric(axis) * seconds
  } else {
    times <- CFtime::as_timestamp(CFtime::CFtime(encoding, calendar, as.numeric(axis)),
      asPOSIX = FALSE
    )
  }
  if (!inherits(times, "POSIXct")) {
    labels <- sub("T", " ", times, fixed = TRUE)
    times <- .parse_time(labels)
    if (anyNA(times) || any(format(times, "%Y-%m-%d %H:%M:%S", tz = "UTC") != labels)) {
      stop("Source calendar contains dates not representable by POSIXct; no calendar conversion is performed")
    }
  }
  if (anyNA(times) || any(diff(as.numeric(times)) <= 0)) {
    stop("Store timestamps must be known and strictly increasing")
  }
  meta$calendar <- calendar
  meta$time_units <- encoding
  meta$time_values <- as.numeric(axis)
  meta$times <- times
  meta$path <- path
  meta$variable <- variable
  assign(key, meta, .store_metadata_cache)
  meta
}

.select_store_time <- function(times, start_time = NULL, end_time = NULL,
                               n_timesteps = NULL) {
  start <- .parse_time(start_time)
  end <- .parse_time(end_time)
  if ((!is.null(start) && (is.na(start) || start < min(times) || start > max(times))) ||
    (!is.null(end) && (is.na(end) || end > max(times) || end < min(times)))) {
    stop("Requested time range is outside source coverage", call. = FALSE)
  }
  keep <- rep(TRUE, length(times))
  if (!is.null(start)) keep <- keep & times >= start
  if (!is.null(end)) keep <- keep & times <= end
  indices <- which(keep)
  if (!is.null(n_timesteps)) {
    if (length(n_timesteps) != 1 || is.na(n_timesteps) ||
      n_timesteps < 1 || n_timesteps != as.integer(n_timesteps)) {
      stop("Invalid n_timesteps")
    }
    if (length(indices) < n_timesteps) stop("Insufficient source timesteps")
    indices <- utils::head(indices, n_timesteps)
  }
  if (!length(indices)) stop("Requested time range has no source timestamps")
  indices
}

.check_grid_bounds <- function(xy, xs, ys) {
  inside <- function(v, axis) {
    half <- if (length(axis) > 1) abs(axis[2] - axis[1]) / 2 else 0
    is.finite(v) & v >= min(axis) - half & v <= max(axis) + half
  }
  if (any(!inside(xy[, 1], xs) | !inside(xy[, 2], ys))) {
    stop("Requested points fall outside the source grid", call. = FALSE)
  }
}

.store_grid <- function(meta) {
  xs <- as.numeric(meta$dimensions$x$values[[1]])
  ys <- as.numeric(meta$dimensions$y$values[[1]])
  if (!length(xs) || !length(ys) || is.na(meta$srs)) stop("WRF grid/CRS is missing")
  dims <- stars::st_dimensions(x = xs, y = ys, cell_midpoints = TRUE)
  grid <- stars::st_as_stars(list(grid = array(NA_real_, c(length(xs), length(ys)))), dimensions = dims)

  sf::st_set_crs(grid, meta$srs)
}

.read_store_window <- function(path, variable, start_time = NULL, end_time = NULL,
                               n_timesteps = NULL, points = NULL) {
  meta <- .store_metadata(path, variable)
  dimensions <- names(meta$dimensions)
  if (length(dimensions) != 3) stop("Expected two spatial dimensions and time")
  selection <- lapply(meta$dimensions, function(d) seq_len(d$to - d$from + 1L))
  selection$time <- .select_store_time(meta$times, start_time, end_time, n_timesteps)
  if (!is.null(points)) {
    cells <- cae_grid_cells(points, .store_grid(meta))
    selection$x <- seq(min(cells$cell_i), max(cells$cell_i))
    selection$y <- seq(min(cells$cell_j), max(cells$cell_j))
  }
  offset <- vapply(selection, min, integer(1)) - 1L
  count <- lengths(selection)
  raw <- sf::gdal_read_mdim(.store_path(path), variable,
    offset = unname(offset), count = unname(count)
  )
  values <- raw$array_list[[1]]
  units <- attr(values, "units")
  arr <- array(as.numeric(values), rev(count))
  axes <- lapply(rev(dimensions), function(name) {
    if (name == "time") {
      meta$times[selection$time]
    } else {
      as.numeric(meta$dimensions[[name]]$values[[1]])[selection[[name]]]
    }
  })
  names(axes) <- rev(dimensions)
  dims <- do.call(stars::st_dimensions, c(axes, list(
    .raster = rev(dimensions)[1:2],
    cell_midpoints = TRUE
  )))
  dims$time <- stars::st_dimensions(time = meta$times[selection$time], point = TRUE)$time
  result <- stars::st_as_stars(stats::setNames(list(arr), variable), dimensions = dims)
  if (!is.null(units) && nzchar(units)) result[[1]] <- units::set_units(result[[1]], units, mode = "standard")
  if (!is.na(meta$srs)) result <- sf::st_set_crs(result, meta$srs)
  attr(result, "cae_provenance") <- .source_provenance(path, variable, meta, selection$time)
  result
}

.resolve_store <- function(variable, model, scenario, timescale, resolution, source = NULL) {
  if (is.list(source)) {
    if (is.null(source[[variable]])) stop("Local source missing variable: ", variable)
    return(source[[variable]])
  }
  if (!is.null(source)) {
    if (file.exists(source) && !dir.exists(source)) {
      return(source)
    }
    if (file.exists(file.path(source, ".zgroup")) || file.exists(file.path(source, ".zmetadata"))) {
      return(source)
    }
    candidate <- file.path(source, variable)
    if (!dir.exists(candidate)) stop("Local source missing: ", candidate)
    return(candidate)
  }
  activity <- .detect_activity(model, timescale, resolution, variable)
  cae_zarr_path(activity, model, scenario, variable, timescale, resolution)
}

.fetch_points_grid <- function(variable, model, scenario, points, timescale,
                               resolution, start_time, end_time, n_timesteps,
                               source = NULL) {
  path <- tryCatch(.resolve_store(variable, model, scenario, timescale, resolution, source),
    error = function(e) if (variable == "prec") NULL else stop(e)
  )
  if (!is.null(path)) {
    return(.read_store_window(path, variable, start_time, end_time, n_timesteps, points))
  }
  # Read one preceding source record so differencing retains the first increment.
  rain_path <- .resolve_store("rainc", model, scenario, timescale, resolution, source)
  meta <- .store_metadata(rain_path, "rainc")
  idx <- .select_store_time(meta$times, start_time, end_time, n_timesteps)
  if (min(idx) == 1L) stop("Precipitation requires a preceding accumulation record")
  padded_start <- meta$times[min(idx) - 1L]
  a <- .read_store_window(rain_path, "rainc", padded_start, meta$times[max(idx)], points = points)
  b <- .read_store_window(.resolve_store("rainnc", model, scenario, timescale, resolution, source),
    "rainnc", padded_start, meta$times[max(idx)],
    points = points
  )
  if (!identical(.get_time_values(a), .get_time_values(b))) stop("Precipitation component times differ")
  a[[1]] <- a[[1]] + b[[1]]
  a <- .deaccumulate_precip(a)
  dims <- stars::st_dimensions(a)
  dims$time <- stars::st_dimensions(time = .get_time_values(a)[-1], point = TRUE)$time
  out <- stars::st_as_stars(list(prec = a[[1]][, , -1, drop = FALSE]), dimensions = dims)
  p <- .combine_provenance(list(a, b))
  p$times <- p$times[-1]
  p$time_values <- p$time_values[-1]
  p$history <- c(p$history, "Derived precipitation increments from rainc + rainnc with counter-reset handling")
  attr(out, "cae_provenance") <- p
  out
}

#' Get WRF grid geometry without reading climate values
#' @inheritParams cae_fetch
#' @return A `stars` grid containing cell geometry and CRS; values are NA.
#' @export
cae_reference_grid <- function(model = "ERA5", scenario = "reanalysis",
                               resolution = "d03", source = NULL) {
  path <- .resolve_store("t2", model, scenario, "1hr", resolution, source)
  meta <- .store_metadata(path, "t2")
  grid <- .store_grid(meta)
  attr(grid, "source_time_start") <- min(meta$times)
  attr(grid, "source_time_end") <- max(meta$times)
  grid
}
