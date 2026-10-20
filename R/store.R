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
    labels <- times
    times <- .parse_time(labels)
    if (anyNA(times) || any(format(times, "%Y-%m-%d %H:%M:%S", tz = "UTC") != labels)) {
      stop("Source calendar contains dates not representable by POSIXct; no calendar conversion is performed")
    }
  }
  if (anyNA(times) || any(diff(as.numeric(times)) <= 0)) {
    stop("Store timestamps must be known and strictly increasing")
  }
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

.read_store_window <- function(path, variable, start_time = NULL, end_time = NULL,
                               n_timesteps = NULL) {
  meta <- .store_metadata(path, variable)
  dimensions <- names(meta$dimensions)
  if (length(dimensions) != 3) stop("Expected two spatial dimensions and time")
  selection <- lapply(meta$dimensions, function(d) seq_len(d$to - d$from + 1L))
  selection$time <- .select_store_time(meta$times, start_time, end_time, n_timesteps)
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
  result
}

