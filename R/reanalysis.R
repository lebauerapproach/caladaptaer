#' List hourly files from UCLA's ERA5-driven WRF reanalysis
#'
#' Currently supports UCLA's ERA5-driven WRF reanalysis only. GCM historical
#' simulations and other reanalysis products are not supported by this function.
#' @param start_time,end_time Inclusive UTC timestamps; include one preceding
#'   hour when precipitation must be differenced.
#' @param resolution WRF domain, default `d03` (3 km).
#' @return Data frame with `time`, `key`, `precipitation_key`, `size`, and
#'   `last_modified`. At overlapping September boundaries, precipitation_key
#'   identifies the previous run's final record for counter differencing.
#' @export
cae_reanalysis_files <- function(start_time, end_time, resolution = "d03") {
  resolution <- match.arg(resolution, .ucla_wrf_domains)
  start <- .parse_time(start_time)
  end <- .parse_time(end_time)
  if (is.na(start) || is.na(end) || start > end) stop("Invalid reanalysis period")
  source_year <- function(time) as.integer(format(time, "%Y")) - as.integer(format(time, "%m") < "09")
  years <- seq.int(source_year(start), source_year(end))
  files <- lapply(years, function(year) {
    x <- .ucla_wrf_list_prefix(.ucla_wrf_source_prefix(year, resolution))$contents
    x <- x[grepl("/auxhist_", x$key), , drop = FALSE]
    x$time <- as.POSIXct(sub(".*_([0-9]{4}-[0-9]{2}-[0-9]{2})_([0-9:]+)$", "\\1 \\2", x$key), tz = "UTC")
    x[x$time >= start & x$time <= end & !is.na(x$time), , drop = FALSE]
  })
  result <- do.call(rbind, files)
  result <- result[order(result$time), , drop = FALSE]
  result$precipitation_key <- result$key
  overlap <- duplicated(result$time) | duplicated(result$time, fromLast = TRUE)
  if (any(overlap)) {
    if (any(format(result$time[overlap], "%m-%d %H:%M:%S", tz = "UTC") != "09-01 00:00:00")) {
      stop("Unexpected overlapping UCLA source timestamps")
    }
    # Use the new water year's weather but difference the old run's final
    # accumulation against its preceding hour. The new run has a warm-start
    # precipitation offset, so its initial total is not an hourly increment.
    for (time in unique(as.numeric(result$time[overlap]))) {
      rows <- which(as.numeric(result$time) == time)
      result$precipitation_key[rows] <- result$key[rows[1]]
    }
    result <- result[!duplicated(result$time, fromLast = TRUE), , drop = FALSE]
  }
  rownames(result) <- NULL
  result
}

.raw_wrf_grid <- function(nc) {
  attr <- function(name) ncdf4::ncatt_get(nc, 0, name)$value
  if (!identical(as.integer(attr("MAP_PROJ")), 1L)) stop("Expected Lambert WRF grid")
  crs <- paste(
    "+proj=lcc +R=6370000 +units=m",
    paste0("+lat_1=", attr("TRUELAT1")), paste0("+lat_2=", attr("TRUELAT2")),
    paste0("+lat_0=", attr("MOAD_CEN_LAT")), paste0("+lon_0=", attr("STAND_LON"))
  )
  nx <- nc$dim$west_east$len
  ny <- nc$dim$south_north$len
  # UCLA auxhist files omit XLAT/XLONG; CEN_* describes the mass-grid center.
  anchor <- sf::st_transform(sf::st_sfc(sf::st_point(c(attr("CEN_LON"), attr("CEN_LAT"))), crs = 4326), crs)
  xy <- sf::st_coordinates(anchor)[1, ]
  xs <- xy[1] + (seq_len(nx) - (nx + 1) / 2) * attr("DX")
  ys <- xy[2] + (seq_len(ny) - (ny + 1) / 2) * attr("DY")
  dims <- stars::st_dimensions(x = xs, y = ys, cell_midpoints = TRUE)
  grid <- stars::st_as_stars(list(grid = array(NA_real_, c(nx, ny))), dimensions = dims)
  sf::st_set_crs(grid, crs)
}

.download_reanalysis_file <- function(key, cache_dir) {
  .check_cache_keys(key)
  path <- file.path(cache_dir, key)
  if (!file.exists(path)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    tmp <- tempfile(tmpdir = dirname(path))
    on.exit(unlink(tmp), add = TRUE)
    url <- paste0("https://", .ucla_wrf_bucket, ".s3.us-west-2.amazonaws.com/", key)
    status <- utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
    if (status != 0 || !file.exists(tmp) || file.info(tmp)$size == 0) stop("Reanalysis source download incomplete")
    if (!file.rename(tmp, path)) stop("Failed to stage reanalysis source")
  }
  path
}

# A private scratch directory owns disposable downloads. The reusable cache is
# read-only in this mode, so cleanup cannot remove a caller's pre-existing file.
.with_reanalysis_file <- function(key, cache_dir, retain_source, read) {
  existing <- file.path(cache_dir, key)
  if (retain_source || file.exists(existing)) {
    return(read(.download_reanalysis_file(key, cache_dir)))
  }
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  scratch <- tempfile("reanalysis-hour-", tmpdir = cache_dir)
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  read(.download_reanalysis_file(key, scratch))
}

#' Build CF drivers from UCLA's ERA5-driven WRF reanalysis
#'
#' Currently supports UCLA's ERA5-driven WRF reanalysis only. GCM historical
#' simulations and other reanalysis products are not supported by this function.
#'
#' Streams each source file across bounded cell batches before discarding it.
#' Source internal timestamps are preserved; nominal filename times are recorded
#' separately. No gaps are filled and no calendar conversion is performed.
#' @inheritParams cae_build_met_drivers
#' @param files Data frame from [cae_reanalysis_files()], with one preceding hour.
#' @param cache_dir Source-file cache directory.
#' @param resolution WRF domain, default `d03` (3 km).
#' @param retain_source Keep newly downloaded files (default TRUE). FALSE deletes
#'   only owned temporary downloads after all requested cells have been read.
#' @return Invisibly, a data frame of `site_id`, `year`, and output `file`.
#' @export
cae_build_reanalysis_drivers <- function(sites, files, start_year, end_year,
                                        outdir, cache_dir, resolution = "d03",
                                        overwrite = FALSE, verbose = TRUE,
                                        batch_size = 64L, retain_source = TRUE) {
  .validate_sites(sites)
  resolution <- match.arg(resolution, .ucla_wrf_domains)
  if (length(batch_size) != 1L || !is.finite(batch_size) || batch_size < 1 || batch_size != as.integer(batch_size)) stop("Invalid batch_size")
  if (length(retain_source) != 1L || is.na(retain_source) || !is.logical(retain_source)) stop("Invalid retain_source")
  if (start_year > end_year) stop("start_year exceeds end_year")
  if (!all(c("time", "key") %in% names(files))) stop("Provide source time and key")
  files$time <- .parse_time(files$time)
  if (anyNA(files$time) || anyDuplicated(files$time)) stop("Invalid source timestamps")
  .check_cache_keys(files$key)
  if (!"precipitation_key" %in% names(files)) files$precipitation_key <- files$key
  .check_cache_keys(files$precipitation_key)
  results <- list()
  for (year in seq.int(start_year, end_year)) {
    nominal <- seq(.parse_time(paste0(year, "-01-01")) - 3600,
      .parse_time(paste0(year, "-12-31 23:00:00")), by = "hour")
    rows <- match(nominal, files$time)
    if (anyNA(rows)) stop("Source coverage incomplete for ", year, " including preceding hour")
    selected <- files[rows, , drop = FALSE]
    manifest <- .reanalysis_manifest(selected, outdir, resolution, year)
    identity <- .driver_identity("ERA5", "reanalysis", resolution, list(manifest = manifest$checksum))
    paths <- file.path(outdir, sites$site_id, paste0("ERA5.reanalysis.", year, ".nc"))
    valid <- vapply(seq_len(nrow(sites)), function(i) .valid_reanalysis_file(paths[i], nominal[-1], sites$lat[i], sites$lon[i], identity), logical(1))
    wanted <- which(overwrite | !valid)
    if (length(wanted)) {
      .stream_reanalysis_year(sites[wanted, , drop = FALSE], selected, paths[wanted],
        cache_dir, resolution, year, batch_size, retain_source, manifest, identity, verbose)
    }
    results[[length(results) + 1L]] <- data.frame(site_id = sites$site_id, year = year, file = paths)
  }
  invisible(do.call(rbind, results))
}

.reanalysis_manifest <- function(files, outdir, resolution, year) {
  directory <- file.path(outdir, "source-manifests")
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(tmpdir = directory)
  on.exit(unlink(tmp))
  jsonlite::write_json(files, tmp, dataframe = "rows", auto_unbox = TRUE, digits = NA)
  checksum <- unname(tools::md5sum(tmp))
  path <- file.path(directory, paste0("ERA5.", resolution, ".", year, ".", checksum, ".json"))
  if (!file.exists(path) || !identical(unname(tools::md5sum(path)), checksum)) {
    if (!file.rename(tmp, path)) stop("Could not write source manifest")
  }
  list(path = path, checksum = checksum)
}

.reanalysis_provenance <- function(files, times, resolution, manifest) {
  list(source = paste0("s3://", .ucla_wrf_bucket, "/", unique(dirname(files$key))),
    catalog = "not applicable: direct UCLA reanalysis files", model = "ERA5", scenario = "reanalysis",
    member = "one source realization", variable = .wrf_cf_map$wrf_name, resolution = resolution,
    calendar = "standard", time_units = "seconds since 1970-01-01 00:00:00",
    times = times, time_values = as.numeric(times),
    requested_start = as.character(files$time[2]), requested_end = as.character(utils::tail(files$time, 1)),
    history = c("Preserved internal WRF Times; nominal filename times are source_nominal_time",
      "Derived precipitation from accumulated counters using preceding records and source-run boundary records",
      "Converted precipitation increments using actual source interval seconds; mixing ratio to specific humidity; wind speed from u10/v10"))
}

.stream_reanalysis_year <- function(sites, files, paths, cache_dir, resolution, year,
                                   batch_size, retain_source, manifest, identity, verbose) {
  temporary <- vapply(paths, function(path) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    tempfile(tmpdir = dirname(path), fileext = ".nc")
  }, character(1))
  on.exit(unlink(temporary), add = TRUE)
  nominal <- files$time[-1]
  p <- .reanalysis_provenance(files, nominal, resolution, manifest)
  empty <- stats::setNames(rep(list(rep(NA_real_, length(nominal))), 9L), c(.wrf_cf_map$cf_name, "wind_speed"))
  for (i in seq_along(temporary)) {
    .write_site_cf_nc(empty, nominal, sites$lat[i], sites$lon[i], temporary[i], year, p)
    nc <- ncdf4::nc_open(temporary[i], write = TRUE)
    tryCatch({
      nc <- ncdf4::ncvar_add(nc, ncdf4::ncvar_def("source_nominal_time", p$time_units, list(nc$dim$time), prec = "double"))
      ncdf4::ncvar_put(nc, "source_nominal_time", as.numeric(nominal))
    }, finally = ncdf4::nc_close(nc))
  }
  previous <- rep(NA_real_, nrow(sites))
  cells <- NULL
  actual <- as.POSIXct(rep(NA_real_, nrow(files)), origin = "1970-01-01", tz = "UTC")
  precipitation_end <- actual
  for (j in seq_len(nrow(files))) {
    process <- function(path, boundary_path = NULL) {
      if (is.null(cells)) {
        reference <- .read_reanalysis_hour(path, sites[1, , drop = FALSE], NULL, resolution, files$time[j], TRUE)
        cells <<- cae_grid_cells(sites, reference$grid)
      }
      ids <- unique(cells$cell_id)
      batches <- split(ids, ceiling(seq_along(ids) / batch_size))
      for (batch in batches) {
        k <- match(batch, cells$cell_id)
        hour <- .read_reanalysis_hour(path, sites[k, , drop = FALSE], if (is.null(cells)) NULL else cells[k, , drop = FALSE], resolution, files$time[j])
        if (is.na(actual[j])) actual[j] <<- hour$time
        if (!identical(as.numeric(actual[j]), as.numeric(hour$time))) stop("Source timestamps differ between cell batches")
        boundary_hour <- if (is.null(boundary_path)) hour else .read_reanalysis_hour(boundary_path,
          sites[k, , drop = FALSE], cells[k, , drop = FALSE], resolution, files$time[j], TRUE)
        boundary <- boundary_hour$total
        precipitation_end[j] <<- boundary_hour$time
        if (j > 1L) {
          dt <- as.numeric(difftime(precipitation_end[j], actual[j - 1L], units = "secs"))
          if (!is.finite(dt) || dt <= 0) stop("Source internal timestamps are not strictly increasing")
          raw <- hour$values
          raw$prec <- .reanalysis_precip_increment(hour$total, previous[k], boundary)
          converted <- .convert_driver_values(raw, dt)
          owners <- which(cells$cell_id %in% batch)
          for (owner in owners) {
            q <- match(cells$cell_id[owner], cells$cell_id[k])
            nc <- ncdf4::nc_open(temporary[owner], write = TRUE)
            tryCatch({
              for (v in names(converted)) ncdf4::ncvar_put(nc, v, converted[[v]][q], start = c(1, 1, j - 1L), count = c(1, 1, 1))
              ncdf4::ncvar_put(nc, "time", as.numeric(actual[j]), start = j - 1L, count = 1L)
            }, finally = ncdf4::nc_close(nc))
          }
        }
        previous[k] <<- hour$total
      }
    }
    .with_reanalysis_file(files$key[j], cache_dir, retain_source, function(path) {
      if (files$precipitation_key[j] == files$key[j]) process(path) else {
        .with_reanalysis_file(files$precipitation_key[j], cache_dir, retain_source, function(old) process(path, old))
      }
    })
    if (verbose && j %% 168L == 0L) message(year, ": ", j, " / ", nrow(files), " source hours")
  }
  p <- .reanalysis_provenance(files, actual[-1], resolution, manifest)
  for (i in seq_along(temporary)) {
    nc <- ncdf4::nc_open(temporary[i], write = TRUE)
    tryCatch({
      nv <- ncdf4::ncdim_def("bounds", "", 1:2, create_dimvar = FALSE)
      nc <- ncdf4::ncvar_add(nc, ncdf4::ncvar_def("time_bounds", p$time_units, list(nv, nc$dim$time), prec = "double"))
      ncdf4::ncvar_put(nc, "time_bounds", rbind(as.numeric(utils::head(actual, -1)), as.numeric(precipitation_end[-1])))
      nc <- ncdf4::ncvar_add(nc, ncdf4::ncvar_def("precipitation_time", p$time_units, list(nc$dim$time), prec = "double"))
      ncdf4::ncvar_put(nc, "precipitation_time", as.numeric(precipitation_end[-1]))
      ncdf4::ncatt_put(nc, "precipitation_time", "standard_name", "time")
      ncdf4::ncatt_put(nc, "precipitation_time", "bounds", "time_bounds")
      ncdf4::ncatt_put(nc, "precipitation_flux", "coordinates", "precipitation_time")
      .write_provenance(nc, p, actual[-1], "Streamed CF export without calendar modification")
      ncdf4::ncatt_put(nc, 0, "source_manifest", file.path("source-manifests", basename(manifest$path)))
      ncdf4::ncatt_put(nc, 0, "source_manifest_md5", manifest$checksum)
      ncdf4::ncatt_put(nc, 0, "cae_request", identity)
    }, finally = ncdf4::nc_close(nc))
    if (!.valid_reanalysis_file(temporary[i], nominal, sites$lat[i], sites$lon[i], identity)) stop("Reanalysis export validation failed")
    if (!file.rename(temporary[i], paths[i])) stop("Could not publish reanalysis file")
  }
  invisible(paths)
}

.valid_reanalysis_file <- function(path, nominal, lat, lon, identity) {
  if (!file.exists(path)) return(FALSE)
  tryCatch({
    nc <- ncdf4::nc_open(path)
    on.exit(ncdf4::nc_close(nc))
    meta <- .store_metadata(path, "air_temperature")
    actual <- as.numeric(meta$times)
    nominal <- as.numeric(nominal)
    n <- length(nominal)
    if (!n || length(actual) != n ||
        !all(c("source_nominal_time", "precipitation_time", "time_bounds") %in% names(nc$var))) return(FALSE)
    # Auxiliary coordinates use the same epoch seconds as the primary time axis.
    units <- "seconds since 1970-01-01 00:00:00"
    if (meta$time_units != units || meta$calendar != "standard" ||
        !all(vapply(c("source_nominal_time", "precipitation_time", "time_bounds"),
          function(v) identical(ncdf4::ncatt_get(nc, v, "units")$value, units), logical(1)))) return(FALSE)
    bounds <- ncdf4::ncvar_get(nc, "time_bounds", collapse_degen = FALSE)
    endpoint <- as.numeric(ncdf4::ncvar_get(nc, "precipitation_time"))
    if (!identical(dim(bounds), c(2L, as.integer(n))) ||
        length(endpoint) != n || any(!is.finite(c(actual, bounds, endpoint)))) return(FALSE)
    # Keep source seconds offsets, but require each timestamp to belong to its file hour.
    identical(as.numeric(ncdf4::ncvar_get(nc, "source_nominal_time")), nominal) &&
      all(floor(actual / 3600) == floor(nominal / 3600)) &&
      all(floor(endpoint / 3600) == floor(nominal / 3600)) &&
      floor(bounds[1, 1] / 3600) == floor(nominal[1] / 3600) - 1 &&
      identical(as.numeric(bounds[1, -1]), actual[-n]) &&
      identical(as.numeric(bounds[2, ]), endpoint) &&
      all(bounds[2, ] > bounds[1, ]) &&
      .valid_driver_file(path, meta$times, lat, lon, identity)
  }, error = function(e) FALSE)
}

# A boundary's previous-run final total and new-run baseline are distinct.
.reanalysis_precip_increment <- function(current, previous, boundary_total = current) {
  delta <- boundary_total - previous
  ifelse(delta < -0.01, boundary_total, pmax(delta, 0))
}

.read_reanalysis_hour <- function(path, sites, cells, resolution, expected, precipitation_only = FALSE) {
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc))
  grid <- .raw_wrf_grid(nc)
  dx <- ncdf4::ncatt_get(nc, 0, "DX")$value
  if (abs(dx - c(d01 = 45000, d02 = 9000, d03 = 3000)[[resolution]]) > 1) stop("Source resolution mismatch")
  mapped <- cae_grid_cells(sites, grid)
  if (!is.null(cells) && !identical(mapped[c("cell_i", "cell_j")], cells[c("cell_i", "cell_j")])) stop("Source grid changed")
  cells <- mapped
  time <- .parse_time(gsub("_", " ", as.character(ncdf4::ncvar_get(nc, "Times")))[1])
  if (is.na(time) || format(time, "%Y-%m-%d %H", tz = "UTC") != format(expected, "%Y-%m-%d %H", tz = "UTC")) stop("Source timestamp does not belong to nominal file hour")
  read <- function(name) {
    field <- ncdf4::ncvar_get(nc, name)
    as.numeric(field[cbind(cells$cell_i, cells$cell_j)])
  }
  variables <- if (precipitation_only) character() else setdiff(.wrf_cf_map$wrf_name, "prec")
  values <- lapply(variables, function(v) read(toupper(v))) |> stats::setNames(variables)
  list(grid = grid, cells = cells, values = values, total = read("RAINC") + read("RAINNC"), time = time)
}
