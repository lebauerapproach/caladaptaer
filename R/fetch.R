#' Fetch Cal-Adapt Climate Data
#'
#' Read climate data from the Cal-Adapt Analytics Engine Zarr stores
#' on AWS S3. Returns a stars object with the requested variable,
#' optionally clipped to a spatial extent and/or time window.

#' @param variable character; variable name (e.g. "t2", "prec", "psfc").
#'   Use cae_variables() to see what's available.
#' @param model character; GCM name (e.g. "CESM2", "MPI-ESM1-2-HR").
#'   Use cae_models() to see options.
#' @param scenario character; SSP experiment (e.g. "ssp370", "ssp245",
#'   "historical"). Use cae_scenarios() to see options.
#' @param timescale character; temporal resolution. One of "1hr", "day", "mon".
#'   Default "1hr".
#' @param resolution character; spatial grid. One of "d01" (45km),
#'   "d02" (9km), "d03" (3km). Default "d01".
#' @param start_time POSIXct or character; start of time window.
#'   If character, parsed as UTC. Default NULL reads from the beginning.
#' @param end_time POSIXct or character; end of time window.
#'   Default NULL reads to the end.
#' @param n_timesteps integer; number of timesteps to read.
#'   Alternative to end_time for quick reads. Default NULL.
#' @return A stars object with the requested data.
#' @export
#' @examples
#' \dontrun{
#' # Read 24 hours of temperature data at 45km
#' t2 <- cae_fetch("t2", model = "CESM2", scenario = "ssp370",
#'                n_timesteps = 24)
#'
#' # Read a month of daily precipitation from LOCA2
#' pr <- cae_fetch("pr", model = "EC-Earth3", scenario = "ssp370",
#'                timescale = "day", resolution = "d03",
#'                start_time = "2050-01-01", end_time = "2050-01-31")
#' }
cae_fetch <- function(variable, model, scenario,
                     timescale = "1hr", resolution = "d01",
                     start_time = NULL, end_time = NULL,
                     n_timesteps = NULL) {

  # figure out activity from catalog -- WRF or LOCA2
  activity <- .detect_activity(model, timescale, resolution, variable)

  # handle derived precipitation for models that store components
  # separately (MPI-ESM1-2-HR, MIROC6, TaiESM1 have rainc/rainnc
  # instead of prec)
  if (variable == "prec" && activity == "WRF") {
    has_prec <- nrow(cae_search(activity = "WRF", model = model,
                               scenario = scenario, variable = "prec",
                               timescale = timescale,
                               resolution = resolution)) > 0
    if (!has_prec) {
      return(.fetch_derived_precip(model, scenario, timescale,
                                   resolution, start_time, end_time,
                                   n_timesteps))
    }
  }

  .fetch_single(variable, activity, model, scenario, timescale,
                resolution, start_time, end_time, n_timesteps)
}


#' Read a single variable from S3
#'
#' Computes array offset/count for the requested time window, reads via
#' GDAL's multidimensional API, then decodes the time dimension to
#' POSIXct. Time decoding is done by reading the Zarr time/.zattrs
#' encoding directly because stars + CFtime has a subsetting bug where
#' CFtime's [ operator returns integer indices instead of timestamps
#' (stars 0.7.x, CFtime 1.7.x).
#'
#' @keywords internal
.fetch_single <- function(variable, activity, model, scenario,
                          timescale, resolution, start_time, end_time,
                          n_timesteps) {

  s3_path <- cae_zarr_path(activity = activity, model = model,
                          scenario = scenario, variable = variable,
                          timescale = timescale, resolution = resolution)

  vsi_path <- .s3_to_vsi(s3_path)
  .setup_s3_env()

  # read time encoding from the zarr store metadata
  time_enc <- .read_zarr_time_encoding(s3_path)

  # compute offset and count for the time dimension
  time_offset <- 0L
  time_count  <- NA  # NA = read all

  if (!is.null(n_timesteps)) {
    time_count <- as.integer(n_timesteps)
  }

  if (!is.null(time_enc) && (!is.null(start_time) || !is.null(end_time))) {
    rng <- .time_index_range(time_enc, start_time, end_time, timescale)
    time_offset <- rng$offset
    time_count  <- rng$count
  }

  data <- tryCatch(
    stars::read_mdim(vsi_path,
                     offset = c(0, 0, time_offset),
                     count = c(NA, NA, time_count)),
    error = function(e) {
      stop("Failed to read Zarr store at ", s3_path, "\n",
           e$message, call. = FALSE)
    }
  )

  # decode time to POSIXct using the zarr encoding we read from .zattrs
  if (!is.null(time_enc)) {
    data <- .decode_time_dimension(data, time_enc, time_offset, timescale)
  }

  data
}


#' Derive total precipitation from WRF components
#'
#' Some WRF GCMs store convective and non-convective rain separately
#' as accumulated totals (rainc, rainnc) instead of a pre-summed
#' per-timestep prec field. This reads both components, sums them,
#' and deaccumulates to get per-timestep precipitation in mm.
#'
#' WRF accumulation counters reset periodically (roughly annually).
#' Resets are detected as large negative diffs and handled by using
#' the post-reset value as the timestep increment.
#'
#' @keywords internal
.fetch_derived_precip <- function(model, scenario, timescale,
                                  resolution, start_time, end_time,
                                  n_timesteps) {
  message("Model ", model, " does not have 'prec'; ",
          "deriving from rainc + rainnc (deaccumulating)")

  rainc <- .fetch_single("rainc", "WRF", model, scenario,
                         timescale, resolution, start_time,
                         end_time, n_timesteps)
  rainnc <- .fetch_single("rainnc", "WRF", model, scenario,
                          timescale, resolution, start_time,
                          end_time, n_timesteps)

  # sum the accumulated components
  result <- rainc
  result[[1]] <- rainc[[1]] + rainnc[[1]]

  # deaccumulate: convert running total to per-timestep mm
  result <- .deaccumulate_precip(result)

  result
}


#' Deaccumulate a stars precipitation object
#'
#' Converts accumulated precipitation (running total since WRF
#' simulation start) to per-timestep increments by differencing
#' along the time axis. Handles accumulation resets (where the
#' counter drops back to ~0) and float precision artifacts.
#'
#' @param data stars object with accumulated precipitation
#' @return stars object with per-timestep precipitation (same units)
#' @keywords internal
.deaccumulate_precip <- function(data) {
  arr <- data[[1]]

  # preserve units if present
  has_units <- inherits(arr, "units")
  if (has_units) {
    u_str <- units::deparse_unit(arr)
    arr <- units::drop_units(arr)
  }

  d <- dim(arr)
  nd <- if (is.null(d)) 1L else length(d)

  if (nd == 3) {
    # grid data: [x, y, time]
    nt <- d[3]
    if (nt > 1) {
      delta <- arr[,,2:nt] - arr[,,1:(nt-1)]
      # resets: large negative diff means counter was reset to ~0
      # and re-accumulated; the post-reset value is the increment
      reset_mask <- delta < -0.01
      if (any(reset_mask)) {
        delta[reset_mask] <- arr[,,2:nt][reset_mask]
      }
      # clamp remaining float-precision negatives
      delta[delta < 0] <- 0
      arr[,,1] <- 0
      arr[,,2:nt] <- delta
    } else {
      arr[] <- 0
    }
  } else {
    # 1D vector (single-point extraction or flat array)
    v <- as.vector(arr)
    nt <- length(v)
    if (nt > 1) {
      delta <- diff(v)
      reset_mask <- delta < -0.01
      if (any(reset_mask)) {
        delta[reset_mask] <- v[-1][reset_mask]
      }
      delta[delta < 0] <- 0
      arr <- c(0, delta)
    } else {
      arr <- 0
    }
  }

  if (has_units) {
    arr <- units::set_units(arr, u_str, mode = "standard")
  }
  data[[1]] <- arr
  data
}


#' Extract time series at a point
#'
#' Fetch data and extract a time series at a specific lon/lat location.
#' Handles coordinate transformation from WGS84 to the WRF Lambert
#' Conformal grid automatically.
#'
#' @inheritParams cae_fetch
#' @param lon numeric; longitude in decimal degrees (WGS84)
#' @param lat numeric; latitude in decimal degrees (WGS84)
#' @return A data.frame with columns: time, value (in native units)
#' @export
#' @examples
#' \dontrun{
#' # Get hourly temperature at Fresno for one day
#' ts <- cae_fetch_point("t2", model = "CESM2", scenario = "ssp370",
#'                      lon = -119.77, lat = 36.75, n_timesteps = 24)
#' plot(ts$time, ts$value, type = "l")
#' }
cae_fetch_point <- function(variable, model, scenario,
                           lon, lat,
                           timescale = "1hr", resolution = "d01",
                           start_time = NULL, end_time = NULL,
                           n_timesteps = NULL) {

  data <- cae_fetch(variable = variable, model = model,
                   scenario = scenario, timescale = timescale,
                   resolution = resolution, start_time = start_time,
                   end_time = end_time, n_timesteps = n_timesteps)

  # build point in WGS84 and transform to data CRS
  pt_wgs84 <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)
  pt_native <- sf::st_transform(pt_wgs84, sf::st_crs(data))

  # extract
  extracted <- stars::st_extract(data, pt_native)

  # reshape to data.frame
  time_vals <- .get_time_values(data)
  values <- as.numeric(extracted[[1]])

  data.frame(
    time = time_vals,
    value = values,
    stringsAsFactors = FALSE
  )
}


#' Extract time series at multiple points (bulk)
#'
#' Fetch data once and extract time series at many lon/lat locations.
#' The grid is read from S3 only once regardless of site count, making
#' this dramatically faster than looping over \code{cae_fetch_point()}.
#'
#' @inheritParams cae_fetch
#' @param points data.frame with columns \code{lon} and \code{lat} in
#'   WGS84 decimal degrees. An optional \code{site_id} column provides
#'   labels; otherwise sites are numbered 1..N.
#' @return A data.frame with columns: site_id, time, value (in native
#'   units). One row per site per timestep.
#' @export
#' @examples
#' \dontrun{
#' # 5 sites across California
#' sites <- data.frame(
#'   site_id = c("fresno", "la", "sacramento", "redding", "bakersfield"),
#'   lon = c(-119.77, -118.24, -121.49, -122.39, -119.02),
#'   lat = c(36.75, 34.05, 38.58, 40.59, 35.37)
#' )
#'
#' ts <- cae_fetch_points("t2", model = "CESM2", scenario = "ssp370",
#'                       points = sites, n_timesteps = 24)
#' head(ts)
#'
#' # works for thousands of sites -- grid is read once
#' big <- data.frame(lon = runif(10000, -124, -114),
#'                   lat = runif(10000, 32, 42))
#' ts_big <- cae_fetch_points("t2", model = "CESM2", scenario = "ssp370",
#'                           points = big, n_timesteps = 24)
#' }
cae_fetch_points <- function(variable, model, scenario,
                            points,
                            timescale = "1hr", resolution = "d01",
                            start_time = NULL, end_time = NULL,
                            n_timesteps = NULL) {

  if (!is.data.frame(points) || !all(c("lon", "lat") %in% names(points))) {
    stop("'points' must be a data.frame with 'lon' and 'lat' columns",
         call. = FALSE)
  }

  n_sites <- nrow(points)
  if (n_sites == 0) stop("'points' has zero rows", call. = FALSE)

  site_ids <- if ("site_id" %in% names(points)) {
    points$site_id
  } else {
    seq_len(n_sites)
  }

  # read the grid ONCE from S3
  message("Reading grid (1 S3 fetch for all ", n_sites, " sites)")
  grid <- cae_fetch(variable = variable, model = model,
                   scenario = scenario, timescale = timescale,
                   resolution = resolution, start_time = start_time,
                   end_time = end_time, n_timesteps = n_timesteps)

  # build sf points in WGS84, transform to grid CRS
  pts_sf <- sf::st_as_sf(points, coords = c("lon", "lat"), crs = 4326)
  pts_native <- sf::st_transform(pts_sf, sf::st_crs(grid))

  # extract ALL sites in one vectorized call
  message("Extracting ", n_sites, " sites")
  extracted <- stars::st_extract(grid, pts_native)

  # reshape to tidy data.frame
  time_vals <- .get_time_values(grid)
  n_times <- length(time_vals)

  vals <- extracted[[1]]
  if (inherits(vals, "units")) vals <- units::drop_units(vals)
  vals <- as.numeric(vals)

  if (length(vals) != n_sites * n_times) {
    stop("Unexpected extraction result: expected ", n_sites * n_times,
         " values but got ", length(vals), call. = FALSE)
  }

  # st_extract returns [n_sites x n_times] matrix
  # as.vector goes column-major; we want site-major output
  mat <- matrix(vals, nrow = n_sites, ncol = n_times)

  data.frame(
    site_id = rep(site_ids, each = n_times),
    time = rep(time_vals, times = n_sites),
    value = as.vector(t(mat)),
    stringsAsFactors = FALSE
  )
}


# -- internal helpers --

#' Detect activity_id from model name and params
#' @keywords internal
.detect_activity <- function(model, timescale, resolution,
                             variable = NULL) {
  cat <- cae_catalog()

  # filter by model, timescale, resolution
  base <- cat[cat$source_id == model &
              cat$table_id == timescale &
              cat$grid_label == resolution, ]

  # if variable is provided, use it to disambiguate
  if (!is.null(variable)) {
    base_var <- base[base$variable_id == variable, ]
    if (nrow(base_var) > 0) base <- base_var
  }

  activities <- unique(base$activity_id)
  if ("WRF" %in% activities) return("WRF")
  if ("LOCA2" %in% activities) return("LOCA2")

  stop("Model '", model, "' not found in catalog for timescale='",
       timescale, "', resolution='", resolution, "'",
       call. = FALSE)
}


#' Convert s3:// path to GDAL /vsis3/ path
#' @keywords internal
.s3_to_vsi <- function(s3_path) {
  sub("^s3://", "/vsis3/", s3_path)
}


#' Set environment variables for anonymous S3 access
#' @keywords internal
.setup_s3_env <- function() {
  Sys.setenv(AWS_NO_SIGN_REQUEST = "YES")
  if (Sys.getenv("AWS_DEFAULT_REGION") == "") {
    Sys.setenv(AWS_DEFAULT_REGION = "us-west-2")
  }
}


#' Get time values from a stars object
#'
#' Extracts POSIXct time values from the time dimension. Uses the
#' explicit values set by .decode_time_dimension() if available,
#' otherwise falls back to reconstructing from offset + delta.
#'
#' @param data stars object with time dimension
#' @return POSIXct vector of time values
#' @keywords internal
.get_time_values <- function(data) {
  dims <- stars::st_dimensions(data)
  td <- dims[["time"]]

  # .decode_time_dimension stores explicit POSIXct values
  if (!is.null(td$values) && inherits(td$values, "POSIXct")) return(td$values)

  # fallback for offset + delta representation
  n <- td$to - td$from + 1L
  if (inherits(td$offset, "POSIXct") && !is.null(td$delta)) {
    delta_secs <- as.numeric(td$delta, units = "secs")
    return(td$offset + seq(0, by = delta_secs, length.out = n))
  }

  # last resort
  stars::st_get_dimension_values(data, "time")
}


#' Parse time strings that may use ISO 8601 T separator
#' @keywords internal
.parse_time <- function(x) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "POSIXct")) return(x)
  as.POSIXct(sub("T", " ", x), tz = "UTC")
}


#' Read Zarr time encoding from the store metadata
#'
#' Fetches time/.zattrs from the CADCAT S3 bucket to get the CF time
#' units string (e.g. "seconds since 2014-09-01"). This is needed
#' because stars + CFtime has a subsetting bug where CFtime's [ operator
#' returns integer indices instead of timestamps (r-spatial/stars,
#' CFtime 1.7.x). We read the encoding ourselves and compute timestamps
#' from the array offset.
#'
#' @param s3_path character; the s3:// path to the zarr store
#' @return list with unit, origin (POSIXct), calendar, raw_units;
#'   or NULL if attributes cannot be read
#' @keywords internal
.read_zarr_time_encoding <- function(s3_path) {
  https_base <- sub("^s3://([^/]+)/",
                    "https://\\1.s3.us-west-2.amazonaws.com/", s3_path)
  if (!grepl("/$", https_base)) https_base <- paste0(https_base, "/")
  url <- paste0(https_base, "time/.zattrs")

  attrs <- tryCatch(
    jsonlite::fromJSON(url),
    error = function(e) {
      warning("Could not read Zarr time attributes from ", url,
              ": ", e$message, call. = FALSE)
      return(NULL)
    }
  )
  if (is.null(attrs) || is.null(attrs$units)) return(NULL)

  parts <- strsplit(attrs$units, " since ")[[1]]
  if (length(parts) != 2) return(NULL)

  list(
    unit     = parts[1],
    origin   = as.POSIXct(parts[2], tz = "UTC"),
    calendar = if (!is.null(attrs$calendar)) attrs$calendar else "standard",
    raw_units = attrs$units
  )
}


#' Compute array index offset and count for a time window
#'
#' Translates a start/end time range into 0-based array indices for
#' the read_mdim offset/count parameters.
#'
#' @param time_enc list from .read_zarr_time_encoding
#' @param start_time character or POSIXct; start of window (NULL = beginning)
#' @param end_time character or POSIXct; end of window (NULL = end)
#' @param timescale character; "1hr" or "day"
#' @return list with integer offset and count
#' @keywords internal
.time_index_range <- function(time_enc, start_time, end_time,
                              timescale = "1hr") {
  start_time <- .parse_time(start_time)
  end_time   <- .parse_time(end_time)

  step_secs <- switch(timescale,
    "1hr" = 3600L, "day" = 86400L,
    stop("Unsupported timescale for index range: ", timescale,
         call. = FALSE))

  offset <- 0L
  if (!is.null(start_time)) {
    secs <- as.numeric(difftime(start_time, time_enc$origin, units = "secs"))
    offset <- as.integer(round(secs / step_secs))
  }

  count <- NA
  if (!is.null(end_time)) {
    secs_end <- as.numeric(difftime(end_time, time_enc$origin, units = "secs"))
    end_idx <- as.integer(round(secs_end / step_secs))
    count <- end_idx - offset + 1L
  }

  list(offset = offset, count = count)
}


#' Decode time dimension to POSIXct
#'
#' stars + CFtime has a subsetting bug where CFtime's [ operator returns
#' integer indices instead of timestamps. This computes correct POSIXct
#' timestamps from the zarr time encoding and the array offset we used
#' for read_mdim.
#'
#' @param data stars object
#' @param time_enc list from .read_zarr_time_encoding
#' @param time_offset integer; 0-based array offset used in read_mdim
#' @param timescale character; "1hr" or "day"
#' @return stars object with correct POSIXct time dimension
#' @keywords internal
.decode_time_dimension <- function(data, time_enc, time_offset,
                                   timescale = "1hr") {
  dims <- stars::st_dimensions(data)
  time_dim <- dims[["time"]]

  n <- time_dim$to - time_dim$from + 1L
  step_secs <- switch(timescale, "1hr" = 3600L, "day" = 86400L, 3600L)

  array_indices <- seq(from = time_offset, length.out = n)
  timestamps <- time_enc$origin + array_indices * step_secs

  stars::st_set_dimensions(data, "time", values = timestamps)
}
