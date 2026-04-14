#' Fetch Cal-Adapt Climate Data
#'
#' Read climate data from the Cal-Adapt Analytics Engine Zarr stores
#' on AWS S3. Returns a stars object with the requested variable,
#' optionally clipped to a spatial extent and/or time window.

#' @param variable character; variable name (e.g. "t2", "prec", "psfc").
#'   Use ca_variables() to see what's available.
#' @param model character; GCM name (e.g. "CESM2", "MPI-ESM1-2-HR").
#'   Use ca_models() to see options.
#' @param scenario character; SSP experiment (e.g. "ssp370", "ssp245",
#'   "historical"). Use ca_scenarios() to see options.
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
#' t2 <- ca_fetch("t2", model = "CESM2", scenario = "ssp370",
#'                n_timesteps = 24)
#'
#' # Read a month of daily precipitation from LOCA2
#' pr <- ca_fetch("pr", model = "EC-Earth3", scenario = "ssp370",
#'                timescale = "day", resolution = "d03",
#'                start_time = "2050-01-01", end_time = "2050-01-31")
#' }
ca_fetch <- function(variable, model, scenario,
                     timescale = "1hr", resolution = "d01",
                     start_time = NULL, end_time = NULL,
                     n_timesteps = NULL) {

  # figure out activity from catalog -- WRF or LOCA2
  activity <- .detect_activity(model, timescale, resolution, variable)

  # handle derived precipitation for models that store components
  # separately (MPI-ESM1-2-HR, MIROC6, TaiESM1 have rainc/rainnc
  # instead of prec)
  if (variable == "prec" && activity == "WRF") {
    has_prec <- nrow(ca_search(activity = "WRF", model = model,
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
#' @keywords internal
.fetch_single <- function(variable, activity, model, scenario,
                          timescale, resolution, start_time, end_time,
                          n_timesteps) {

  s3_path <- ca_zarr_path(activity = activity, model = model,
                          scenario = scenario, variable = variable,
                          timescale = timescale, resolution = resolution)

  vsi_path <- .s3_to_vsi(s3_path)
  .setup_s3_env()
  count <- .build_count(n_timesteps)

  data <- tryCatch(
    stars::read_mdim(vsi_path, count = count),
    error = function(e) {
      stop("Failed to read Zarr store at ", s3_path, "\n",
           e$message, call. = FALSE)
    }
  )

  if (!is.null(start_time) || !is.null(end_time)) {
    data <- .subset_time(data, start_time, end_time)
  }

  data
}


#' Derive total precipitation from WRF components
#'
#' Some WRF GCMs store convective and non-convective rain separately
#' (rainc, rainnc) instead of a pre-summed prec field. This reads
#' both components and sums them.
#'
#' @keywords internal
.fetch_derived_precip <- function(model, scenario, timescale,
                                  resolution, start_time, end_time,
                                  n_timesteps) {
  message("Model ", model, " does not have 'prec'; ",
          "deriving from rainc + rainnc")

  rainc <- .fetch_single("rainc", "WRF", model, scenario,
                         timescale, resolution, start_time,
                         end_time, n_timesteps)
  rainnc <- .fetch_single("rainnc", "WRF", model, scenario,
                          timescale, resolution, start_time,
                          end_time, n_timesteps)

  # sum the components
  result <- rainc
  result[[1]] <- rainc[[1]] + rainnc[[1]]
  result
}


#' Extract time series at a point
#'
#' Fetch data and extract a time series at a specific lon/lat location.
#' Handles coordinate transformation from WGS84 to the WRF Lambert
#' Conformal grid automatically.
#'
#' @inheritParams ca_fetch
#' @param lon numeric; longitude in decimal degrees (WGS84)
#' @param lat numeric; latitude in decimal degrees (WGS84)
#' @return A data.frame with columns: time, value (in native units)
#' @export
#' @examples
#' \dontrun{
#' # Get hourly temperature at Fresno for one day
#' ts <- ca_fetch_point("t2", model = "CESM2", scenario = "ssp370",
#'                      lon = -119.77, lat = 36.75, n_timesteps = 24)
#' plot(ts$time, ts$value, type = "l")
#' }
ca_fetch_point <- function(variable, model, scenario,
                           lon, lat,
                           timescale = "1hr", resolution = "d01",
                           start_time = NULL, end_time = NULL,
                           n_timesteps = NULL) {

  data <- ca_fetch(variable = variable, model = model,
                   scenario = scenario, timescale = timescale,
                   resolution = resolution, start_time = start_time,
                   end_time = end_time, n_timesteps = n_timesteps)

  # build point in WGS84 and transform to data CRS
  pt_wgs84 <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)
  pt_native <- sf::st_transform(pt_wgs84, sf::st_crs(data))

  # extract
  extracted <- stars::st_extract(data, pt_native)

  # reshape to data.frame
  time_vals <- stars::st_get_dimension_values(data, "time")
  values <- as.vector(units::drop_units(extracted[[1]]))

  data.frame(
    time = time_vals,
    value = values,
    stringsAsFactors = FALSE
  )
}


# -- internal helpers --

#' Detect activity_id from model name and params
#' @keywords internal
.detect_activity <- function(model, timescale, resolution,
                             variable = NULL) {
  cat <- ca_catalog()

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


#' Build the count argument for read_mdim
#' @keywords internal
.build_count <- function(n_timesteps) {
  if (!is.null(n_timesteps)) {
    # NA means "read all" for spatial dims, n for time
    return(c(NA, NA, n_timesteps))
  }
  # default: read everything (may be huge -- user should set limits)
  NULL
}


#' Subset a stars object by time
#' @keywords internal
.subset_time <- function(data, start_time, end_time) {
  time_vals <- stars::st_get_dimension_values(data, "time")

  if (!is.null(start_time)) {
    start_time <- as.POSIXct(start_time, tz = "UTC")
    keep <- time_vals >= start_time
  } else {
    keep <- rep(TRUE, length(time_vals))
  }

  if (!is.null(end_time)) {
    end_time <- as.POSIXct(end_time, tz = "UTC")
    keep <- keep & time_vals <= end_time
  }

  idx <- which(keep)
  if (length(idx) == 0) {
    stop("No timesteps fall within the requested time window.",
         call. = FALSE)
  }

  data[, , , idx]
}
