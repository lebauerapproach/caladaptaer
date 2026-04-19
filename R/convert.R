#' Unit Conversion and CF Standard Formatting
#'
#' Functions to convert Cal-Adapt WRF variables to CF-standard
#' NetCDF format. Handles variable renaming, unit conversions, and
#' metadata stamping.

# WRF-to-CF variable mapping
# source: WRF output conventions + CF met standard
.wrf_cf_map <- data.frame(
  wrf_name = c("t2", "psfc", "prec", "q2", "swdnb", "lwdnb", "u10", "v10"),
  cf_name = c("air_temperature", "air_pressure", "precipitation_flux",
              "specific_humidity",
              "surface_downwelling_shortwave_flux_in_air",
              "surface_downwelling_longwave_flux_in_air",
              "eastward_wind", "northward_wind"),
  cf_units = c("K", "Pa", "kg m-2 s-1", "1", "W m-2", "W m-2", "m s-1",
               "m s-1"),
  wrf_units = c("K", "Pa", "mm", "kg kg-1", "W m-2", "W m-2", "m s-1",
                "m s-1"),
  stringsAsFactors = FALSE
)


#' Convert WRF precipitation from accumulation to flux
#'
#' WRF reports precipitation as accumulated mm per timestep.
#' CF standard wants kg/m2/s (equivalent to mm/s).
#'
#' @param precip numeric vector; accumulated precipitation in mm
#' @param dt_seconds numeric; timestep in seconds (e.g. 3600 for hourly)
#' @return numeric vector; precipitation flux in kg/m2/s
#' @keywords internal
.precip_to_flux <- function(precip, dt_seconds) {
  precip / dt_seconds
}


#' Convert WRF mixing ratio to specific humidity
#'
#' WRF q2 is water vapor mixing ratio in kg/kg.
#' Specific humidity q = w / (1 + w) where w is mixing ratio.
#' For small w this is approximately equal, but we do it right.
#'
#' @param q2 numeric vector; mixing ratio in kg/kg
#' @return numeric vector; specific humidity (dimensionless)
#' @keywords internal
.mixing_ratio_to_specific_humidity <- function(q2) {
  q2 / (1.0 + q2)
}


#' Compute wind speed from components
#'
#' @param u10 numeric vector; eastward wind in m/s
#' @param v10 numeric vector; northward wind in m/s
#' @return numeric vector; wind speed in m/s
#' @keywords internal
.compute_wind_speed <- function(u10, v10) {
  sqrt(u10^2 + v10^2)
}


#' Convert a stars object to CF-standard NetCDF
#'
#' Takes a list of stars objects (one per WRF variable), applies
#' unit conversions, renames to CF standard names, and writes a
#' CF-compliant NetCDF file.
#'
#' @param var_list named list of stars objects. Names should be WRF
#'   variable names (t2, prec, psfc, q2, swdnb, u10, v10).
#' @param outfile character; path for the output NetCDF file
#' @param dt_seconds numeric; timestep duration in seconds.
#'   Default 3600 (hourly).
#' @param overwrite logical; overwrite existing file? Default FALSE.
#' @return character; path to the written NetCDF file (invisibly)
#' @export
ca_write_cf_netcdf <- function(var_list, outfile, dt_seconds = 3600,
                               overwrite = FALSE) {

  if (file.exists(outfile) && !overwrite) {
    stop("Output file already exists: ", outfile,
         "\nSet overwrite=TRUE to replace.", call. = FALSE)
  }

  required <- c("t2", "psfc", "prec", "q2", "swdnb", "lwdnb", "u10", "v10")
  missing <- setdiff(required, names(var_list))
  if (length(missing) > 0) {
    stop("Missing required variables: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  # get time info from the first variable
  time_vals <- .get_time_values(var_list[[1]])
  n_time <- length(time_vals)

  # get spatial info -- extract point values if single-point
  # (for gridded output this writes the full grid)
  first_var <- var_list[[1]]
  dims <- stars::st_dimensions(first_var)

  # apply unit conversions and extract raw arrays
  converted <- list()

  # temperature -- already in K, no conversion needed
  converted$air_temperature <- units::drop_units(var_list$t2[[1]])

  # pressure -- already in Pa
  converted$air_pressure <- units::drop_units(var_list$psfc[[1]])

  # precipitation -- mm/timestep to kg/m2/s
  raw_prec <- units::drop_units(var_list$prec[[1]])
  converted$precipitation_flux <- .precip_to_flux(raw_prec, dt_seconds)

  # humidity -- mixing ratio to specific humidity
  raw_q2 <- units::drop_units(var_list$q2[[1]])
  converted$specific_humidity <- .mixing_ratio_to_specific_humidity(raw_q2)

  # shortwave -- already in W/m2
  converted$surface_downwelling_shortwave_flux_in_air <-
    units::drop_units(var_list$swdnb[[1]])

  # longwave -- already in W/m2
  converted$surface_downwelling_longwave_flux_in_air <-
    units::drop_units(var_list$lwdnb[[1]])

  # winds -- already in m/s
  raw_u10 <- units::drop_units(var_list$u10[[1]])
  raw_v10 <- units::drop_units(var_list$v10[[1]])
  converted$eastward_wind <- raw_u10
  converted$northward_wind <- raw_v10
  converted$wind_speed <- .compute_wind_speed(raw_u10, raw_v10)

  # write NetCDF
  .write_cf_nc(converted, time_vals, dims, outfile)

  invisible(outfile)
}


#' Write CF-compliant NetCDF from converted data
#' @keywords internal
.write_cf_nc <- function(converted, time_vals, dims, outfile) {

  # define time dimension
  time_origin <- as.POSIXct("1970-01-01", tz = "UTC")
  time_secs <- as.numeric(difftime(time_vals, time_origin, units = "secs"))
  time_dim <- ncdf4::ncdim_def("time", "seconds since 1970-01-01 00:00:00",
                                time_secs, unlim = TRUE)

  # figure out spatial dims from the first variable
  arr <- converted[[1]]
  ndim <- length(dim(arr))

  if (ndim == 1) {
    # point time series -- 1D
    nc_vars <- lapply(names(converted), function(vname) {
      cf_row <- .wrf_cf_map[.wrf_cf_map$cf_name == vname, ]
      u <- if (nrow(cf_row) > 0) cf_row$cf_units else ""
      # wind_speed isn't in wrf_cf_map
      if (vname == "wind_speed") u <- "m s-1"
      ncdf4::ncvar_def(vname, u, list(time_dim), missval = -9999)
    })
    names(nc_vars) <- names(converted)

    nc <- ncdf4::nc_create(outfile, nc_vars)
    on.exit(ncdf4::nc_close(nc), add = TRUE)

    for (vname in names(converted)) {
      ncdf4::ncvar_put(nc, vname, converted[[vname]])
    }

  } else {
    # gridded output -- 3D (x, y, time)
    x_vals <- seq(dims$x$offset, by = dims$x$delta,
                  length.out = dims$x$to)
    y_vals <- seq(dims$y$offset, by = dims$y$delta,
                  length.out = dims$y$to)
    x_dim <- ncdf4::ncdim_def("x", "m", x_vals)
    y_dim <- ncdf4::ncdim_def("y", "m", y_vals)

    nc_vars <- lapply(names(converted), function(vname) {
      cf_row <- .wrf_cf_map[.wrf_cf_map$cf_name == vname, ]
      u <- if (nrow(cf_row) > 0) cf_row$cf_units else ""
      if (vname == "wind_speed") u <- "m s-1"
      ncdf4::ncvar_def(vname, u, list(x_dim, y_dim, time_dim),
                        missval = -9999)
    })
    names(nc_vars) <- names(converted)

    nc <- ncdf4::nc_create(outfile, nc_vars)
    on.exit(ncdf4::nc_close(nc), add = TRUE)

    for (vname in names(converted)) {
      ncdf4::ncvar_put(nc, vname, converted[[vname]])
    }
  }

  # global attributes
  ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.8")
  ncdf4::ncatt_put(nc, 0, "source", "Cal-Adapt Analytics Engine (CADCAT)")
  ncdf4::ncatt_put(nc, 0, "created_by", "caladaptR")
  ncdf4::ncatt_put(nc, 0, "history",
                    paste("Created", Sys.time(), "by caladaptR"))
}
