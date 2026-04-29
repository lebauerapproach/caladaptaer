#' Build CF-standard met driver files for multiple sites
#'
#' Fetches WRF data from S3 once per variable per year and extracts all
#' sites in a single vectorized call via [cae_fetch_points()], then writes
#' one CF-compliant NetCDF per site per year. For N sites, 8 variables,
#' and Y years this makes 8*Y S3 requests regardless of N.
#'
#' Output files follow the PEcAn CF Meteorology convention and can be
#' registered in BETY or used directly by ecosystem models that read
#' CF-standard met inputs.
#'
#' @param sites data.frame with columns `site_id`, `lon`, `lat` (WGS84
#'   decimal degrees). See Examples.
#' @param model character; WRF GCM name. Default `"CESM2"`.
#'   See `cae_models(activity = "WRF")` for options.
#' @param scenario character; SSP experiment. Default `"ssp370"`.
#'   See `cae_scenarios()` for options.
#' @param start_year integer; first year to process
#' @param end_year integer; last year to process
#' @param outdir character; root output directory. Files are written to
#'   `{outdir}/{site_id}/{model}.{scenario}.{year}.nc`.
#' @param resolution character; WRF grid label. Default `"d01"` (45 km).
#' @param overwrite logical; overwrite existing files? Default `FALSE`.
#' @param verbose logical; print progress messages? Default `TRUE`.
#' @return A data.frame with columns `site_id`, `year`, `file` (invisibly).
#' @export
#' @seealso [cae_fetch_points()] for the underlying multi-site extraction,
#'   [cae_write_cf_netcdf()] for the stars-based single-grid approach
#' @examples
#' \dontrun{
#' sites <- data.frame(
#'   site_id = c("fresno", "la", "sacramento"),
#'   lon = c(-119.77, -118.24, -121.49),
#'   lat = c(36.75, 34.05, 38.58)
#' )
#' result <- cae_build_met_drivers(
#'   sites, model = "CESM2", scenario = "ssp370",
#'   start_year = 2050, end_year = 2051,
#'   outdir = "met_output"
#' )
#' head(result)
#' }
cae_build_met_drivers <- function(sites, model = "CESM2", scenario = "ssp370",
                                 start_year, end_year,
                                 outdir,
                                 resolution = "d01",
                                 overwrite = FALSE, verbose = TRUE) {

  if (!is.data.frame(sites) ||
      !all(c("site_id", "lon", "lat") %in% names(sites))) {
    stop("'sites' must be a data.frame with 'site_id', 'lon', and 'lat' columns",
         call. = FALSE)
  }
  if (nrow(sites) == 0) stop("'sites' has zero rows", call. = FALSE)

  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

  # the 8 WRF variables we need (matches .wrf_cf_map in convert.R)
  wrf_vars <- .wrf_cf_map$wrf_name
  years <- seq(start_year, end_year)

  # collect results as a list, rbind at the end
  result_list <- vector("list", length(years) * nrow(sites))
  ri <- 0L

  for (year in years) {
    if (verbose) {
      message("Year ", year, " (", which(years == year),
              " of ", length(years), ")")
    }

    year_start <- paste0(year, "-01-01T00:00:00")
    year_end   <- paste0(year, "-12-31T23:00:00")

    # build expected file paths for this year
    nc_paths <- file.path(
      outdir, sites$site_id,
      paste(model, scenario, year, "nc", sep = ".")
    )

    # if all sites already have files, skip the S3 fetch entirely
    if (!overwrite && all(file.exists(nc_paths))) {
      if (verbose) message("  all ", nrow(sites), " sites exist, skipping")
      for (k in seq_len(nrow(sites))) {
        ri <- ri + 1L
        result_list[[ri]] <- data.frame(
          site_id = sites$site_id[k], year = year,
          file = nc_paths[k], stringsAsFactors = FALSE
        )
      }
      next
    }

    # fetch each variable for ALL sites in one S3 call per variable
    var_data <- vector("list", length(wrf_vars))
    names(var_data) <- wrf_vars

    for (v in wrf_vars) {
      if (verbose) message("  fetching ", v)
      var_data[[v]] <- cae_fetch_points(
        variable   = v,
        model      = model,
        scenario   = scenario,
        points     = sites[, c("site_id", "lon", "lat")],
        timescale  = "1hr",
        resolution = resolution,
        start_time = year_start,
        end_time   = year_end
      )
    }

    # grab time values from the first variable, first site
    first_site <- sites$site_id[1]
    first_mask <- var_data[[1]]$site_id == first_site
    time_vals  <- var_data[[1]]$time[first_mask]

    # write one NetCDF per site
    for (k in seq_len(nrow(sites))) {
      sid <- sites$site_id[k]
      nc_file <- nc_paths[k]

      if (file.exists(nc_file) && !overwrite) {
        if (verbose) message("  ", sid, ": exists, skipping")
        ri <- ri + 1L
        result_list[[ri]] <- data.frame(
          site_id = sid, year = year,
          file = nc_file, stringsAsFactors = FALSE
        )
        next
      }

      # create site output directory
      dir.create(dirname(nc_file), showWarnings = FALSE, recursive = TRUE)

      # extract this site's values for each variable
      site_vals <- lapply(var_data, function(df) df$value[df$site_id == sid])

      # unit conversions -- reuse helpers from convert.R
      site_vals[["prec"]] <- .precip_to_flux(site_vals[["prec"]], 3600)
      site_vals[["q2"]]   <- .mixing_ratio_to_specific_humidity(site_vals[["q2"]])
      wind_speed <- .compute_wind_speed(site_vals[["u10"]], site_vals[["v10"]])

      # map WRF names to CF names
      converted <- stats::setNames(
        site_vals,
        .wrf_cf_map$cf_name[match(names(site_vals), .wrf_cf_map$wrf_name)]
      )
      converted[["wind_speed"]] <- wind_speed

      .write_site_cf_nc(
        converted, time_vals,
        lat = sites$lat[k], lon = sites$lon[k],
        outfile = nc_file, year = year
      )

      if (verbose) message("  wrote ", nc_file)

      ri <- ri + 1L
      result_list[[ri]] <- data.frame(
        site_id = sid, year = year,
        file = nc_file, stringsAsFactors = FALSE
      )
    }
  }

  results <- do.call(rbind, result_list[seq_len(ri)])
  invisible(results)
}


#' Write a single-site CF-standard NetCDF file
#'
#' Creates a CF-1.8 compliant NetCDF with latitude, longitude, and
#' unlimited time dimensions. Used internally by [cae_build_met_drivers()].
#'
#' @param converted named list of CF variable names -> numeric vectors
#' @param time_vals POSIXct vector of timesteps
#' @param lat numeric; site latitude (WGS84)
#' @param lon numeric; site longitude (WGS84)
#' @param outfile character; output file path
#' @param year integer; reference year for time origin
#' @keywords internal
.write_site_cf_nc <- function(converted, time_vals, lat, lon, outfile, year) {

  time_origin <- paste0(year, "-01-01 00:00:00")
  time_secs <- as.numeric(difftime(
    time_vals,
    as.POSIXct(time_origin, tz = "UTC"),
    units = "secs"
  ))

  lat_dim  <- ncdf4::ncdim_def("latitude", "degree_north",
                                lat, create_dimvar = TRUE)
  lon_dim  <- ncdf4::ncdim_def("longitude", "degree_east",
                                lon, create_dimvar = TRUE)
  time_dim <- ncdf4::ncdim_def("time",
                                paste("seconds since", time_origin),
                                time_secs,
                                create_dimvar = TRUE, unlim = TRUE)
  dims <- list(lat_dim, lon_dim, time_dim)

  # CF units from the mapping table + wind_speed
  cf_units <- stats::setNames(.wrf_cf_map$cf_units, .wrf_cf_map$cf_name)
  cf_units[["wind_speed"]] <- "m s-1"

  nc_vars <- lapply(names(converted), function(vname) {
    ncdf4::ncvar_def(vname, cf_units[[vname]], dims, missval = -9999.0)
  })

  nc <- ncdf4::nc_create(outfile, nc_vars)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  for (j in seq_along(converted)) {
    ncdf4::ncvar_put(nc, nc_vars[[j]], converted[[j]])
  }

  ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.8")
  ncdf4::ncatt_put(nc, 0, "source", "Cal-Adapt Analytics Engine (CADCAT)")
  ncdf4::ncatt_put(nc, 0, "created_by", "caladaptaer::cae_build_met_drivers")
  ncdf4::ncatt_put(nc, 0, "history",
                    paste("Created", Sys.time(), "by caladaptaer"))
}
