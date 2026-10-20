#' Build CF-standard met driver files for multiple sites
#'
#' Extracts WRF data in bounded site batches, optionally staging reusable
#' source chunks locally, and writes one CF NetCDF per site per year.
#'
#' Output files follow the PEcAn CF Meteorology convention and can be
#' registered in BETY or used directly by ecosystem models that read
#' CF-standard met inputs.
#'
#' @param sites data.frame with columns `site_id`, `lon`, `lat` (WGS84
#'   decimal degrees). See Examples.
#' @param model character; WRF model or reanalysis source identifier
#'   (e.g. `"CESM2"` or `"ERA5"`). Default `"CESM2"`.
#'   See `cae_models(activity = "WRF")` for options.
#' @param scenario character; experiment identifier (e.g. `"ssp370"`,
#'   `"historical"`, or `"reanalysis"`). Default `"ssp370"`.
#'   See `cae_scenarios()` for options.
#' @param start_year integer; first year to process
#' @param end_year integer; last year to process
#' @param outdir character; root output directory. Files are written to
#'   `{outdir}/{site_id}/{model}.{scenario}.{year}.nc`.
#' @param resolution character; WRF grid label. Default `"d01"` (45 km).
#' @param overwrite logical; overwrite existing files? Default `FALSE`.
#' @param verbose logical; print progress messages? Default `TRUE`.
#' @param cache_dir Optional source chunk cache; NULL reads remotely.
#' @param batch_size Maximum sites held in memory together (default 64).
#' @param source Optional local source store/file, primarily for offline reuse.
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
                                 start_year, end_year, outdir,
                                 resolution = "d01", overwrite = FALSE,
                                 verbose = TRUE, cache_dir = NULL,
                                 batch_size = 64L, source = NULL) {
  .validate_sites(sites)
  if (length(batch_size) != 1L || !is.finite(batch_size) || batch_size < 1 || batch_size != as.integer(batch_size)) stop("Invalid batch_size")
  if (start_year > end_year) stop("start_year exceeds end_year")
  sites$site_id <- as.character(sites$site_id)
  results <- list()
  for (year in seq.int(start_year, end_year)) {
    reference <- .resolve_store("t2", model, scenario, "1hr", resolution, source)
    meta <- .store_metadata(reference, "t2")
    indices <- which(format(meta$times, "%Y", tz = "UTC") == year)
    times <- meta$times[indices]
    .validate_driver_year(times, year)
    grid <- .store_grid(meta)
    mapped <- cae_grid_cells(sites, grid)
    paths <- file.path(outdir, sites$site_id, paste(model, scenario, year, "nc", sep = "."))
    identity <- .driver_identity(model, scenario, resolution, source)
    complete <- vapply(seq_len(nrow(sites)), function(i) .valid_driver_file(paths[i], times, sites$lat[i], sites$lon[i], identity), logical(1))
    wanted <- which(overwrite | !complete)
    sources <- stats::setNames(rep(list(source), nrow(.wrf_cf_map)), .wrf_cf_map$wrf_name)
    if (length(wanted) && !is.null(cache_dir) && is.null(source)) {
      available <- cae_check_variables(model, scenario, resolution = resolution)
      for (v in names(sources)) {
        variables <- if (v == "prec" && !any(available$variable == "prec" & available$available)) c("rainc", "rainnc") else v
        start <- if (length(variables) == 2L) min(times) - 3600 else min(times)
        staged <- lapply(variables, function(component) cae_stage_zarr(component, model, scenario,
          sites[wanted, ], cache_dir, start, max(times), resolution))
        sources[[v]] <- if (length(staged) == 1L) staged[[1]] else stats::setNames(staged, variables)
      }
    }
    cell_ids <- unique(mapped$cell_id[wanted])
    batches <- split(cell_ids, ceiling(seq_along(cell_ids) / batch_size))
    for (ids in batches) {
      k <- wanted[mapped$cell_id[wanted] %in% ids]
      cells <- mapped[match(ids, mapped$cell_id), c("cell_id", "cell_lon", "cell_lat")]
      names(cells) <- c("site_id", "lon", "lat")
      if (verbose) message(model, " / ", scenario, " / ", year, ": ", nrow(cells), " cells")
      data <- lapply(names(sources), function(v) cae_fetch_points(v, model, scenario,
        cells, resolution = resolution, start_time = min(times), end_time = max(times), source = sources[[v]]))
      names(data) <- names(sources)
      for (i in k) {
        values <- lapply(data, function(x) x[x$site_id == mapped$cell_id[i], c("time", "value")])
        if (!all(vapply(values, function(x) identical(x$time, times), logical(1)))) stop("Variable timestamps differ")
        raw <- lapply(values, `[[`, "value")
        .publish_driver(.convert_driver_values(raw), times, sites$lat[i], sites$lon[i], paths[i], year,
          .combine_provenance(data), identity)
      }
    }
    results[[length(results) + 1L]] <- data.frame(site_id = sites$site_id, year = year, file = paths)
  }
  invisible(do.call(rbind, results))
}

.convert_driver_values <- function(values, dt_seconds = 3600) {
  values$prec <- .precip_to_flux(values$prec, dt_seconds)
  values$q2 <- .mixing_ratio_to_specific_humidity(values$q2)
  wind <- .compute_wind_speed(values$u10, values$v10)
  names(values) <- .wrf_cf_map$cf_name[match(names(values), .wrf_cf_map$wrf_name)]
  values$wind_speed <- wind
  values
}


.validate_sites <- function(sites) {
  if (!is.data.frame(sites) || !all(c("site_id", "lon", "lat") %in% names(sites)) || !nrow(sites) ||
      anyNA(sites[c("site_id", "lon", "lat")]) || anyDuplicated(sites$site_id) ||
      any(!is.finite(sites$lon)) || any(!is.finite(sites$lat))) stop("Provide unique site_id and finite lon/lat")
  if (any(!nzchar(as.character(sites$site_id))) || any(grepl("[/\\\\]", sites$site_id)) || any(sites$site_id %in% c(".", ".."))) stop("Unsafe site_id")
}

.validate_driver_year <- function(times, year) {
  expected <- seq(.parse_time(paste0(year, "-01-01")), .parse_time(paste0(year, "-12-31 23:00:00")), by = "hour")
  # Some sources omit every February 29 record even under Gregorian metadata.
  if (!any(format(times, "%m-%d", tz = "UTC") == "02-29")) expected <- expected[format(expected, "%m-%d", tz = "UTC") != "02-29"]
  if (!identical(as.numeric(times), as.numeric(expected))) stop("Source coverage incomplete or irregular for ", year, "; no gap filling is performed")
}

.driver_identity <- function(model, scenario, resolution, source = NULL) {
  fingerprint <- if (is.null(source)) NULL else lapply(as.list(source), function(path) {
    if (file.exists(path) && !dir.exists(path)) {
      info <- file.info(path)
      list(path = normalizePath(path), size = info$size, mtime = as.numeric(info$mtime))
    } else path
  })
  as.character(jsonlite::toJSON(list(model = model, scenario = scenario, resolution = resolution,
    source = fingerprint, export_version = 1L), auto_unbox = TRUE, null = "null", digits = NA))
}

.publish_driver <- function(values, times, lat, lon, path, year, provenance, identity) {
  if (!length(times) || anyNA(times) || anyDuplicated(times) || any(diff(as.numeric(times)) <= 0)) stop("Invalid driver timestamps")
  if (!all(vapply(values, function(x) length(x) == length(times) && !any(is.infinite(x)), logical(1)))) stop("Invalid driver values")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(tmpdir = dirname(path), fileext = ".nc")
  on.exit(unlink(tmp), add = TRUE)
  .write_site_cf_nc(values, times, lat, lon, tmp, year, provenance)
  nc <- ncdf4::nc_open(tmp, write = TRUE)
  tryCatch(ncdf4::ncatt_put(nc, 0, "cae_request", identity), finally = ncdf4::nc_close(nc))
  if (!.valid_driver_file(tmp, times, lat, lon, identity)) stop("Driver failed validation before publication")
  if (!file.rename(tmp, path)) stop("Could not publish driver: ", path)
  invisible(path)
}

.valid_driver_file <- function(path, times, lat, lon, identity) {
  if (!file.exists(path)) return(FALSE)
  tryCatch({
    nc <- ncdf4::nc_open(path)
    on.exit(ncdf4::nc_close(nc))
    meta <- .store_metadata(path, "air_temperature")
    identical(ncdf4::ncatt_get(nc, 0, "cae_request")$value, identity) &&
      identical(as.numeric(meta$times), as.numeric(times)) &&
      isTRUE(all.equal(as.numeric(nc$dim$latitude$vals), lat)) &&
      isTRUE(all.equal(as.numeric(nc$dim$longitude$vals), lon)) &&
      all(c(.wrf_cf_map$cf_name, "wind_speed") %in% names(nc$var)) &&
      all(vapply(c(.wrf_cf_map$cf_name, "wind_speed"), function(v) {
        x <- ncdf4::ncvar_get(nc, v)
        length(x) == length(times) && !any(is.infinite(x))
      }, logical(1))) &&
      all(vapply(c("source", "source_calendar", "source_time_units", "history"), function(v) ncdf4::ncatt_get(nc, 0, v)$hasatt, logical(1)))
  }, error = function(e) FALSE)
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
#' @param provenance Optional source metadata carried by retrieved objects.
#' @keywords internal
.write_site_cf_nc <- function(converted, time_vals, lat, lon, outfile, year, provenance = NULL) {

  encoding <- .export_time(time_vals, provenance)

  lat_dim  <- ncdf4::ncdim_def("latitude", "degree_north",
                                lat, create_dimvar = TRUE)
  lon_dim  <- ncdf4::ncdim_def("longitude", "degree_east",
                                lon, create_dimvar = TRUE)
  time_dim <- ncdf4::ncdim_def("time",
                                encoding$units,
                                encoding$values,
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

  .write_provenance(nc, provenance, time_vals,
    "CF export: precipitation mm per timestep to kg m-2 s-1; mixing ratio to specific humidity; wind speed from u10/v10")
}
