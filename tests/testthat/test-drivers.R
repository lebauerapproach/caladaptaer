test_that("bulk builder writes and reuses a complete local-source year", {
  year <- 2024L
  f <- tempfile(fileext = ".nc")
  times <- 0:(365 * 24 - 1)
  dims <- list(
    ncdf4::ncdim_def("x", "m", 500000 + 3000 * (0:2)),
    ncdf4::ncdim_def("y", "m", 4000000 + 3000 * (0:2)),
    ncdf4::ncdim_def("time", "hours since 2024-01-01", times)
  )
  vars <- lapply(seq_len(nrow(.wrf_cf_map)), function(i) ncdf4::ncvar_def(.wrf_cf_map$wrf_name[i], .wrf_cf_map$wrf_units[i], dims, -9999))
  nc <- ncdf4::nc_create(f, c(vars, list(ncdf4::ncvar_def("crs", "", list(), -9999))))
  defaults <- c(t2 = 285, psfc = 100000, prec = 1, q2 = .005, swdnb = 100, lwdnb = 300, u10 = 1, v10 = 2)
  for (v in names(defaults)) {
    ncdf4::ncvar_put(nc, v, array(defaults[v], c(3, 3, length(times))))
    ncdf4::ncatt_put(nc, v, "grid_mapping", "crs")
  }
  ncdf4::ncatt_put(nc, "crs", "spatial_ref", sf::st_crs(32611)$wkt)
  ncdf4::ncatt_put(nc, "time", "calendar", "365_day")
  ncdf4::nc_close(nc)
  xy <- sf::st_coordinates(sf::st_transform(sf::st_sfc(sf::st_point(c(503000, 4003000)), crs = 32611), 4326))
  sites <- data.frame(site_id = c("a", "b"), lon = xy[1], lat = xy[2])
  out <- tempfile()
  built <- cae_build_met_drivers(sites, "ERA5", "reanalysis", year, year, out, "d03", source = f, verbose = FALSE)
  native <- .store_metadata(f, "t2")$times
  expect_equal(nrow(built), 2L)
  expect_true(.valid_driver_file(built$file[1], native, sites$lat[1], sites$lon[1], .driver_identity("ERA5", "reanalysis", "d03", f)))
  before <- file.info(built$file)$mtime
  again <- cae_build_met_drivers(sites, "ERA5", "reanalysis", year, year, out, "d03", source = f, verbose = FALSE)
  expect_equal(file.info(again$file)$mtime, before)
  nc <- ncdf4::nc_open(built$file[1])
  expect_equal(sum(ncdf4::ncvar_get(nc, "precipitation_flux")) * 3600, length(times), tolerance = 1e-6)
  ncdf4::nc_close(nc)
  writeLines("interrupted", built$file[1])
  repaired <- cae_build_met_drivers(sites, "ERA5", "reanalysis", year, year, out, "d03", source = f, verbose = FALSE)
  expect_true(.valid_driver_file(repaired$file[1], native, sites$lat[1], sites$lon[1], .driver_identity("ERA5", "reanalysis", "d03", f)))
})


test_that("publication preserves missingness and rejects inconsistent lengths", {
  times <- .parse_time("2024-01-01") + 0:1 * 3600
  values <- lapply(.wrf_cf_map$wrf_name, function(v) c(1, NA_real_)) |> stats::setNames(.wrf_cf_map$wrf_name)
  values <- .convert_driver_values(values)
  path <- tempfile(fileext = ".nc")
  on.exit(unlink(path))
  identity <- .driver_identity("CESM2", "ssp370", "d03")
  .publish_driver(values, times, 38, -121, path, 2024, NULL, identity)
  expect_true(.valid_driver_file(path, times, 38, -121, identity))
  expect_false(.valid_driver_file(path, times, 39, -121, identity))
  expect_false(.valid_driver_file(path, times, 38, -121, .driver_identity("CESM2", "ssp585", "d03")))
  values$air_temperature <- 1
  expect_error(.publish_driver(values, times, 38, -121, path, 2024, NULL, identity), "Invalid driver values")
  expect_true(.valid_driver_file(path, times, 38, -121, identity))
})
