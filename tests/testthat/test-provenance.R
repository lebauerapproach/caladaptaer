test_that("CF export preserves native no-leap coordinates and metadata", {
  times <- as.POSIXct(c("2024-02-28 23:00:00", "2024-03-01 00:00:00"), tz = "UTC")
  f <- make_weather_store(times)
  x <- cae_fetch("t2", "CESM2", "ssp370", source = f$path)
  p <- attr(x, "cae_provenance")
  p$calendar <- "365_day"
  p$time_values <- c(1415, 1416)
  p$source <- "s3://cadcat/example/r1i1p1f1/t2"
  p$catalog <- .catalog_csv_url
  path <- tempfile(fileext = ".nc")
  on.exit(unlink(path))
  .write_site_cf_nc(list(air_temperature = c(280, NA_real_)), times, 38, -121, path, 2024, p)
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  expect_equal(as.numeric(ncdf4::ncvar_get(nc, "time")), c(1415, 1416))
  expect_equal(ncdf4::ncatt_get(nc, "time", "calendar")$value, "365_day")
  expect_equal(ncdf4::ncatt_get(nc, 0, "Conventions")$value, "CF-1.8, ACDD-1.3")
  for (name in c("source", "catalog", "model", "scenario", "member", "variable", "resolution", "requested_start", "requested_end", "source_calendar", "source_time_units", "history")) {
    expect_true(ncdf4::ncatt_get(nc, 0, name)$hasatt)
  }
  expect_true(is.na(ncdf4::ncvar_get(nc, "air_temperature")[2]))
})

test_that("staged source identity and caller metadata are explicit", {
  times <- as.POSIXct("2024-01-01", tz = "UTC") + 0:2 * 3600
  f <- make_weather_store(times)
  meta <- .store_metadata(f$path, "t2")
  folder <- tempfile()
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE))
  jsonlite::write_json(list(source = "s3://cadcat/example"), file.path(folder, ".caladaptaer-source.json"), auto_unbox = TRUE)
  p <- .source_provenance(folder, "t2", meta, 1:3)
  expect_equal(p$source, "s3://cadcat/example")
  expect_equal(p$catalog, .catalog_csv_url)
  expect_error(.export_time(times[-1], p), "timestamps differ")
  path <- tempfile(fileext = ".nc")
  on.exit(unlink(path), add = TRUE)
  .write_site_cf_nc(list(air_temperature = c(1, 2, 3)), times, 38, -121, path, 2024)
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  expect_equal(ncdf4::ncatt_get(nc, 0, "source")$value, "unknown")
  expect_equal(as.numeric(ncdf4::ncvar_get(nc, "time")), as.numeric(times))
})


test_that("grid exports preserve subset coordinates and CRS", {
  times <- as.POSIXct("2024-01-01", tz = "UTC") + 0:1 * 3600
  f <- make_weather_store(times)
  x <- cae_fetch("t2", "CESM2", "ssp370", source = f$path)
  variables <- stats::setNames(rep(list(x), nrow(.wrf_cf_map)), .wrf_cf_map$wrf_name)
  path <- tempfile(fileext = ".nc")
  on.exit(unlink(path))
  cae_write_cf_netcdf(variables, path)
  nc <- ncdf4::nc_open(path)
  expect_equal(as.numeric(nc$dim$x$vals), f$x)
  expect_equal(as.numeric(nc$dim$y$vals), f$y)
  expect_equal(ncdf4::ncatt_get(nc, "air_temperature", "grid_mapping")$value, "crs")
  expect_equal(sf::st_crs(ncdf4::ncatt_get(nc, "crs", "crs_wkt")$value), sf::st_crs(x))
  ncdf4::nc_close(nc)
})
