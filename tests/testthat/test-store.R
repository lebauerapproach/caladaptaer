test_that("date windows follow stored timestamps across omitted leap days", {
  times <- as.POSIXct(c("2024-02-28 23:00:00", "2024-03-01 00:00:00", "2024-03-01 01:00:00"), tz = "UTC")
  f <- make_weather_store(times)
  x <- .read_store_window(f$path, "t2", start_time = "2024-03-01", n_timesteps = 2)
  expect_equal(.get_time_values(x), times[2:3])
  expect_equal(as.numeric(x[[1]]), as.numeric(f$values[, , 2:3]))
  expect_error(.read_store_window(f$path, "t2", start_time = "2026-01-01"), "coverage")
})


test_that("selection uses coordinates after multiple missing leap days", {
  times <- as.POSIXct(c("2024-02-28 23:00:00", "2024-03-01 00:00:00",
                       "2028-02-28 23:00:00", "2028-03-01 00:00:00"), tz = "UTC")
  expect_equal(.select_store_time(times, "2028-03-01"), 4L)
  expect_error(.select_store_time(times, "2024-02-29", "2024-02-29 23:00:00"), "no source")
  leap <- as.POSIXct("2024-02-28 23:00:00", tz = "UTC") + 0:25 * 3600
  f <- make_weather_store(leap)
  out <- .read_store_window(f$path, "t2", "2024-02-29", "2024-02-29 23:00:00")
  expect_equal(.get_time_values(out), leap[2:25])
  expect_equal(as.numeric(out[[1]]), as.numeric(f$values[, , 2:25]))
})

test_that("seconds and hours encodings select equivalent values", {
  times <- as.POSIXct("2024-03-01", tz = "UTC") + 0:2 * 3600
  f <- make_weather_store(times)
  g <- make_weather_store(times)
  nc <- ncdf4::nc_open(g$path, write = TRUE)
  ncdf4::ncatt_put(nc, "time", "units", "seconds since 2024-01-01")
  ncdf4::ncvar_put(nc, "time", as.numeric(difftime(times, as.POSIXct("2024-01-01", tz = "UTC"), units = "secs")))
  ncdf4::nc_close(nc)
  expect_equal(.read_store_window(f$path, "t2"), .read_store_window(g$path, "t2"))
})


test_that("explicit no-leap calendars keep calendar labels", {
  times <- as.POSIXct("2024-01-01", tz = "UTC") + 0:2 * 3600
  f <- make_weather_store(times)
  nc <- ncdf4::nc_open(f$path, write = TRUE)
  ncdf4::ncatt_put(nc, "time", "calendar", "365_day")
  ncdf4::ncvar_put(nc, "time", c(1415, 1416, 1417))
  ncdf4::nc_close(nc)
  out <- .read_store_window(f$path, "t2")
  expect_equal(.get_time_values(out), as.POSIXct(c("2024-02-28 23:00:00", "2024-03-01 00:00:00", "2024-03-01 01:00:00"), tz = "UTC"))
})


test_that("windowed extraction preserves coordinates and values", {
  times <- as.POSIXct("2024-01-01", tz = "UTC") + 0:2 * 3600
  f <- make_weather_store(times)
  pts <- sf::st_as_sf(data.frame(x = f$x[c(2, 4)], y = f$y[c(2, 3)]), coords = c("x", "y"), crs = 32611) |> sf::st_transform(4326)
  xy <- sf::st_coordinates(pts)
  points <- data.frame(site_id = c("a", "b"), lon = xy[, 1], lat = xy[, 2])
  out <- cae_fetch_points("t2", "CESM2", "ssp370", points, source = f$path)
  expect_equal(out$value, c(f$values[2, 2, ], f$values[4, 3, ]))
  grid <- .read_store_window(f$path, "t2", points = points)
  expect_equal(stars::st_get_dimension_values(grid, "x"), f$x[2:4])
  expect_equal(stars::st_get_dimension_values(grid, "y"), f$y[2:3])
  outside <- data.frame(lon = 0, lat = 0)
  expect_error(cae_fetch_points("t2", "CESM2", "ssp370", outside, source = f$path), "outside")
})

test_that("single-cell windows retain values and duplicate point identity", {
  times <- as.POSIXct("2024-01-01", tz = "UTC") + 0:2 * 3600
  f <- make_weather_store(times)
  xy <- sf::st_coordinates(sf::st_transform(sf::st_sfc(sf::st_point(c(f$x[3], f$y[2])), crs = 32611), 4326))
  points <- data.frame(site_id = c("a", "b"), lon = rep(xy[1], 2), lat = rep(xy[2], 2))
  x <- cae_fetch_points("t2", "CESM2", "ssp370", points, source = f$path)
  expect_equal(x$value, rep(f$values[3, 2, ], 2))
  expect_equal(x$site_id, rep(c("a", "b"), each = 3))
})

test_that("precipitation differencing retains the first requested hour and resets", {
  f <- tempfile(fileext = ".nc")
  dims <- list(
    ncdf4::ncdim_def("x", "m", c(500000, 503000)),
    ncdf4::ncdim_def("y", "m", c(4000000, 4003000)),
    ncdf4::ncdim_def("time", "hours since 2023-12-31 23:00:00", 0:2)
  )
  nc <- ncdf4::nc_create(f, list(
    ncdf4::ncvar_def("rainc", "mm", dims, -9999),
    ncdf4::ncvar_def("rainnc", "mm", dims, -9999),
    ncdf4::ncvar_def("crs", "", list(), -9999)
  ))
  ncdf4::ncvar_put(nc, "rainc", array(rep(c(10, 2, 3), each = 4), c(2, 2, 3)))
  ncdf4::ncvar_put(nc, "rainnc", array(0, c(2, 2, 3)))
  for (v in c("rainc", "rainnc")) ncdf4::ncatt_put(nc, v, "grid_mapping", "crs")
  ncdf4::ncatt_put(nc, "crs", "spatial_ref", sf::st_crs(32611)$wkt)
  ncdf4::nc_close(nc)
  xy <- sf::st_coordinates(sf::st_transform(sf::st_sfc(sf::st_point(c(500000, 4000000)), crs = 32611), 4326))
  x <- cae_fetch_points("prec", "test", "test", data.frame(lon = xy[1], lat = xy[2]),
    start_time = "2024-01-01", n_timesteps = 2,
    source = list(rainc = f, rainnc = f)
  )
  expect_equal(x$value, c(2, 1))
  expect_equal(x$time, as.POSIXct("2024-01-01", tz = "UTC") + 0:1 * 3600)
})


test_that("reference geometry and geographic-grid local fetch preserve data", {
  times <- as.POSIXct("2024-01-01", tz = "UTC") + 0:2 * 3600
  f <- make_weather_store(times)
  grid <- cae_reference_grid(source = f$path)
  expect_equal(stars::st_get_dimension_values(grid, "x"), f$x)
  expect_true(all(is.na(grid[[1]])))
  expect_equal(cae_fetch("t2", "CESM2", "ssp370", source = f$path), .read_store_window(f$path, "t2"))
})


test_that("CF geographic axes without a grid mapping support point extraction", {
  path <- tempfile(fileext = ".nc")
  on.exit(unlink(path))
  dims <- list(ncdf4::ncdim_def("lon", "degrees_east", c(-122, -121)),
               ncdf4::ncdim_def("lat", "degrees_north", c(38, 39)),
               ncdf4::ncdim_def("time", "days since 2024-02-28", 0:2))
  nc <- ncdf4::nc_create(path, ncdf4::ncvar_def("tasmax", "K", dims, -9999))
  values <- array(1:12, c(2, 2, 3))
  ncdf4::ncvar_put(nc, "tasmax", values)
  ncdf4::nc_close(nc)
  out <- cae_fetch_point("tasmax", "EC-Earth3", "ssp370", -121, 38,
    timescale = "day", resolution = "d03", source = path)
  expect_equal(out$value, as.numeric(values[2, 1, ]))
  expect_equal(out$time, as.POSIXct("2024-02-28", tz = "UTC") + 0:2 * 86400)
})
