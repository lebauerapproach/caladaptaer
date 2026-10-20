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
