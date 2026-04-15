test_that("ca_fetch reads WRF hourly data from S3", {
  skip_if_offline()
  skip_on_cran()

  # read 1 timestep of temperature at 45km
  x <- ca_fetch("t2", model = "CESM2", scenario = "ssp370",
                n_timesteps = 1)

  expect_s3_class(x, "stars")

  # should have 3 dimensions: x, y, time
  d <- stars::st_dimensions(x)
  expect_true("time" %in% names(d))

  # temperature should be in Kelvin, reasonable range
  vals <- units::drop_units(x[[1]])
  expect_true(min(vals, na.rm = TRUE) > 200)  # warmer than -73C
  expect_true(max(vals, na.rm = TRUE) < 350)  # cooler than 77C
})


test_that("ca_fetch_point extracts a time series", {
  skip_if_offline()
  skip_on_cran()

  # Fresno, CA -- 24 hours
  ts <- ca_fetch_point("t2", model = "CESM2", scenario = "ssp370",
                       lon = -119.77, lat = 36.75,
                       n_timesteps = 24)

  expect_s3_class(ts, "data.frame")
  expect_equal(nrow(ts), 24)
  expect_true(all(c("time", "value") %in% names(ts)))

  # temperature in K, Central Valley in Sept
  expect_true(min(ts$value) > 270)  # > -3C
  expect_true(max(ts$value) < 320)  # < 47C
})


test_that("ca_fetch reads LOCA2 daily data from S3", {
  skip_if_offline()
  skip_on_cran()

  x <- ca_fetch("pr", model = "EC-Earth3", scenario = "ssp370",
                timescale = "day", resolution = "d03",
                n_timesteps = 1)

  expect_s3_class(x, "stars")

  d <- stars::st_dimensions(x)
  expect_true("time" %in% names(d))

  # daily precip should be non-negative
  vals <- units::drop_units(x[[1]])
  expect_true(min(vals, na.rm = TRUE) >= 0)
})


test_that("ca_fetch_points extracts multiple sites at once", {
  skip_if_offline()
  skip_on_cran()

  sites <- data.frame(
    site_id = c("fresno", "la", "sacramento"),
    lon = c(-119.77, -118.24, -121.49),
    lat = c(36.75, 34.05, 38.58)
  )

  ts <- ca_fetch_points("t2", model = "CESM2", scenario = "ssp370",
                        points = sites, n_timesteps = 24)

  expect_s3_class(ts, "data.frame")
  expect_equal(nrow(ts), 3 * 24)  # 3 sites x 24 hours
  expect_true(all(c("site_id", "time", "value") %in% names(ts)))
  expect_equal(sort(unique(ts$site_id)), c("fresno", "la", "sacramento"))

  # temperature in K, California in Sept
  expect_true(min(ts$value) > 270)
  expect_true(max(ts$value) < 330)

  # without site_id column, should auto-number
  sites_no_id <- sites[, c("lon", "lat")]
  ts2 <- ca_fetch_points("t2", model = "CESM2", scenario = "ssp370",
                         points = sites_no_id, n_timesteps = 24)
  expect_equal(sort(unique(ts2$site_id)), 1:3)
})


test_that("all 7 SIPNET-required variables are readable", {
  skip_if_offline()
  skip_on_cran()
  # this test is slow -- each variable requires an S3 round trip
  skip_on_ci()

  sipnet_vars <- c("t2", "prec", "psfc", "q2", "swdnb", "u10", "v10")

  for (v in sipnet_vars) {
    x <- ca_fetch(v, model = "CESM2", scenario = "ssp370",
                  n_timesteps = 1)
    expect_s3_class(x, "stars")
  }
})
