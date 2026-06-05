test_that("cae_fetch reads WRF hourly data from S3", {
  skip_if_offline()
  skip_on_cran()

  # read 1 timestep of temperature at 45km
  x <- cae_fetch("t2", model = "CESM2", scenario = "ssp370",
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


test_that("cae_fetch_point extracts a time series", {
  skip_if_offline()
  skip_on_cran()

  # Fresno, CA -- 24 hours
  ts <- cae_fetch_point("t2", model = "CESM2", scenario = "ssp370",
                       lon = -119.77, lat = 36.75,
                       n_timesteps = 24)

  expect_s3_class(ts, "data.frame")
  expect_equal(nrow(ts), 24)
  expect_true(all(c("time", "value") %in% names(ts)))

  # temperature in K, Central Valley in Sept
  expect_true(min(ts$value) > 270)  # > -3C
  expect_true(max(ts$value) < 320)  # < 47C
})


test_that("cae_fetch reads LOCA2 daily data from S3", {
  skip_if_offline()
  skip_on_cran()

  x <- cae_fetch("pr", model = "EC-Earth3", scenario = "ssp370",
                timescale = "day", resolution = "d03",
                n_timesteps = 1)

  expect_s3_class(x, "stars")

  d <- stars::st_dimensions(x)
  expect_true("time" %in% names(d))

  # daily precip should be non-negative
  vals <- units::drop_units(x[[1]])
  expect_true(min(vals, na.rm = TRUE) >= 0)
})


test_that("cae_fetch_points extracts multiple sites at once", {
  skip_if_offline()
  skip_on_cran()

  sites <- data.frame(
    site_id = c("fresno", "la", "sacramento"),
    lon = c(-119.77, -118.24, -121.49),
    lat = c(36.75, 34.05, 38.58)
  )

  ts <- cae_fetch_points("t2", model = "CESM2", scenario = "ssp370",
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
  ts2 <- cae_fetch_points("t2", model = "CESM2", scenario = "ssp370",
                         points = sites_no_id, n_timesteps = 24)
  expect_equal(sort(unique(ts2$site_id)), 1:3)
})


test_that("all 8 CF-standard met variables are readable", {
  skip_if_offline()
  skip_on_cran()
  # this test is slow -- each variable requires an S3 round trip
  skip_on_ci()

  cf_vars <- c("t2", "prec", "psfc", "q2", "swdnb", "lwdnb", "u10", "v10")

  for (v in cf_vars) {
    x <- cae_fetch(v, model = "CESM2", scenario = "ssp370",
                  n_timesteps = 1)
    expect_s3_class(x, "stars")
  }
})


test_that("cae_grid_cells snaps points to cells and dedups", {
  # synthetic projected grid, no S3 needed. 100 km cells in a Lambert CRS.
  crs <- paste("+proj=lcc +lat_0=38 +lon_0=-70 +lat_1=30 +lat_2=60",
               "+R=6370000 +units=m +no_defs")
  xs <- seq(-4.5e6, -3.6e6, by = 1e5)
  ys <- seq(0.6e6, 1.8e6, by = 1e5)
  g <- stars::st_as_stars(array(1, dim = c(length(xs), length(ys))))
  g <- stars::st_set_dimensions(g, 1, names = "x", values = xs)
  g <- stars::st_set_dimensions(g, 2, names = "y", values = ys)
  sf::st_crs(g) <- crs

  pts <- data.frame(site_id = c("a", "b", "c"),
                    lon = c(-121.0, -121.05, -118.0),
                    lat = c(38.0, 38.0, 34.0))
  out <- cae_grid_cells(pts, g)

  expect_true(all(c("cell_i", "cell_j", "cell_id", "cell_lon", "cell_lat")
                  %in% names(out)))
  expect_equal(nrow(out), 3L)
  # a and b are ~4 km apart, far inside one 100 km cell -> same cell
  expect_equal(out$cell_id[1], out$cell_id[2])
  # c is hundreds of km away -> a different cell
  expect_false(out$cell_id[1] == out$cell_id[3])
})
