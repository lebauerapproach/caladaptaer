test_that("catalog loads and has expected structure", {
  skip_if_offline()

  cat <- ca_catalog()
  expect_s3_class(cat, "data.frame")
  expect_true(nrow(cat) > 1000)

  expected_cols <- c("activity_id", "source_id", "experiment_id",
                     "table_id", "variable_id", "grid_label", "path")
  expect_true(all(expected_cols %in% names(cat)))

  # should have both WRF and LOCA2
  expect_true("WRF" %in% cat$activity_id)
  expect_true("LOCA2" %in% cat$activity_id)
})


test_that("ca_models returns known GCMs", {
  skip_if_offline()

  wrf_models <- ca_models(activity = "WRF")
  expect_true("CESM2" %in% wrf_models)
  expect_true("MIROC6" %in% wrf_models)
  expect_true(length(wrf_models) >= 8)
})


test_that("ca_scenarios returns known SSPs", {
  skip_if_offline()

  ssps <- ca_scenarios(activity = "WRF", model = "CESM2")
  expect_true("ssp370" %in% ssps)
  expect_true("historical" %in% ssps)
})


test_that("ca_variables returns expected WRF hourly vars", {
  skip_if_offline()

  vars <- ca_variables(activity = "WRF", timescale = "1hr")
  cf_vars <- c("t2", "prec", "psfc", "q2", "swdnb", "lwdnb", "u10", "v10")
  for (v in cf_vars) {
    expect_true(v %in% vars, info = paste("Missing variable:", v))
  }
})


test_that("ca_search filters correctly", {
  skip_if_offline()

  hits <- ca_search(activity = "WRF", model = "CESM2",
                    scenario = "ssp370", variable = "t2",
                    timescale = "1hr", resolution = "d01")
  expect_equal(nrow(hits), 1)
  expect_true(grepl("s3://cadcat/", hits$path[1]))
})


test_that("ca_search returns empty with warning on bad query", {
  skip_if_offline()

  expect_warning(
    hits <- ca_search(activity = "WRF", model = "NONEXISTENT_MODEL"),
    "No catalog entries"
  )
  expect_equal(nrow(hits), 0)
})


test_that("ca_check_variables reports coverage correctly", {
  skip_if_offline()

  # CESM2 should have all 8 directly
  cesm <- ca_check_variables("CESM2", "ssp370")
  expect_equal(nrow(cesm), 8)
  expect_true(all(cesm$available))

  # MPI-ESM1-2-HR should be missing prec but have it derivable
  mpi <- ca_check_variables("MPI-ESM1-2-HR", "ssp370")
  prec_row <- mpi[mpi$variable == "prec", ]
  expect_false(prec_row$available)
  expect_true(prec_row$derivable)
})


test_that("catalog caching works", {
  skip_if_offline()

  cat1 <- ca_catalog()
  cat2 <- ca_catalog()  # should hit cache
  expect_identical(cat1, cat2)

  # force refresh
  cat3 <- ca_catalog(refresh = TRUE)
  expect_equal(nrow(cat1), nrow(cat3))
})
