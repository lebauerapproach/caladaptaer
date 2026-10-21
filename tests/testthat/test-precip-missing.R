test_that("precipitation increments preserve gaps and handle known resets", {
  totals <- c(NA, 2, 5, NA, 9, 1, 4)
  expected <- c(NA, NA, 3, NA, NA, 1, 3)
  vector <- stars::st_as_stars(list(prec = array(totals, length(totals))))
  expect_equal(as.numeric(.deaccumulate_precip(vector)[[1]]), expected)

  grid <- stars::st_as_stars(list(prec = array(totals, c(1, 1, 7))))
  grid[[1]] <- units::set_units(grid[[1]], "mm")
  result <- .deaccumulate_precip(grid)[[1]]
  expect_equal(as.numeric(result), expected)
  expect_equal(units::deparse_unit(result), "mm")

  for (shape in list(1L, c(1L, 1L, 1L))) {
    missing <- stars::st_as_stars(list(prec = array(NA_real_, shape)))
    expect_true(all(is.na(.deaccumulate_precip(missing)[[1]])))
  }
})
