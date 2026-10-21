test_that("Zarr staging batches new chunks and reuses existing chunks", {
  target <- tempfile()
  dir.create(target)
  on.exit(unlink(target, recursive = TRUE))
  file.create(file.path(target, "existing"))
  batches <- list()
  local_mocked_bindings(.stage_zarr_download_batch = function(base, keys, target) {
    batches[[length(batches) + 1L]] <<- keys
  })
  .stage_zarr_download("https://example.test", c("existing", as.character(1:9), "1"), target)
  expect_equal(lengths(batches), c(4L, 4L, 1L))
  expect_equal(unlist(batches, use.names = FALSE), as.character(1:9))
})

test_that("Zarr batch downloads publish complete files and clean failed downloads", {
  target <- tempfile()
  dir.create(target)
  on.exit(unlink(target, recursive = TRUE))
  local_mocked_bindings(download.file = function(url, destfile, ...) {
    for (path in destfile) writeLines("chunk", path)
    0L
  }, .package = "utils")
  .stage_zarr_download_batch("https://example.test", c("v/0", "v/1"), target)
  expect_equal(readLines(file.path(target, "v/0")), "chunk")
  expect_equal(readLines(file.path(target, "v/1")), "chunk")
  local_mocked_bindings(download.file = function(url, destfile, ...) {
    writeLines("partial", destfile[1])
    stop("network interrupted")
  }, .package = "utils")
  expect_error(.stage_zarr_download_batch("https://example.test", c("v/2", "v/3"), target), "network interrupted")
  expect_equal(sort(list.files(file.path(target, "v"))), c("0", "1"))
})


test_that("source chunk paths cannot escape the cache", {
  expect_error(.check_cache_keys(c("v/0", "../outside")), "Unsafe")
  expect_error(.check_cache_keys("/absolute"), "Unsafe")
  expect_silent(.check_cache_keys(c("v/0.1.2", ".zmetadata")))
})


test_that("mixed libcurl results never publish partial chunks", {
  target <- tempfile()
  dir.create(target)
  on.exit(unlink(target, recursive = TRUE))
  local_mocked_bindings(download.file = function(url, destfile, ...) {
    for (path in destfile) writeLines("partial", path)
    structure(0L, retvals = c(0L, 1L))
  }, .package = "utils")
  expect_error(.stage_zarr_download_batch("https://example.test", c("v/0", "v/1"), target), "Could not download")
  expect_length(list.files(file.path(target, "v")), 0L)
})
