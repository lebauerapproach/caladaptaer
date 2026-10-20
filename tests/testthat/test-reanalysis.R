test_that("UCLA file listings parse timestamps and September source folders", {
  queried <- character()
  local_mocked_bindings(.ucla_wrf_list_prefix=function(prefix) {
    queried <<- c(queried,prefix)
    list(contents=data.frame(key=paste0(prefix,"auxhist_d01_2023-09-01_00:00:00"),
                              last_modified="2025-09-01",size=123))
  })
  x <- cae_reanalysis_files("2023-09-01","2023-09-01","d03")
  expect_equal(nrow(x),1L)
  expect_equal(x$time,as.POSIXct("2023-09-01",tz="UTC"))
  expect_equal(queried,"downscaled_products/reanalysis/era5/hourly/2023/d03/")
})

test_that("overlapping September boundary selects the new source folder", {
  local_mocked_bindings(.ucla_wrf_list_prefix = function(prefix) {
    list(contents = data.frame(key = paste0(prefix, "auxhist_d01_2023-09-01_00:00:00"),
                               last_modified = "2025-09-01", size = 123))
  })
  x <- cae_reanalysis_files("2023-08-31 23:00:00", "2023-09-01 00:00:00")
  expect_equal(nrow(x), 1L)
  expect_match(x$key, "/2023/d03/")
  expect_match(x$precipitation_key, "/2022/d03/")
})

test_that("warm-start accumulation at a source boundary is not counted as rain", {
  # Observed UCLA d03 counter values around 2023-09-01 at the pilot cell.
  old_previous <- 701.59058
  old_final <- 701.59058
  new_initial <- 17.75461
  new_next <- 17.75461
  expect_equal(.reanalysis_precip_increment(new_initial, old_previous, old_final), 0)
  expect_equal(.reanalysis_precip_increment(new_next, new_initial), 0)
  expect_equal(.reanalysis_precip_increment(1, 100), 1)
})

test_that("discard mode cleans owned downloads and preserves existing sources", {
  cache <- tempfile()
  dir.create(cache)
  on.exit(unlink(cache, recursive = TRUE))
  local_mocked_bindings(.download_reanalysis_file = function(key, cache_dir) {
    path <- file.path(cache_dir, key)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    if (!file.exists(path)) writeLines("source", path)
    path
  })
  read <- function(path) {
    expect_true(file.exists(path))
    readLines(path)
  }
  expect_equal(.with_reanalysis_file("new.nc", cache, FALSE, read), "source")
  expect_length(list.files(cache, all.files = TRUE, no.. = TRUE), 0L)
  expect_error(.with_reanalysis_file("failed.nc", cache, FALSE, function(path) stop("read failed")), "read failed")
  expect_length(list.files(cache, all.files = TRUE, no.. = TRUE), 0L)
  cached <- .download_reanalysis_file("existing.nc", cache)
  expect_equal(.with_reanalysis_file("existing.nc", cache, FALSE, read), "source")
  expect_true(file.exists(cached))
  .with_reanalysis_file("retained.nc", cache, TRUE, read)
  expect_true(file.exists(file.path(cache, "retained.nc")))
})



test_that("streaming preserves actual times and interval precipitation", {
  cache <- tempfile()
  dir.create(cache)
  out <- tempfile()
  dir.create(out)
  on.exit(unlink(c(cache, out), recursive = TRUE))
  nominal <- as.POSIXct(c("2023-12-31 23:00:00", "2024-01-01 00:00:00", "2024-01-01 01:00:00"), tz = "UTC")
  actual <- nominal + c(0, 11, 7)
  files <- data.frame(time = nominal, key = paste0("h", 1:3, ".nc"))
  files$precipitation_key <- files$key
  grid <- NULL
  for (j in 1:3) {
    x <- ncdf4::ncdim_def("west_east", "", 1:3)
    y <- ncdf4::ncdim_def("south_north", "", 1:3)
    nchar_dim <- ncdf4::ncdim_def("DateStrLen", "", 1:19, create_dimvar = FALSE)
    variables <- c("T2", "PSFC", "Q2", "SWDNB", "LWDNB", "U10", "V10", "RAINC", "RAINNC")
    nc <- ncdf4::nc_create(file.path(cache, files$key[j]), c(lapply(variables, function(v) ncdf4::ncvar_def(v, "", list(x, y), -9999)), list(ncdf4::ncvar_def("Times", "", list(nchar_dim), prec = "char"))))
    attrs <- list(MAP_PROJ = 1L, TRUELAT1 = 30, TRUELAT2 = 60, MOAD_CEN_LAT = 38, STAND_LON = -70, CEN_LON = -121, CEN_LAT = 38, DX = 3000, DY = 3000)
    for (v in names(attrs)) ncdf4::ncatt_put(nc, 0, v, attrs[[v]])
    defaults <- c(T2 = if (j == 3L) NA_real_ else 280, PSFC = 100000, Q2 = .005, SWDNB = 100, LWDNB = 300, U10 = 1, V10 = 2, RAINC = j, RAINNC = 0)
    for (v in variables) ncdf4::ncvar_put(nc, v, array(defaults[v], c(3, 3)))
    ncdf4::ncvar_put(nc, "Times", format(actual[j], "%Y-%m-%d_%H:%M:%S", tz = "UTC"))
    grid <- .raw_wrf_grid(nc)
    ncdf4::nc_close(nc)
  }
  sites <- data.frame(site_id = c("a", "b"), lon = -121, lat = 38)
  paths <- file.path(out, paste0(sites$site_id, ".nc"))
  manifest <- .reanalysis_manifest(files, out, "d03", 2024)
  identity <- .driver_identity("ERA5", "reanalysis", "d03", list(manifest = manifest$checksum))
  .stream_reanalysis_year(sites, files, paths, cache, "d03", 2024, 1L, FALSE, manifest, identity, FALSE)
  for (path in paths) {
    expect_true(.valid_reanalysis_file(path, nominal[-1], 38, -121, identity))
    nc <- ncdf4::nc_open(path)
    expect_equal(as.numeric(ncdf4::ncvar_get(nc, "time")), as.numeric(actual[-1]))
    expect_equal(as.numeric(ncdf4::ncvar_get(nc, "source_nominal_time")), as.numeric(nominal[-1]))
    bounds <- ncdf4::ncvar_get(nc, "time_bounds")
    expect_equal(sum(ncdf4::ncvar_get(nc, "precipitation_flux") * (bounds[2, ] - bounds[1, ])), 2, tolerance = 1e-6)
    expect_true(is.na(ncdf4::ncvar_get(nc, "air_temperature")[2]))
    ncdf4::nc_close(nc)
  }
  # The old run's precipitation endpoint can differ from the new weather snapshot.
  old <- file.path(cache, "old.nc")
  file.copy(file.path(cache, files$key[2]), old)
  nc <- ncdf4::nc_open(old, write = TRUE)
  ncdf4::ncvar_put(nc, "Times", format(nominal[2] + 5, "%Y-%m-%d_%H:%M:%S", tz = "UTC"))
  ncdf4::ncvar_put(nc, "RAINC", array(1.5, c(3, 3)))
  ncdf4::nc_close(nc)
  files$precipitation_key[2] <- "old.nc"
  manifest <- .reanalysis_manifest(files, out, "d03", 2024)
  .stream_reanalysis_year(sites, files, paths, cache, "d03", 2024, 1L, FALSE, manifest, identity, FALSE)
  nc <- ncdf4::nc_open(paths[1])
  bounds <- ncdf4::ncvar_get(nc, "time_bounds")
  expect_equal(bounds[2, 1], as.numeric(nominal[2] + 5))
  expect_equal(as.numeric(ncdf4::ncvar_get(nc, "time")), as.numeric(actual[-1]))
  expect_equal(sum(ncdf4::ncvar_get(nc, "precipitation_flux") * (bounds[2, ] - bounds[1, ])), 1.5, tolerance = 1e-6)
  ncdf4::nc_close(nc)
  expect_true(all(file.exists(file.path(cache, files$key))))
  expect_error(cae_build_reanalysis_drivers(sites, files, 2024, 2024, out, cache), "coverage incomplete")
})


test_that("failed source downloads cannot become cached sources", {
  cache <- tempfile()
  dir.create(cache)
  on.exit(unlink(cache, recursive = TRUE))
  local_mocked_bindings(download.file = function(url, destfile, ...) {
    writeLines("partial", destfile)
    1L
  }, .package = "utils")
  expect_error(.download_reanalysis_file("hour.nc", cache), "download incomplete")
  expect_length(list.files(cache, all.files = TRUE, no.. = TRUE), 0L)
})
