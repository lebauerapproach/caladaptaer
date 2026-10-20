make_weather_store <- function(times) {
  f <- tempfile(fileext = ".nc")
  xs <- 500000 + 3000 * (0:4)
  ys <- 4000000 + 3000 * (0:3)
  dims <- list(
    ncdf4::ncdim_def("x", "m", xs),
    ncdf4::ncdim_def("y", "m", ys),
    ncdf4::ncdim_def("time", "hours since 2024-01-01", as.numeric(difftime(times, as.POSIXct("2024-01-01", tz = "UTC"), units = "hours")))
  )
  nc <- ncdf4::nc_create(f, list(
    ncdf4::ncvar_def("t2", "K", dims, -9999),
    ncdf4::ncvar_def("crs", "", list(), -9999)
  ))
  values <- array(seq_len(5 * 4 * length(times)), c(5, 4, length(times)))
  ncdf4::ncvar_put(nc, "t2", values)
  ncdf4::ncatt_put(nc, "crs", "spatial_ref", sf::st_crs(32611)$wkt)
  ncdf4::ncatt_put(nc, "t2", "grid_mapping", "crs")
  ncdf4::nc_close(nc)
  list(path = f, x = xs, y = ys, values = values)
}

