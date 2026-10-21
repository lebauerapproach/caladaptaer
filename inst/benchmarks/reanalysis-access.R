#!/usr/bin/env Rscript
# Run each mode in a fresh process; capture CPL_CURL_VERBOSE stderr for bytes.
# Rscript reanalysis-access.R MODE SITE_CSV OUTPUT_DIR SOURCE_KEY
args <- commandArgs(trailingOnly = TRUE)
mode <- match.arg(args[1], c("remote", "staged"))
sites <- utils::read.csv(args[2])
out <- args[3]
key <- args[4]
dir.create(out, recursive = TRUE, showWarnings = FALSE)
library(caladaptaer)
Sys.setenv(GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR", CPL_CURL_VERBOSE = "YES")
url <- paste0("https://wrf-cmip6-noversioning.s3.us-west-2.amazonaws.com/", key)
grid_file <- file.path(out, "grid-source.nc")
if (!file.exists(grid_file)) stop("Stage a representative file as grid-source.nc first")
nc <- ncdf4::nc_open(grid_file)
grid <- caladaptaer:::.raw_wrf_grid(nc)
ncdf4::nc_close(nc)
cells <- caladaptaer::cae_grid_cells(sites, grid)
cells <- cells[!duplicated(cells$cell_id), ]
variables <- c("T2", "PSFC", "Q2", "SWDNB", "LWDNB", "U10", "V10", "RAINC", "RAINNC")
elapsed <- system.time({
  if (mode == "staged") {
    path <- file.path(out, "fresh-download.nc")
    utils::download.file(url, path, mode = "wb", quiet = TRUE)
    nc <- ncdf4::nc_open(path)
    values <- lapply(variables, function(v) {
      x <- ncdf4::ncvar_get(nc, v)
      as.numeric(x[cbind(cells$cell_i, cells$cell_j)])
    })
    ncdf4::nc_close(nc)
  } else {
    values <- lapply(variables, function(v) {
      source <- paste0('HDF5:"/vsicurl/', url, '"://', v)
      x <- stars::read_stars(source)[[1]]
      # HDF5Image retains the stored WRF row order for these ungeoreferenced arrays.
      as.numeric(x[cbind(cells$cell_i, cells$cell_j)])
    })
  }
})[["elapsed"]]
names(values) <- variables
saveRDS(values, file.path(out, paste0(mode, "-values.rds")))
peak <- grep("VmHWM", readLines("/proc/self/status"), value = TRUE)
measurement <- list(method = mode, elapsed_seconds = elapsed, sites = nrow(sites),
  cells = nrow(cells), source_bytes = file.info(grid_file)$size,
  peak_process_memory = peak,
  extracted_variables = variables)
jsonlite::write_json(measurement, file.path(out, paste0(mode, ".json")), auto_unbox = TRUE, pretty = TRUE)
print(measurement)
