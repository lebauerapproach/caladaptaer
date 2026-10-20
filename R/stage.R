#' Stage the Zarr chunks needed by a point/time selection
#'
#' Downloads source chunks without changing their values or layout. Existing
#' chunks are reused. Coordinates and grid mapping metadata are retained so
#' the returned local store can be read using the `source` fetch argument.
#' @inheritParams cae_fetch
#' @param points Data frame of WGS84 `lon`, `lat` locations.
#' @param cache_dir Local directory for reusable source stores.
#' @return Path to a local Zarr store.
#' @export
cae_stage_zarr <- function(variable, model, scenario, points, cache_dir,
                           start_time = NULL, end_time = NULL,
                           resolution = "d03", timescale = "1hr") {
  path <- .resolve_store(variable, model, scenario, timescale, resolution)
  base <- sub("^s3://([^/]+)/", "https://\\1.s3.us-west-2.amazonaws.com/", path)
  base <- sub("/+$", "", base)
  target <- file.path(cache_dir, sub("^s3://", "", path))
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  download <- function(keys) .stage_zarr_download(base, keys, target)
  download(".zmetadata")
  metadata <- jsonlite::fromJSON(file.path(target, ".zmetadata"), simplifyVector = FALSE)$metadata
  if (is.null(metadata)) stop("Source has no consolidated Zarr metadata")
  # Materialize metadata files too; GDAL versions differ in consolidated access.
  .check_cache_keys(names(metadata))
  for (key in names(metadata)) {
    output <- file.path(target, key)
    if (!file.exists(output)) {
      dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
      jsonlite::write_json(metadata[[key]], output, auto_unbox = TRUE, null = "null", digits = NA)
    }
  }
  meta <- .store_metadata(path, variable)
  selected <- list(time = .select_store_time(meta$times, start_time, end_time))
  cells <- cae_grid_cells(points, .store_grid(meta))
  selected$x <- unique(cells$cell_i)
  selected$y <- unique(cells$cell_j)
  arrays <- sub("/\\.zarray$", "", grep("/\\.zarray$", names(metadata), value = TRUE))
  for (name in arrays) {
    spec <- metadata[[paste0(name, "/.zarray")]]
    axes <- unlist(metadata[[paste0(name, "/.zattrs")]]$`_ARRAY_DIMENSIONS`)
    chunks <- as.integer(unlist(spec$chunks))
    shape <- as.integer(unlist(spec$shape))
    if (all(c("x", "y") %in% axes)) {
      # Keep spatial chunk pairs, avoiding a Cartesian product across scattered sites.
      coords <- unique(data.frame(
        x = (cells$cell_i - 1L) %/% chunks[match("x", axes)],
        y = (cells$cell_j - 1L) %/% chunks[match("y", axes)]
      ))
      tids <- if ("time" %in% axes) unique((selected$time - 1L) %/% chunks[match("time", axes)]) else 0L
      keys <- unlist(lapply(tids, function(t) {
        apply(coords, 1, function(p) {
          v <- c(time = t, x = unname(p["x"]), y = unname(p["y"]))
          paste(v[axes], collapse = if (is.null(spec$dimension_separator)) "." else spec$dimension_separator)
        })
      }))
    } else {
      ranges <- lapply(seq_along(shape), function(i) seq.int(0L, ceiling(shape[i] / chunks[i]) - 1L))
      keys <- if (!length(ranges)) "0" else apply(do.call(expand.grid, ranges), 1, paste, collapse = ".")
    }
    download(paste0(name, "/", keys))
  }
  jsonlite::write_json(list(source = path), file.path(target, ".caladaptaer-source.json"), auto_unbox = TRUE)
  normalizePath(target)
}

# Bound open files and connections while overlapping independent S3 requests.
.stage_zarr_download <- function(base, keys, target) {
  .check_cache_keys(keys)
  keys <- unique(keys)
  keys <- keys[!file.exists(file.path(target, keys))]
  groups <- split(keys, ceiling(seq_along(keys) / 4L))
  for (group in groups) .stage_zarr_download_batch(base, group, target)
  invisible(file.path(target, keys))
}

.stage_zarr_download_batch <- function(base, keys, target) {
  output <- file.path(target, keys)
  for (directory in unique(dirname(output))) {
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  }
  tmp <- vapply(dirname(output), function(directory) tempfile(tmpdir = directory), character(1))
  on.exit(unlink(tmp), add = TRUE)
  status <- utils::download.file(paste0(base, "/", keys), tmp,
                                 method = "libcurl", mode = "wb", quiet = TRUE)
  if (status != 0 || any(attr(status, "retvals") != 0) || any(!file.exists(tmp)) || any(file.info(tmp)$size == 0)) stop("Could not download staged chunks")
  if (!all(file.rename(tmp, output))) stop("Could not publish staged chunks")
  invisible(output)
}

.check_cache_keys <- function(keys) {
  if (any(grepl("^[/\\\\]|(^|[/\\\\])[.][.]([/\\\\]|$)", keys))) {
    stop("Unsafe source chunk path")
  }
  invisible(keys)
}
