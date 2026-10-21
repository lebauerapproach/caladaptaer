# Provenance is a small list carried as an attribute on retrieved objects.
.source_provenance <- function(path, variable, meta, indices) {
  original <- path
  manifest <- file.path(path, ".caladaptaer-source.json")
  if (dir.exists(path) && file.exists(manifest)) {
    original <- jsonlite::fromJSON(manifest)$source
  }
  list(source = original, catalog = if (startsWith(original, "s3://cadcat/")) .catalog_csv_url else "not applicable",
       variable = variable, calendar = meta$calendar, time_units = meta$time_units,
       time_values = meta$time_values[indices], times = meta$times[indices],
       history = "Selected source coordinates and values; no interpolation or gap filling")
}

.request_provenance <- function(x, model, scenario, resolution, start, end) {
  p <- attr(x, "cae_provenance")
  if (is.null(p)) return(x)
  p$model <- model
  p$scenario <- scenario
  p$resolution <- resolution
  member <- regmatches(p$source, regexpr("r[0-9]+i[0-9]+p[0-9]+f[0-9]+", p$source))
  p$member <- if (length(member) && any(nzchar(member))) unique(member[nzchar(member)]) else "unknown"
  p$requested_start <- if (is.null(start)) "source start" else as.character(start)
  p$requested_end <- if (is.null(end)) "source end" else as.character(end)
  attr(x, "cae_provenance") <- p
  x
}

.combine_provenance <- function(objects) {
  ps <- lapply(objects, function(x) attr(x, "cae_provenance"))
  ps <- Filter(Negate(is.null), ps)
  if (!length(ps)) return(NULL)
  for (name in c("model", "scenario", "member", "resolution", "calendar")) {
    values <- unique(unlist(lapply(ps, `[[`, name), use.names = FALSE))
    values <- values[!is.na(values) & nzchar(values) & values != "unknown"]
    if (length(values) > 1L) stop("Conflicting source provenance: ", name)
  }
  out <- ps[[1]]
  for (name in c("source", "variable", "history")) {
    out[[name]] <- unique(unlist(lapply(ps, `[[`, name), use.names = FALSE))
  }
  out
}

.export_time <- function(times, provenance = NULL) {
  if (!is.null(provenance) && identical(as.numeric(provenance$times), as.numeric(times))) {
    return(list(values = provenance$time_values, units = provenance$time_units,
                calendar = provenance$calendar))
  }
  if (!is.null(provenance)) stop("Export timestamps differ from source provenance")
  list(values = as.numeric(times), units = "seconds since 1970-01-01 00:00:00", calendar = "standard")
}

.write_provenance <- function(nc, provenance, times, processing) {
  field <- function(name) {
    value <- provenance[[name]]
    if (is.null(value) || !length(value)) "unknown" else paste(unique(value), collapse = "\n")
  }
  ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.8, ACDD-1.3")
  for (name in c("source", "catalog", "model", "scenario", "member", "variable", "resolution",
                 "requested_start", "requested_end")) {
    ncdf4::ncatt_put(nc, 0, name, field(name))
  }
  ncdf4::ncatt_put(nc, 0, "time_coverage_start", format(min(times), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  ncdf4::ncatt_put(nc, 0, "time_coverage_end", format(max(times), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  encoding <- .export_time(times, provenance)
  ncdf4::ncatt_put(nc, "time", "calendar", encoding$calendar)
  ncdf4::ncatt_put(nc, 0, "source_calendar", if (is.null(provenance)) "unknown" else field("calendar"))
  ncdf4::ncatt_put(nc, 0, "source_time_units", if (is.null(provenance)) "unknown" else field("time_units"))
  ncdf4::ncatt_put(nc, 0, "history", paste(format(Sys.time(), tz = "UTC", usetz = TRUE),
    paste(c(provenance$history, processing), collapse = "; ")))
}
