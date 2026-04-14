#' Cal-Adapt Analytics Engine Data Catalog
#'
#' Functions for loading, caching, and querying the Cal-Adapt
#' intake-ESM catalog. The catalog indexes all Zarr stores on the
#' CADCAT S3 bucket with CMIP6-style metadata fields.

# -- catalog URLs (match climakitae core/paths.py) --
.catalog_csv_url <- "https://cadcat.s3-us-west-2.amazonaws.com/cae-zarr.csv"
.catalog_json_url <- "https://cadcat.s3.amazonaws.com/cae-collection.json"

# package-level cache (loaded once per session)
.catalog_cache <- new.env(parent = emptyenv())


#' Load the Cal-Adapt data catalog
#'
#' Downloads and caches the intake-ESM catalog CSV from CADCAT S3.
#' The catalog is loaded once per R session and reused on subsequent calls.
#'
#' @param refresh logical; if TRUE, force re-download even if cached.
#'        Default FALSE.
#' @return A data.frame with columns: activity_id, institution_id,
#'   source_id, experiment_id, member_id, table_id, variable_id,
#'   grid_label, path
#' @export
#' @examples
#' \dontrun{
#' cat <- ca_catalog()
#' head(cat)
#' }
ca_catalog <- function(refresh = FALSE) {
  if (!refresh && exists("catalog", envir = .catalog_cache)) {
    return(get("catalog", envir = .catalog_cache))
  }

  catalog <- tryCatch(
    utils::read.csv(.catalog_csv_url, stringsAsFactors = FALSE),
    error = function(e) {
      stop("Failed to download Cal-Adapt catalog from ",
           .catalog_csv_url, "\n", e$message,
           call. = FALSE)
    }
  )

  # standardize column names
  names(catalog) <- tolower(names(catalog))

  assign("catalog", catalog, envir = .catalog_cache)
  catalog
}


#' List available climate models
#'
#' @param activity character; "WRF" or "LOCA2" or NULL for both.
#' @return character vector of model names (source_id values)
#' @export
ca_models <- function(activity = NULL) {
  cat <- ca_catalog()
  if (!is.null(activity)) {
    cat <- cat[cat$activity_id == activity, ]
  }
  sort(unique(cat$source_id))
}


#' List available scenarios (SSPs)
#'
#' @param activity character; "WRF" or "LOCA2" or NULL for both.
#' @param model character; filter to a specific GCM. NULL for all.
#' @return character vector of experiment_id values
#' @export
ca_scenarios <- function(activity = NULL, model = NULL) {
  cat <- ca_catalog()
  if (!is.null(activity)) cat <- cat[cat$activity_id == activity, ]
  if (!is.null(model)) cat <- cat[cat$source_id == model, ]
  sort(unique(cat$experiment_id))
}


#' List available variables
#'
#' @param activity character; "WRF" or "LOCA2" or NULL for both.
#' @param timescale character; "1hr", "day", "mon", or NULL for all.
#' @return character vector of variable_id values
#' @export
ca_variables <- function(activity = NULL, timescale = NULL) {
  cat <- ca_catalog()
  if (!is.null(activity)) cat <- cat[cat$activity_id == activity, ]
  if (!is.null(timescale)) cat <- cat[cat$table_id == timescale, ]
  sort(unique(cat$variable_id))
}


#' Search the catalog
#'
#' Filter the Cal-Adapt catalog by any combination of metadata fields.
#' All filters are combined with AND logic.
#'
#' @param activity character; "WRF" or "LOCA2"
#' @param model character; GCM source_id (e.g. "CESM2", "MIROC6")
#' @param scenario character; experiment_id (e.g. "ssp370", "historical")
#' @param variable character; variable_id (e.g. "t2", "prec")
#' @param timescale character; table_id (e.g. "1hr", "day", "mon")
#' @param resolution character; grid_label (e.g. "d01" for 45km,
#'   "d02" for 9km, "d03" for 3km)
#' @return data.frame of matching catalog entries
#' @export
#' @examples
#' \dontrun{
#' # Find all hourly WRF temperature data at 45km
#' ca_search(activity = "WRF", variable = "t2",
#'           timescale = "1hr", resolution = "d01")
#' }
ca_search <- function(activity = NULL, model = NULL, scenario = NULL,
                      variable = NULL, timescale = NULL,
                      resolution = NULL) {
  cat <- ca_catalog()

  if (!is.null(activity))   cat <- cat[cat$activity_id == activity, ]
  if (!is.null(model))      cat <- cat[cat$source_id == model, ]
  if (!is.null(scenario))   cat <- cat[cat$experiment_id == scenario, ]
  if (!is.null(variable))   cat <- cat[cat$variable_id == variable, ]
  if (!is.null(timescale))  cat <- cat[cat$table_id == timescale, ]
  if (!is.null(resolution)) cat <- cat[cat$grid_label == resolution, ]

  if (nrow(cat) == 0) {
    warning("No catalog entries match the search criteria.")
  }
  cat
}


#' Check SIPNET variable coverage for a model/scenario
#'
#' Reports which of the 7 SIPNET-required variables are directly
#' available and which need derivation (e.g. prec from rainc + rainnc).
#'
#' @param model character; GCM name
#' @param scenario character; SSP experiment
#' @param timescale character; temporal resolution. Default "1hr".
#' @param resolution character; grid label. Default "d01".
#' @return A data.frame with columns: variable, available, derivable, note
#' @export
#' @examples
#' \dontrun{
#' ca_check_variables("MPI-ESM1-2-HR", "ssp370")
#' ca_check_variables("CESM2", "ssp370")
#' }
ca_check_variables <- function(model, scenario,
                               timescale = "1hr", resolution = "d01") {
  sipnet_vars <- c("t2", "psfc", "prec", "q2", "swdnb", "u10", "v10")
  precip_components <- c("rainc", "rainnc")

  cat <- ca_catalog()
  model_vars <- cat[cat$source_id == model &
                    cat$experiment_id == scenario &
                    cat$table_id == timescale &
                    cat$grid_label == resolution, "variable_id"]
  model_vars <- unique(model_vars)

  result <- data.frame(
    variable = sipnet_vars,
    available = sipnet_vars %in% model_vars,
    derivable = FALSE,
    note = "",
    stringsAsFactors = FALSE
  )

  # check if prec can be derived from components
  prec_row <- which(result$variable == "prec")
  if (!result$available[prec_row]) {
    has_components <- all(precip_components %in% model_vars)
    result$derivable[prec_row] <- has_components
    if (has_components) {
      result$note[prec_row] <- "derived from rainc + rainnc"
    } else {
      result$note[prec_row] <- "NOT available"
    }
  }

  result
}


#' Get the S3 Zarr path for a specific dataset
#'
#' Convenience wrapper around ca_search() that returns exactly one
#' S3 path. Errors if zero or multiple entries match.
#'
#' @inheritParams ca_search
#' @param member character; member_id for specific ensemble member.
#'   NULL uses the first available.
#' @return character; the S3 path (e.g. "s3://cadcat/wrf/ucla/...")
#' @keywords internal
ca_zarr_path <- function(activity, model, scenario, variable,
                         timescale, resolution, member = NULL) {
  hits <- ca_search(activity = activity, model = model,
                    scenario = scenario, variable = variable,
                    timescale = timescale, resolution = resolution)
  if (!is.null(member)) {
    hits <- hits[hits$member_id == member, ]
  }

  if (nrow(hits) == 0) {
    stop("No catalog entry found for: ",
         paste(activity, model, scenario, variable,
               timescale, resolution, sep = "/"),
         call. = FALSE)
  }
  if (nrow(hits) > 1 && is.null(member)) {
    # take the first member
    hits <- hits[1, , drop = FALSE]
  }

  hits$path[1]
}
