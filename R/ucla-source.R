#' UCLA WRF source-bucket listing helpers
#'
#' Source listings are independent of the CADCAT Zarr catalog.
#'
#' @keywords internal

.ucla_wrf_bucket <- "wrf-cmip6-noversioning"
.ucla_wrf_era5_hourly_prefix <- "downscaled_products/reanalysis/era5/hourly"
.ucla_wrf_domains <- c("d01", "d02", "d03")


#' Build a UCLA WRF source-bucket prefix
#' @keywords internal
.ucla_wrf_source_prefix <- function(year = NULL, domain = NULL,
                                    product = "era5",
                                    timescale = "hourly") {
  parts <- c("downscaled_products", "reanalysis", product, timescale)
  if (!is.null(year)) parts <- c(parts, as.character(year))
  if (!is.null(domain)) parts <- c(parts, domain)
  paste0(paste(parts, collapse = "/"), "/")
}


#' Build a UCLA WRF source-bucket object key
#' @keywords internal
.ucla_wrf_source_key <- function(year, domain, timestamp,
                                 product = "era5",
                                 timescale = "hourly",
                                 file_domain = "d01",
                                 stream = "auxhist") {
  prefix <- .ucla_wrf_source_prefix(
    year = year, domain = domain,
    product = product, timescale = timescale
  )
  paste0(prefix, stream, "_", file_domain, "_", timestamp)
}


#' Convert a UCLA WRF source-bucket key to an s3:// URI
#' @keywords internal
.ucla_wrf_source_uri <- function(key) {
  paste0("s3://", .ucla_wrf_bucket, "/", key)
}


#' Build the S3 listing URL for a public UCLA WRF source prefix
#' @keywords internal
.ucla_wrf_list_url <- function(prefix, delimiter = NULL, max_keys = 1000,
                               continuation_token = NULL) {
  query <- paste0(
    "list-type=2&prefix=", utils::URLencode(prefix, reserved = TRUE),
    "&max-keys=", as.integer(max_keys)
  )
  if (!is.null(delimiter)) {
    query <- paste0(query, "&delimiter=", utils::URLencode(delimiter, reserved = TRUE))
  }
  if (!is.null(continuation_token)) {
    query <- paste0(
      query, "&continuation-token=",
      utils::URLencode(continuation_token, reserved = TRUE)
    )
  }
  paste0("https://", .ucla_wrf_bucket, ".s3.us-west-2.amazonaws.com/?", query)
}


#' Parse an S3 ListObjectsV2 XML response
#' @keywords internal
.parse_s3_list_xml <- function(xml) {
  if (length(xml) > 1) xml <- paste(xml, collapse = "")

  text_values <- function(tag) {
    pattern <- paste0("<", tag, ">([^<]*)</", tag, ">")
    hits <- regmatches(xml, gregexpr(pattern, xml))[[1]]
    if (length(hits) == 0) return(character())
    sub(pattern, "\\1", hits)
  }

  keys <- text_values("Key")
  modified <- text_values("LastModified")
  sizes <- text_values("Size")

  contents <- data.frame(
    key = keys,
    last_modified = modified[seq_along(keys)],
    size = suppressWarnings(as.numeric(sizes[seq_along(keys)])),
    stringsAsFactors = FALSE
  )

  list(
    contents = contents,
    common_prefixes = text_values("Prefix"),
    is_truncated = identical(text_values("IsTruncated")[1], "true"),
    next_continuation_token = text_values("NextContinuationToken")[1]
  )
}


#' List one UCLA WRF source-bucket prefix
#' @keywords internal
.ucla_wrf_list_prefix <- function(prefix, delimiter = NULL, max_keys = 1000,
                                  paginate = TRUE) {
  pages <- list()
  token <- NULL

  repeat {
    url <- .ucla_wrf_list_url(
      prefix = prefix, delimiter = delimiter,
      max_keys = max_keys, continuation_token = token
    )
    xml <- paste(readLines(url, warn = FALSE), collapse = "")
    page <- .parse_s3_list_xml(xml)
    pages[[length(pages) + 1L]] <- page

    if (!isTRUE(paginate) ||
        !isTRUE(page$is_truncated) ||
        is.na(page$next_continuation_token) ||
        page$next_continuation_token == "") {
      break
    }
    token <- page$next_continuation_token
  }

  contents <- do.call(
    rbind,
    lapply(pages, function(page) page$contents)
  )
  if (is.null(contents)) {
    contents <- data.frame(
      key = character(), last_modified = character(), size = numeric(),
      stringsAsFactors = FALSE
    )
  }

  list(
    contents = contents,
    common_prefixes = unique(unlist(lapply(pages, function(page) page$common_prefixes))),
    is_truncated = FALSE,
    next_continuation_token = NA_character_
  )
}
