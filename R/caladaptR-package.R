#' @keywords internal
"_PACKAGE"

#' caladaptR: Access Cal-Adapt Climate Data from R
#'
#' Native R access to the Cal-Adapt Analytics Engine, California's
#' repository of downscaled CMIP6 climate projections. Reads WRF
#' dynamically downscaled and LOCA2-Hybrid statistically downscaled
#' data directly from AWS S3 Zarr stores via GDAL.
#'
#' @section Catalog functions:
#' \itemize{
#'   \item \code{\link{ca_catalog}} -- load the data catalog
#'   \item \code{\link{ca_models}} -- list available GCMs
#'   \item \code{\link{ca_scenarios}} -- list available SSPs
#'   \item \code{\link{ca_variables}} -- list available variables
#'   \item \code{\link{ca_search}} -- search the catalog
#' }
#'
#' @section Data access functions:
#' \itemize{
#'   \item \code{\link{ca_fetch}} -- fetch gridded data as stars object
#'   \item \code{\link{ca_fetch_point}} -- extract time series at a point
#' }
#'
#' @section Export functions:
#' \itemize{
#'   \item \code{\link{ca_write_cf_netcdf}} -- write CF-standard NetCDF
#' }
#'
#' @name caladaptR-package
NULL
