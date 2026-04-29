#' @keywords internal
"_PACKAGE"

#' caladaptaer: Access Cal-Adapt Climate Data from R
#'
#' Native R access to the Cal-Adapt Analytics Engine, California's
#' repository of downscaled CMIP6 climate projections. Reads WRF
#' dynamically downscaled and LOCA2-Hybrid statistically downscaled
#' data directly from AWS S3 Zarr stores via GDAL.
#'
#' @section Catalog functions:
#' \itemize{
#'   \item \code{\link{cae_catalog}} -- load the data catalog
#'   \item \code{\link{cae_models}} -- list available GCMs
#'   \item \code{\link{cae_scenarios}} -- list available SSPs
#'   \item \code{\link{cae_variables}} -- list available variables
#'   \item \code{\link{cae_search}} -- search the catalog
#' }
#'
#' @section Data access functions:
#' \itemize{
#'   \item \code{\link{cae_fetch}} -- fetch gridded data as stars object
#'   \item \code{\link{cae_fetch_point}} -- extract time series at a point
#' }
#'
#' @section Export functions:
#' \itemize{
#'   \item \code{\link{cae_write_cf_netcdf}} -- write CF-standard NetCDF
#' }
#'
#' @name caladaptaer-package
NULL
