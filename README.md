# caladaptaer

Native R access to the [Cal-Adapt Analytics Engine](https://analytics.cal-adapt.org/),
California's repository of downscaled CMIP6 climate projections.

## What it does

`caladaptaer` reads WRF dynamically downscaled (hourly, 3/9/45 km) and
LOCA2-Hybrid statistically downscaled (daily, 3 km) climate data directly from
AWS S3 Zarr stores and public UCLA historical WRF files. It does not require
Python, an API key, or authentication.

## Quick start

**Installation**

```r
# from GitHub
remotes::install_github("lebauerapproach/caladaptaer")
```

**Example**

```r
library(caladaptaer)

# What WRF-downscaled climate models are available?
cae_models(activity = "WRF")

# What variables are available for WRF on a 1hr timescale?
cae_variables(activity = "WRF", timescale = "1hr")

# Read hourly temperature, 45km grid, one day
t2 <- cae_fetch("t2", model = "CESM2", scenario = "ssp370",
               n_timesteps = 24)

# Extract time series at a point
ts <- cae_fetch_point("t2", model = "CESM2", scenario = "ssp370",
                     lon = -119.77, lat = 36.75,
                     n_timesteps = 24)
plot(ts$time, ts$value - 273.15, type = "l",
     ylab = "Temperature (C)", xlab = "Time")
```

## Available data

| Dataset      | Resolution    | Temporal | GCMs      | SSPs                                       | Period    |
| ------------ | ------------- | -------- | --------- | ------------------------------------------ | --------- |
| WRF CMIP6    | 45 / 9 / 3 km | Hourly   | 8 models  | SSP3-7.0 (all); SSP2-4.5, SSP5-8.5 (CESM2) | 1980-2100 |
| WRF-ERA5 | 45 / 9 / 3 km | Hourly | One reanalysis-driven realization | `reanalysis` | Query source coverage |
| LOCA2-Hybrid | 3 km          | Daily    | 15 models | SSP2-4.5, SSP3-7.0, SSP5-8.5               | 1950-2100 |

WRF data are produced by the UCLA Center for Climate Science (Rahimi et al.
2024). LOCA2-Hybrid data are produced by Scripps/UCSD (Pierce et al.). Both
products are hosted on AWS S3 by the Cal-Adapt Analytics Engine.

## Requirements

- R >= 4.1
- R-linked GDAL >= 3.9 for remote Zarr reads; check with `sf::sf_extSoftVersion()`
- Internet access to AWS S3 (us-west-2 region)

## How it works

1. Downloads the [CADCAT intake-ESM catalog](https://cadcat.s3-us-west-2.amazonaws.com/cae-zarr.csv) (a CSV table indexing all Zarr stores)
2. Filters to the requested model / scenario / variable / resolution
3. Reads the Zarr store from S3 via GDAL's `/vsis3/` virtual filesystem using the GDAL multidimensional API
4. Returns a `stars` object with full spatial and temporal metadata

## License

BSD 3-Clause

## References

- Rahimi, S. et al. (2024). An overview of the Western United States Dynamically Downscaled Dataset (WUS-D3). *Geoscientific Model Development*, 17, 2265-2286. https://doi.org/10.5194/gmd-17-2265-2024
- Cal-Adapt Analytics Engine: https://analytics.cal-adapt.org/
- CADCAT on AWS: https://registry.opendata.aws/caladapt-coproduced-climate-data/

## Source timestamps

Date selections follow stored time coordinates, including omitted leap days.
No missing hours or leap days are inserted. Outputs produced with arithmetic
time indexing for no-leap WRF sources must be regenerated: their values may
have been shifted relative to their date labels (see [issue #22](https://github.com/lebauerapproach/caladaptaer/issues/22)).


## Historical data, mapping, and local reuse

ERA5-driven WRF uses `model = "ERA5"`, `scenario = "reanalysis"`:

```r
historical <- cae_fetch_point("t2", "ERA5", "reanalysis",
                              lon = -121.5, lat = 38.5,
                              resolution = "d03", start_time = "2016-06-01",
                              n_timesteps = 3)
```

Use `cae_search()` and `cae_check_variables()` to check catalog combinations.
Availability differs by model, scenario, variable, and grid: a scenario at 45 km
is not necessarily available at 3 km. The
[official data documentation](https://analytics.cal-adapt.org/data-tools/data-documentation/climate-model-sims.html)
and [catalog](https://cadcat.s3-us-west-2.amazonaws.com/cae-zarr.csv) remain authoritative.

```r
sites <- data.frame(site_id = c("a", "b"), lon = -121.5, lat = 38.5)
grid <- cae_reference_grid(model = "ERA5", scenario = "reanalysis", resolution = "d03")
mapping <- cae_grid_cells(sites, grid)
# Source-grid indices identify cells; cell_lon/cell_lat are their WGS84 centers.
store <- cae_stage_zarr("t2", "CESM2", "ssp370", sites, "source-cache",
                        "2024-06-01", "2024-06-01 02:00:00", resolution = "d03")
values <- cae_fetch_points("t2", "CESM2", "ssp370", sites, resolution = "d03",
                           start_time = "2024-06-01", n_timesteps = 3, source = store)
```

WRF point requests use bounded windows. Sites sharing a native cell reuse its
weather while retaining their identifiers. Cell IDs are meaningful within the
identified source grid; verify geometry before sharing mappings across products.
Staging preserves source chunks and metadata, reuses existing chunks, and makes
new files available only after their downloads finish.

For historical periods beyond cataloged Zarr coverage, `cae_inventory_files()`
lists the separate UCLA hourly files. `cae_build_inventory_drivers()` downloads
and exports those files, retaining sources by default. `retain_source = FALSE`
discards only its own temporary downloads after all requested cells are read;
it never removes pre-existing cached sources. A full-year listing must include
the preceding hour for precipitation. Missing coverage stops the affected year.
See the [driver guide](vignettes/met_drivers.qmd) for examples and storage behavior.

`cae_build_met_drivers()` exports cataloged WRF data by site and year, with
bounded cell batches, optional chunk caching, and validation before reusing
outputs. Exports preserve missing values and native calendars, record CF/ACDD
provenance, and document unit conversions. They do not provide model-specific
calendar conversion or gap filling.
