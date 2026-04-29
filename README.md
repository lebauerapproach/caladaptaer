# caladaptaer

Native R access to the [Cal-Adapt Analytics Engine](https://analytics.cal-adapt.org/) -- California's repository of downscaled CMIP6 climate projections.

## What it does

caladaptaer reads WRF dynamically downscaled (hourly, 3/9/45 km) and LOCA2-Hybrid statistically downscaled (daily, 3 km) climate data directly from AWS S3 Zarr stores. No Python dependency, no API key, no authentication.

## Quick start

```r
library(caladaptaer)

# What's available?
cae_models(activity = "WRF")
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

| Dataset | Resolution | Temporal | GCMs | SSPs | Period |
|---------|-----------|----------|------|------|--------|
| WRF CMIP6 | 45 / 9 / 3 km | Hourly | 8 models | SSP3-7.0 (all); SSP2-4.5, SSP5-8.5 (CESM2) | 1980-2100 |
| LOCA2-Hybrid | 3 km | Daily | 15 models | SSP2-4.5, SSP3-7.0, SSP5-8.5 | 1950-2100 |

Data produced by UCLA Center for Climate Science (WRF, Rahimi et al. 2024) and Scripps/UCSD (LOCA2, Pierce et al.). Hosted on AWS S3 by the Cal-Adapt Analytics Engine.

## Requirements

- R >= 4.1
- GDAL >= 3.4 (for Zarr driver -- check with `sf::sf_extSoftVersion()`)
- Internet access to AWS S3 (us-west-2 region)

## Installation

```r
# from GitHub
remotes::install_github("lebauerapproach/caladaptaer")
```

## How it works

1. Downloads the [CADCAT intake-ESM catalog](https://cadcat.s3-us-west-2.amazonaws.com/cae-zarr.csv) (a CSV table indexing all Zarr stores)
2. Filters to the requested model / scenario / variable / resolution
3. Reads the Zarr store from S3 via GDAL's `/vsis3/` virtual filesystem using `stars::read_mdim()`
4. Returns a `stars` object with full spatial and temporal metadata

No Python, no climakitae, no dask. Just GDAL doing what GDAL does.

## Related projects

- [climakitae](https://github.com/cal-adapt/climakitae) -- official Python package for the Analytics Engine
- [caladaptr](https://github.com/ucanr-igis/caladaptr) -- R package for the legacy CMIP5 Cal-Adapt API (different data, different backend)


## License

BSD 3-Clause

## References

- Rahimi, S. et al. (2024). An overview of the Western United States Dynamically Downscaled Dataset (WUS-D3). *Geoscientific Model Development*, 17, 2265-2286. https://doi.org/10.5194/gmd-17-2265-2024
- Cal-Adapt Analytics Engine: https://analytics.cal-adapt.org/
- CADCAT on AWS: https://registry.opendata.aws/caladapt-coproduced-climate-data/
