# Bounded access benchmarks

Run `inventory-access.R` in separate R processes for `staged` and `remote` modes:

```sh
Rscript inventory-access.R staged points.csv output SOURCE_KEY
CPL_CURL_VERBOSE=YES Rscript inventory-access.R remote points.csv output SOURCE_KEY 2>remote-curl.log
```

First place a representative source file at `output/grid-source.nc`; it supplies
geometry and file size. The staged comparison still makes a fresh full download.
Compare the saved `*-values.rds` objects exactly. JSON results contain elapsed
time, site/cell counts, source size, variables, and peak process memory on Linux.
The benchmark's remote method uses GDAL/HDF5; package historical retrieval uses
download-and-extract. No special remote NetCDF build is required by the package.

On 2026-10-20, Geo's existing GDAL 3.12.3 environment produced these results for
121 distributed native cells and nine fields in the UCLA d03 source file
`downscaled_products/reanalysis/era5/hourly/2022/d03/auxhist_d01_2023-02-25_07:00:00`:

| Method | Elapsed seconds | Transferred bytes | Peak process RSS (kB) |
| --- | ---: | ---: | ---: |
| Full download and extraction | 1.883 | 32,016,814 | 212,016 |
| GDAL HTTP range extraction | 3.387 | 3,080,192 | 195,256 |

All extracted values matched exactly. Range bytes sum GET-response Content-Length
headers, excluding HEAD responses. This comparison favors download-and-extract
for elapsed time; callers can retain those files or discard owned downloads.

A separate CESM2 SSP370 d03 temperature check used two sites in one cell across
2024-02-28 23:00 through 2024-03-01 01:00. Remote retrieval took 18.918 s,
staging took 10.186 s, and the staged read took 0.472 s (80,560,295 cache bytes).
Values and timestamps matched exactly, with no February 29 records inserted.

These are bounded, single-file/window measurements. They do not establish
full-year runtime, output throughput, or storage costs for larger site sets.
