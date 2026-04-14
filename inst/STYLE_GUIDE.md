# caladaptR Style Guide

This guide documents the coding and documentation conventions for the `caladaptR` package.

## R Code

- **Pipe operator:** Use the native pipe `|>`, not `%>%`.
- **Package-qualified calls** in package code: `stars::read_mdim()`, not `read_mdim()` after `library(stars)`.
- **Early returns** over nested `if/else` blocks.
- **Internal helpers** start with `.` (e.g. `.setup_s3_env()`).
- **Error handling:** Use `tryCatch()` at I/O boundaries (S3 reads, catalog download). Let internal errors propagate naturally.
- **No Python dependency.** All data access goes through GDAL's `/vsis3/` virtual filesystem via `stars::read_mdim()`.

## Documentation

- **Roxygen2** with markdown enabled (`Roxygen: list(markdown = TRUE)` in DESCRIPTION).
- **Tags:** `@param`, `@return`, `@export`, `@examples`, `@seealso`.
- **Title:** Short, one line, no trailing period.
- **Description:** Second paragraph after a blank `#'` line.
- Backtick-wrap code references in roxygen and vignettes.
- Examples that hit S3 go inside `\dontrun{}`.

## Vignettes

- Written in Quarto (`.qmd`), not Rmarkdown.
- Each vignette opens with a "What you will learn" callout box.
- Progressive structure: concepts first, then worked examples.
- Include interpretation after visualizations -- don't leave plots unexplained.
- Reference the Cal-Adapt Analytics Engine and underlying data sources (Rahimi et al. 2024 for WRF, Pierce et al. for LOCA2).

## Testing

- Use `testthat` edition 3 (`Config/testthat/edition: 3` in DESCRIPTION).
- Test files go in `tests/testthat/` with naming pattern `test-*.R`.
- Tests that hit S3 must use `skip_if_offline()` and `skip_on_cran()`.
- Slow integration tests (multi-variable reads) also use `skip_on_ci()`.

## Version Control

- Keep commits focused -- one logical change per commit.
- Don't commit generated files (`man/`, `NAMESPACE`). Run `devtools::document()` locally.
