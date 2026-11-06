# NANNO 1.1.0

* Fixed NAMESPACE corruption caused by a free-text `@export` tag; `NANNO_fit()` is now properly exported and spurious exports were removed.
* Fixed `rm.na` typo in `two_part_figure_with_obs()` that caused `max()` to error on every call.
* Fixed `NANNO_test()` failing on the non-existent `isoNO2` column.
* `NANNO_fit_stats()` now matches simulation rows by time value instead of using observation distances as row indices.
* Missing bounds values are now filled with the mean of the same bound row (lower, upper, or initial) rather than the mean across all rows of a column.
* Example data moved to `inst/extdata/` and referenced via `system.file()`; `NANNO_fit()` gained `output_dir`, `init_bound_tol`, and `delta_bound_tol` arguments and returns a result list.
* Speciation, isotope-standard, and fractionation constants centralised in `R/constants.R`.
* Figure functions refactored onto a shared builder; hardcoded axis limits replaced with data-derived ranges.
* Removed dead code (unused `aniup`/`kniup` parameters, commented-out NO2 path) and the unused `scale.par` argument.
* Switched `plyr` and `reshape2` out for base R and `tidyr::pivot_longer()`, updated ggplot2 usage (`linewidth`), replaced `subset()` with base subsetting, and regenerated documentation with roxygen2 7.
