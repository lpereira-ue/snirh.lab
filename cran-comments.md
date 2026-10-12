## Submission

This is an update of snirh.lab (0.1.0 -> 0.2.0). See NEWS.md for details. Main changes:

* The station list is now downloaded from the SNIRH website as CSV (package
  'sf' is no longer needed).
* The parameter conversion table was moved out of the package data. It is
  downloaded on first use in each session and cached in
  `tools::R_user_dir("snirh.lab", "cache")` (a single file that is
  overwritten on each update). If the download fails, the cached copy or the
  table bundled in `inst/extdata` is used.
* Bug fixes and unit tests.

## Test environments

* local Windows 10 x64, R 4.6.1 (`R CMD check --as-cran --run-donttest`)
* win-builder: Windows, R-devel (2026-10-10 r90659 ucrt)
* GitHub Actions: macOS (release), Windows (release), Ubuntu (devel, release, oldrel-1)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Notes

* Examples that download data from the SNIRH website (<https://snirh.apambiente.pt>)
  are wrapped in `\donttest{}` and only run when the website is reachable, since
  it can be slow or unreachable from some networks. Tests run offline.

## Reverse dependencies

There are currently no reverse dependencies.
