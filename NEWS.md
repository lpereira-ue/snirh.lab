# snirh.lab 0.1.0.9000

### Breaking changes
* The `parameters` dataset was removed. The parameter conversion table is now
  maintained outside the package code (`inst/extdata/parameters.csv` in the
  GitHub repository) and downloaded automatically the first time it is needed
  in each R session, so it can be updated without a new package release. If
  the download fails, the last downloaded copy (stored in
  `tools::R_user_dir("snirh.lab", "cache")`) or the version bundled with the
  package is used. Use `list_snirh_parameters()` to browse it.
* `get_snirh_stations()` now returns all the station metadata available in
  SNIRH (name, basin, coordinates, entity, location, dates, purpose, ...)
  besides `station_id` and `status`.

### New features
* New `update_snirh_parameters()` to force an update of the parameter table.
* Custom parameter tables can be used with
  `options(snirh.lab.parameters = "<path or URL>")`.
* The station list is now downloaded from the SNIRH website (CSV), which is
  much faster than the previous WFS service, and is cached for the R session.
  New argument `refresh` in `get_snirh_stations()`.
* Package `sf` is no longer needed.
* `sampling_date` can also be a `Date` or a character date/time
  (e.g. `"2024-01-15 10:30"` or `"15/01/2024 10:30"`).
* `list_snirh_parameters()` accepts `sample_type = "sediment"`.
* Added unit tests.

### Bug fixes
* Fixed an issue where pH temperature data were not correctly extracted when present.
* Values are no longer written in scientific notation (e.g. `<5e-04` is now
  `<0.0005`).
* Non-numeric values (e.g. `"n.d."`) now raise an error instead of being
  silently written as `NA`.
* Different laboratory parameters mapping to the same SNIRH symbol for the same
  station and date (e.g. `pH - Campo` and `pH - Campo (ARHC)`) now raise an
  error instead of being silently replaced by a count.
* Parameters without unit in the parameter table (e.g. `Turvação (LSA)`) can now be
  converted; previously only the IGA was accepted with an empty unit, and it
  was exempted from all missing value checks.
* pH temperature extraction accepts both `º` and `°` (e.g. `7.2 a 20 °C`)
  and rejects temperatures not in Celsius; previously some values were
  silently converted to `NA`. It now uses the parameters of the matrix being
  converted.
* Character sampling dates were sorted and written as text; they are now
  parsed and validated.
* Station validation now runs after removing empty rows and trimming
  whitespace, and stations without status are reported as inactive.
* `check_station_status()` trims station IDs and reports `active = FALSE`
  (instead of `NA`) for stations found without status.
* Whitespace is now trimmed from `parameter`, `unit` and `snirh_entity`.
* `.onLoad()` no longer prints startup messages, and unused imports were
  removed (`data.table::let()` required a newer data.table than declared).

### Changes
* `convert_to_snirh()` now requires `validate_stations` to be a single TRUE/FALSE value (default: `TRUE`; previously `NULL` in the signature).
* Updated pkgdown site configuration to include a **Home** link in the navbar and minor navigation polish.
* Refactored code by moving functions from `convert_to_snirh.R` to separate files for better organization.
* Removed timeout parameters from functions.
* Fixed non-ASCII character encoding issues for CRAN compatibility.
* Minor internal improvements and documentation updates.


# snirh.lab 0.1.0

* Initial CRAN release.
