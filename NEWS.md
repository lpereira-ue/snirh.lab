# snirh.lab 0.2.0

### Changes
* `convert_to_snirh()` now requires `validate_stations` to be a single TRUE/FALSE value (default: `TRUE`; previously `NULL` in the signature).
* Updated pkgdown site configuration to include a **Home** link in the navbar and minor navigation polish.
* Refactored code by moving functions from `convert_to_snirh.R` to separate files for better organization.
* Station metadata is now retrieved directly from the SNIAmb WFS service instead of downloading shapefiles.
* Removed timeout parameters from functions.
* Fixed non-ASCII character encoding issues for CRAN compatibility.
* Minor internal improvements and documentation updates.

### Bug fixes
* Fixed an issue where pH temperature data were not correctly extracted when present.  

# snirh.lab 0.1.0

* Initial CRAN release.
