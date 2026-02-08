# snirh.lab <img src="man/figures/logo.png" alt="snirh.lab logo" align="right" height="139"/>

<!-- badges: start -->

[![CRAN status](https://www.r-pkg.org/badges/version/snirh.lab)](https://CRAN.R-project.org/package=snirh.lab) [![R-CMD-check](https://github.com/lpereira-ue/snirh.lab/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lpereira-ue/snirh.lab/actions/workflows/R-CMD-check.yaml) [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) [![CRAN downloads](https://cranlogs.r-pkg.org/badges/last-month/snirh.lab)](https://cran.r-project.org/package=snirh.lab)

<!-- badges: end -->

## Overview

Convert laboratory data into the official SNIRH (Portuguese Information System for Water Resources) format - https://snirh.apambiente.pt/. The **snirh.lab** package validates monitoring stations, standardizes parameters and units, and generates compliant files for submission to SNIRH.

## Key Features

-   ✅ **Automatic validation** against SNIRH database
-   ✅ **Station status checking** (existence and active status)
-   ✅ **Surface water and biota matrices** support
-   ✅ **Clear, informative error messages**
-   ✅ **Step-by-step progress tracking**
-   ✅ **Detailed validation reports**

## Installation

### From CRAN

``` r
install.packages("snirh.lab")
```

### Development version from GitHub

``` r
# Install pak if needed
install.packages("pak")

# Then install from GitHub
pak::pak("lpereira-ue/snirh.lab")
```

### Dependencies

``` r
# Core requirements
install.packages(c("data.table", "cli"))

# For station validation
install.packages("sf")

# For better internet connectivity checks (optional)
install.packages("curl")
```

## Main Functions

### Core Functions

-   `convert_to_snirh()` - Convert laboratory data to SNIRH format
-   `get_snirh_stations()` - Download station information
-   `check_station_status()` - Validate specific stations
-   `list_snirh_parameters()` - Browse available parameters

## Quick Start

### Basic Conversion

``` r
library(snirh.lab)
library(data.table)

# Prepare your data
lab_data <- data.table(
  snirh_entity = "LAB001",
  station_name = "Rio Douro - Crestuma",
  station_id = "01F/01", # Must be valid SNIRH station
  sampling_date = as.POSIXct("2024-01-15 10:30:00"),
  parameter = "pH - Campo",
  unit = "Escala Sorensen",
  value = "7.2"
)

# Convert with automatic station validation
result <- convert_to_snirh(lab_data, "surface.water")
print(result)
```

### Station Validation

``` r
# Check if your stations are valid and active
my_stations <- c("01F/01", "25G/07", "16H/03")
station_check <- check_station_status(my_stations, "surface.water")
print(station_check)

# Only proceed with active stations
active_stations <- station_check[active == TRUE, station_id]
filtered_data <- lab_data[station_id %in% active_stations]
```

### Browse Available Stations

``` r
# Get all active surface water stations
active_stations <- get_snirh_stations("surface.water", active_only = TRUE)
print(paste("Available stations:", nrow(active_stations)))
```

### List Parameters

``` r
# List all water quality parameters
water_params <- list_snirh_parameters("water")
print(head(water_params))

# Get detailed conversion information
detailed_params <- list_snirh_parameters("water", include_conversion_info = TRUE)
print(detailed_params[1:5, .(param_lab, unit_lab, param_snirh, unit_snirh, factor)])
```

## Error Handling

### Invalid Station Example

``` r
# This will fail with clear error message
bad_data <- data.table(
  snirh_entity = "LAB001",
  station_name = "Invalid Station",
  station_id = "INVALID_ID",
  sampling_date = as.POSIXct("2024-01-15 10:30:00"),
  parameter = "pH - Campo",
  unit = "Escala Sorensen",
  value = "7.2"
)

# Will produce error: "Station ID(s) not found in SNIRH database: INVALID_ID"
try(convert_to_snirh(bad_data, "surface.water"))
```

**Note:** If a station exists but is inactive, you'll get: - "Station(s) not active in SNIRH database: STATION_ID (STATUS)" - "Only stations with status 'ATIVA' can receive data"

## Working Offline

### No Internet Connection

``` r
# Will produce: "Internet connection required for station validation"
# Solution: Check connection or use validate_stations = FALSE
result <- convert_to_snirh(lab_data, "surface.water", validate_stations = FALSE)
```

## Troubleshooting

### Station validation fails

-   Check internet connection
-   Verify station IDs are correct
-   Check if stations are active in SNIRH

### `sf` package not available

-   Install with: `install.packages("sf")`
-   May require system dependencies on Linux

## Getting Help

``` r
# View package help
help(package = "snirh.lab")

# Function-specific help
?convert_to_snirh
?get_snirh_stations
?check_station_status

# List all available parameters
list_snirh_parameters("all")
```

## Best Practices

-   ✅ Always validate stations first for production workflows
-   ✅ Cache station data for batch processing to avoid repeated downloads
-   ✅ Handle errors gracefully in automated systems
-   ✅ Keep the package updated for latest SNIRH compatibility
-   ✅ Test with small datasets before processing large files

## Validation Coverage

The package performs comprehensive validation:

-   ✅ Column structure and naming
-   ✅ Station existence and status
-   ✅ Duplicate detection
-   ✅ Parameter conversion availability
-   ✅ Value format validation
-   ✅ Unit conversion accuracy
-   ✅ Output format compliance

This ensures high-quality data submission to SNIRH with minimal manual intervention.

## Citation

To cite snirh.lab in publications, please use:

``` r
citation("snirh.lab")
```