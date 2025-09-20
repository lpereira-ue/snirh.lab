# SNIRH Lab Package - Enhanced Usage Guide

## Overview

The enhanced SNIRH lab package now includes automatic station validation against the SNIRH database for surface water and biota data. This ensures that only valid, active stations are used for data conversion.

## Key Features

### ✅ Station Validation

-   Automatic validation against SNIRH database
-   Checks for station existence and active status
-   Works for surface water and biota matrices
-   Requires internet connection

### ✅ Enhanced Error Handling

-   Clear, informative error messages
-   Step-by-step progress tracking
-   Detailed validation reports

### ✅ Utility Functions

-   `get_snirh_stations()` - Download station information
-   `check_station_status()` - Validate specific stations
-   `list_snirh_parameters()` - Browse available parameters

## Installation Requirements

``` r
# Core requirements
install.packages(c("data.table", "cli"))

# For station validation
install.packages("sf")

# For better internet connectivity checks (optional)
install.packages("curl")
```

## Basic Usage

### 1. Standard Data Conversion

``` r
library(snirh.lab)

# Prepare your data
lab_data <- data.table(
  snirh_entity = "LAB001",
  station_name = "Rio Douro - Crestuma",
  station_id = "01F/01",  # Must be valid SNIRH station
  sampling_date = as.POSIXct("2024-01-15 10:30:00"),
  parameter = "pH - Campo",
  unit = "Escala Sorensen",
  value = "7.2"
)

# Convert with automatic station validation
result <- convert_to_snirh(lab_data, "surface.water")
```

### 2. Check Station Status Before Conversion

``` r
# Check if your stations are valid and active
my_stations <- c("01F/01", "25G/07", "16H/03")
station_check <- check_station_status(my_stations, "surface.water")
print(station_check)

# Only proceed with active stations
active_stations <- station_check[active == TRUE, station_id]
filtered_data <- lab_data[station_id %in% active_stations]
```

### 3. Browse Available Stations

``` r
# Get all active surface water stations
active_stations <- get_snirh_stations("surface.water", active_only = TRUE)
print(paste("Available stations:", nrow(active_stations)))

# Find stations in your region (example with spatial filtering)
# Note: This requires additional spatial operations
if (requireNamespace("sf", quietly = TRUE)) {
  library(sf)
  stations_sf <- get_snirh_stations("surface.water")
  # Add your spatial filtering logic here
}
```

### 4. Explore Available Parameters

``` r
# List all water quality parameters
water_params <- list_snirh_parameters("water")
print(head(water_params))

# Get detailed conversion information
detailed_params <- list_snirh_parameters("water", include_conversion_info = TRUE)
print(detailed_params[1:5, .(param_lab, unit_lab, param_snirh, unit_snirh, factor)])
```

## Error Handling Examples

### Invalid Station ID

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

### Inactive Station

``` r
# If a station exists but is inactive, you'll get:
# "Station(s) not active in SNIRH database: STATION_ID (EXTINTA)"
# "Only stations with status 'ATIVA' can receive data"
```

### No Internet Connection

``` r
# Will produce: "Internet connection required for station validation"
# Solution: Check connection or use validate_stations = FALSE
result <- convert_to_snirh(lab_data, "surface.water", validate_stations = FALSE)
```

## Advanced Configuration

### Skip Station Validation

``` r
# For testing or when working offline
result <- convert_to_snirh(lab_data, "surface.water", validate_stations = FALSE)
```

### Custom Timeout

``` r
# For slow connections
result <- convert_to_snirh(lab_data, "surface.water", timeout = 60)
```

### Batch Processing

``` r
# Process multiple files with error handling
process_lab_files <- function(file_paths) {
  results <- list()

  for (file_path in file_paths) {
    tryCatch({
      # Read your data
      lab_data <- read_your_data_function(file_path)

      # Check stations first
      unique_stations <- unique(lab_data$station_id)
      station_status <- check_station_status(unique_stations, "surface.water")

      # Filter to active stations only
      active_stations <- station_status[active == TRUE, station_id]
      filtered_data <- lab_data[station_id %in% active_stations]

      if (nrow(filtered_data) > 0) {
        # Convert filtered data
        result <- convert_to_snirh(filtered_data, "surface.water")
        results[[file_path]] <- result

        cat("✅ Successfully processed:", file_path, "\n")
      } else {
        cat("⚠️  No active stations in:", file_path, "\n")
      }

    }, error = function(e) {
      cat("❌ Error processing:", file_path, "-", e$message, "\n")
    })
  }

  return(results)
}
```

## Troubleshooting

### Common Issues

1.  **Station validation fails**

-   Check internet connection
-   Verify station IDs are correct
-   Check if stations are active in SNIRH

2.  **sf package not available**

-   Install with: `install.packages("sf")`
-   May require system dependencies on Linux

3.  **Slow downloads**

-   Increase timeout parameter
-   Check network connection
-   Try during off-peak hours

### Getting Help

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

1.  **Always validate stations first** for production workflows
2.  **Cache station data** for batch processing to avoid repeated downloads
3.  **Handle errors gracefully** in automated systems
4.  **Keep the package updated** for latest SNIRH compatibility
5.  **Test with small datasets** before processing large files

## Data Quality Checks

The package now performs comprehensive validation:

-   ✅ Column structure and naming
-   ✅ Station existence and status
-   ✅ Duplicate detection
-   ✅ Parameter conversion availability
-   ✅ Value format validation
-   ✅ Unit conversion accuracy
-   ✅ Output format compliance

This ensures high-quality data submission to SNIRH with minimal manual intervention.