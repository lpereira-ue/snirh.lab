# Package initialization and global variables ----

# Suppress R CMD CHECK notes for data.table variables and package globals
# These variables are created dynamically by data.table operations and
# are not visible to R CMD CHECK static analysis

# Data manipulation variables (data.table columns and operations)
utils::globalVariables(c(
  # Core data columns
  "station_name", "station_id", "parameter", "unit", "value", "sampling_date",
  "snirh_entity", "std", "flag", "station", "index",

  # Processed/derived columns
  "value_final", "value_converted", "value_clean", "ph", "temp_ph",

  # Parameter mapping columns
  "param_lab", "param_snirh", "symbol_snirh", "unit_lab", "unit_snirh",
  "factor", "sample_type",

  # Station validation columns
  "status", "Codigo", "Estado",

  # Temporary/utility columns
  "total", "parameter_fault",

  # data.table special symbols
  ".", ".SD", ".N", ".I", ".GRP", ".NGRP", ".BY", ".EACHI",

  # Column selection helpers (used with ..cols syntax)
  "..cols_converted", "..cols_final", "..cols_vip", "..final_cols"
))

# Package datasets
utils::globalVariables(c(
  "parameters"  # Main parameter conversion lookup table
))

# Package constants (defined in convert_to_snirh.R)
utils::globalVariables(c(
  "VALID_MATRICES", "REQUIRED_COLUMNS", "NETWORK_CONFIG", "SNIRH_STATIONS_URL"
))

# Utility function variables
utils::globalVariables(c(
  "found", "active"  # Used in station validation utilities
))

#' Package startup message
#'
#' Displays welcome message when package is loaded, including version information
#' and brief usage guidance.
#'
#' @param libname Library name (unused)
#' @param pkgname Package name
#' @importFrom utils packageVersion
#' @noRd
.onAttach <- function(libname, pkgname) {
  version <- packageVersion(pkgname)

  packageStartupMessage(
    paste0(
      "Welcome to ", pkgname, " version ", version, "\n",
      "For help getting started, try: help(package = '", pkgname, "')\n",
      "Main function: convert_to_snirh() - see ?convert_to_snirh for details\n",
      "Station validation requires 'sf' package for surface water and biota data"
    )
  )
}

#' Package detach cleanup
#'
#' Performs cleanup operations when package is detached (optional).
#' Currently no cleanup operations are needed.
#'
#' @param libpath Library path (unused)
#' @noRd
.onDetach <- function(libpath) {
  # Future cleanup operations can be added here if needed
  invisible()
}

#' Validate package data integrity on load
#'
#' Checks that required datasets are properly formatted and complete.
#' This function is called during package loading to ensure data quality.
#'
#' @param libname Library name (unused)
#' @param pkgname Package name
#' @noRd
.onLoad <- function(libname, pkgname) {
  # Validate parameters dataset if available
  if (exists("parameters", envir = asNamespace(pkgname))) {
    validate_parameters_data()
  }

  # Check for optional dependencies
  check_optional_dependencies()
}

#' Validate parameters dataset structure
#'
#' Internal function to check that the parameters dataset meets requirements
#' for the convert_to_snirh function.
#'
#' @noRd
validate_parameters_data <- function() {
  required_cols <- c("param_lab", "unit_lab", "symbol_snirh",
                     "param_snirh", "unit_snirh", "factor", "sample_type")

  if (!exists("parameters")) {
    warning("Parameters dataset not found. Package may not function correctly.",
            call. = FALSE)
    return(FALSE)
  }

  if (!all(required_cols %in% names(parameters))) {
    missing_cols <- setdiff(required_cols, names(parameters))
    warning(paste("Parameters dataset missing required columns:",
                  paste(missing_cols, collapse = ", ")),
            call. = FALSE)
    return(FALSE)
  }

  # Check for valid sample types
  valid_sample_types <- c("water", "biota", "sediment")
  invalid_types <- setdiff(unique(parameters$sample_type), valid_sample_types)

  if (length(invalid_types) > 0) {
    warning(paste("Parameters dataset contains invalid sample types:",
                  paste(invalid_types, collapse = ", ")),
            call. = FALSE)
    return(FALSE)
  }

  # Check for missing conversion factors
  if (any(is.na(parameters$factor))) {
    warning("Parameters dataset contains missing conversion factors",
            call. = FALSE)
    return(FALSE)
  }

  return(TRUE)
}

#' Check for optional dependencies
#'
#' Checks if optional packages are available and provides informative messages.
#'
#' @noRd
check_optional_dependencies <- function() {
  # Check for sf package (required for station validation)
  if (!requireNamespace("sf", quietly = TRUE)) {
    packageStartupMessage(
      "Note: Package 'sf' is not installed.\n",
      "Station validation for surface water and biota data will not be available.\n",
      "Install with: install.packages('sf')"
    )
  }

  # Check for curl package (used for internet connectivity)
  if (!requireNamespace("curl", quietly = TRUE)) {
    packageStartupMessage(
      "Note: Package 'curl' is not installed.\n",
      "Internet connectivity checks will use base R functions.\n",
      "For better performance, install with: install.packages('curl')"
    )
  }
}
