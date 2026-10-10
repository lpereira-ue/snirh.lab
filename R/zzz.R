# Package initialization and global variables ----

# Suppress R CMD CHECK notes for data.table columns, which are created and
# referenced dynamically and are not visible to static analysis
utils::globalVariables(c(
  # Input data columns
  "snirh_entity", "station_id", "sampling_date", "parameter", "unit", "value",

  # Processed/derived columns
  "value_clean", "value_num", "value_converted", "value_final", "flag", "std",

  # Parameter table columns
  "sample_type", "param_lab", "unit_lab", "symbol_snirh", "param_snirh",
  "unit_snirh", "factor",

  # Station columns
  "station_name", "status", "found", "active",

  # data.table special symbols
  ".", ".SD", ".N",

  # Column selection helpers (used with ..cols syntax)
  "..final_cols"
))

#' Package startup message
#'
#' Displays welcome message when package is attached, including version
#' information and brief usage guidance.
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
      "Main function: convert_to_snirh() - see ?convert_to_snirh for details"
    )
  )
}
