#' List available SNIRH parameters
#'
#' Returns a summary of available parameters in the conversion table,
#' organized by sample type. This helps users understand what parameters
#' can be converted to SNIRH format.
#'
#' @param sample_type Character string specifying the sample type to filter by.
#'   Must be one of "water", "biota" or "all". Default is "all".
#' @param include_conversion_info Logical. If TRUE, includes conversion factors
#'   and unit information. Default is FALSE.
#'
#' @return A data.table with parameter information. Columns depend on
#'   include_conversion_info parameter.
#'
#' @details
#' This function provides an overview of the parameter conversion capabilities
#' of the package. It can help users:
#' \itemize{
#'   \item Understand what parameters are supported
#'   \item Check parameter naming conventions
#'   \item Verify unit conversion factors
#'   \item Plan data preparation activities
#' }
#'
#' @examples
#' \donttest{
#' # List all water parameters
#' water_params <- list_snirh_parameters("water")
#' print(head(water_params))
#'
#' # Get detailed conversion information
#' detailed_params <- list_snirh_parameters("water", include_conversion_info = TRUE)
#' print(head(detailed_params))
#'
#' # Check all available sample types
#' all_params <- list_snirh_parameters("all")
#' unique_types <- unique(all_params$sample_type)
#' print(paste("Available sample types:", paste(unique_types, collapse = ", ")))
#' }
#'
#' @seealso \code{\link{parameters}} for the complete parameter dataset
#'
#' @import data.table
#' @export
list_snirh_parameters <- function(sample_type = "all", include_conversion_info = FALSE) {
  # Validate inputs
  valid_types <- c("water", "biota", "all")
  if (!sample_type %in% valid_types) {
    cli_abort("sample_type must be one of: {.val {valid_types}}")
  }

  # Get parameters data
  if (!exists("parameters")) {
    cli_abort("Parameters dataset not available. Please ensure package is properly loaded.")
  }

  param_data <- copy(parameters)

  # Filter by sample type if not "all"
  if (sample_type != "all") {
    stype <- sample_type
    param_data <- param_data[sample_type == stype]
  }

  # Select columns based on detail level
  if (include_conversion_info) {
    result <- param_data[, .(
      sample_type,
      param_lab,
      unit_lab,
      param_snirh,
      unit_snirh,
      symbol_snirh,
      factor
    )]
  } else {
    result <- param_data[, .(
      sample_type,
      param_lab,
      param_snirh,
      symbol_snirh
    )]
  }

  # Order by sample type and parameter name
  setorder(result, sample_type, param_lab)

  return(result)
}
