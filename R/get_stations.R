#' Get SNIRH station information
#'
#' Downloads and returns information about SNIRH monitoring stations for
#' surface water quality. This function can be used to check station status,
#' get available station IDs, or validate stations before data conversion.
#'
#' @param matrix Character string specifying the matrix type. Currently supports
#'   "surface.water" and "biota" (both use the same station database).
#' @param timeout Numeric. Timeout in seconds for downloading data. Default is 30.
#' @param active_only Logical. If TRUE, returns only active stations (Estado = "ATIVA").
#'   If FALSE, returns all stations. Default is FALSE.
#'
#' @return A data.table with station information containing:
#'   \describe{
#'     \item{station_id}{Station identifier (corresponds to "Código" in SNIRH)}
#'     \item{status}{Station status (e.g., "ATIVA", "EXTINTA")}
#'     \item{geometry}{Spatial geometry (if sf package is available)}
#'   }
#'
#' @details
#' Downloads the latest station information from the SNIRH
#' geoportal. It requires an internet connection and the 'sf' package for
#' processing shapefiles.
#'
#' The station database includes information about:
#' \itemize{
#'   \item Station location (coordinates)
#'   \item Station status (active/inactive)
#'   \item Station metadata
#' }
#'
#' @section Station Status:
#' Stations can have different status values:
#' \describe{
#'   \item{ATIVA}{Station is active and can receive new data}
#'   \item{INATIVA}{Station is inactive (historical data only)}
#'   \item{EXTINTA}{Station is permanently suspended and has no data}
#' }
#'
#' @examples
#' # Get all surface water stations
#' all_stations <- get_snirh_stations("surface.water")
#' print(head(all_stations))
#'
#' # Get only active stations
#' active_stations <- get_snirh_stations("surface.water", active_only = TRUE)
#' print(paste("Active stations:", nrow(active_stations)))
#'
#' # Check if specific stations are active
#' my_stations <- c("07G/01H", "07G/02H")
#' station_info <- get_snirh_stations("surface.water")
#' station_status <- station_info[station_id %in% my_stations]
#' print(station_status)
#' 
#'
#' @seealso \code{\link{convert_to_snirh}} for the main conversion function
#' @seealso \code{\link{check_station_status}} for checking specific stations
#'
#' @importFrom cli cli_alert_info cli_alert_success cli_abort
#' @import data.table
#' @export
get_snirh_stations <- function(matrix = "surface.water", timeout = 30, active_only = FALSE) {

  # Validate inputs
  if (!matrix %in% c("surface.water", "biota")) {
    cli_abort("Matrix must be 'surface.water' or 'biota'")
  }

  # Check internet connectivity
  if (!check_internet_connection(timeout = 10)) {
    cli_abort("Internet connection required to download station data")
  }

  # Download station data using the same function as convert_to_snirh
  stations <- download_snirh_stations(matrix, timeout)

  # Filter for active stations if requested
  if (active_only) {
    stations <- stations[status == "ATIVA"]
  } else {
    active_count <- stations[status == "ATIVA", .N]
  }

  return(stations)
}

#' Check status of specific SNIRH stations
#'
#' Validates specific station IDs against the SNIRH database and returns
#' their current status. This is useful for checking stations before
#' running the full conversion process.
#'
#' @param station_ids Character vector of station IDs to check.
#' @param matrix Character string specifying the matrix type. Currently supports
#'   "surface.water" and "biota". Default is "surface.water".
#' @param timeout Numeric. Timeout in seconds for downloading data. Default is 30.
#'
#' @return A data.table with the following columns:
#'   \describe{
#'     \item{station_id}{The station ID that was checked}
#'     \item{found}{Logical indicating if station exists in SNIRH}
#'     \item{status}{Station status if found, NA if not found}
#'     \item{active}{Logical indicating if station is active (status = "ATIVA")}
#'   }
#'
#' @details
#' This function is particularly useful for:
#' \itemize{
#'   \item Pre-validating station IDs before data conversion
#'   \item Checking why certain stations fail validation
#'   \item Getting an overview of station status for reporting
#' }
#'
#' @examples
#' # Check status of specific stations
#' my_stations <- c("07G/01H", "07G/02H", "INVALID_ID")
#' status_check <- check_station_status(my_stations)
#' print(status_check)
#'
#' # Check which stations are not active
#' inactive <- status_check[active == FALSE | is.na(active)]
#' if (nrow(inactive) > 0) {
#'   print("Stations requiring attention:")
#'   print(inactive)
#'
#' # Check only active stations
#' active_stations <- status_check[active == TRUE]
#'
#' @seealso \code{\link{get_snirh_stations}} for getting all station information
#' @seealso \code{\link{convert_to_snirh}} for the main conversion function
#'
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning
#' @import data.table
#' @export
check_station_status <- function(station_ids, matrix = "surface.water", timeout = 30) {

  # Validate inputs
  if (length(station_ids) == 0) {
    cli_abort("No station IDs provided")
  }

  # Remove NAs and duplicates
  station_ids <- unique(station_ids[!is.na(station_ids)])

  # Get all SNIRH stations
  snirh_stations <- get_snirh_stations(matrix, timeout, active_only = FALSE)

  # Create result data.table
  result <- data.table(
    station_id = station_ids,
    found = station_ids %in% snirh_stations$station_id,
    status = NA_character_,
    active = NA
  )

  # Fill in status information for found stations
  for (i in seq_len(nrow(result))) {
    if (result$found[i]) {
      station_info <- snirh_stations[station_id == result$station_id[i]]
      result$status[i] <- station_info$status[1]
      result$active[i] <- station_info$status[1] == "ATIVA"
    }
  }

  # Report results
  found_count <- sum(result$found)
  active_count <- sum(result$active, na.rm = TRUE)
  missing_count <- sum(!result$found)

  if (missing_count > 0) {
    missing_ids <- result[found == FALSE, station_id]
    cli_alert_warning("Missing from SNIRH: {.val {missing_ids}}")
  }

  inactive_count <- found_count - active_count
  if (inactive_count > 0) {
    inactive_ids <- result[found == TRUE & active == FALSE,
                           paste0(station_id, " (", status, ")")]
    cli_alert_warning("Inactive stations: {.val {inactive_ids}}")
  }

  return(result)
}

#' List available SNIRH parameters
#'
#' Returns a summary of available parameters in the conversion table,
#' organized by sample type. This helps users understand what parameters
#' can be converted to SNIRH format.
#'
#' @param sample_type Character string specifying the sample type to filter by.
#'   Must be one of "water", "biota", "sediment", or "all". Default is "all".
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
#'
#' @seealso \code{\link{parameters}} for the complete parameter dataset
#'
#' @import data.table
#' @export
list_snirh_parameters <- function(sample_type = "all", include_conversion_info = FALSE) {

  # Validate inputs
  valid_types <- c("water", "biota", "sediment", "all")
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
    param_data <- param_data[sample_type == sample_type]
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
