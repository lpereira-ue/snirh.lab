
#' Check status of specific SNIRH stations
#'
#' Validates specific station IDs against the SNIRH database and returns
#' their current status. This is useful for checking stations before
#' running the full conversion process.
#'
#' @param station_ids Character vector of station IDs to check.
#' @param matrix Character string specifying the matrix type. Currently supports
#'   "surface.water" and "biota". Default is "surface.water".
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
#' \donttest{
#' # Check status of specific stations
#' my_stations <- c("07G/50", "25G/07", "INVALID_ID")
#' status_check <- check_station_status(my_stations)
#' print(status_check)
#'
#' # Check which stations are not active
#' inactive <- status_check[active == FALSE | is.na(active)]
#' if (nrow(inactive) > 0) {
#'   print("Stations requiring attention:")
#'   print(inactive)
#'   }
#'
#' # Check only active stations
#' active_stations <- status_check[active == TRUE]
#' }
#'
#' @seealso \code{\link{get_snirh_stations}} for getting all station information
#' @seealso \code{\link{convert_to_snirh}} for the main conversion function
#'
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning
#' @import data.table
#' @export
check_station_status <- function(station_ids, matrix = "surface.water") {

  # Validate inputs
  if (length(station_ids) == 0) {
    cli_abort("No station IDs provided")
  }

  # Remove NAs and duplicates
  station_ids <- unique(station_ids[!is.na(station_ids)])

  # Get all SNIRH stations
  snirh_stations <- get_snirh_stations(matrix, active_only = FALSE)

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
