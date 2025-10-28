#' Get SNIRH station information
#'
#' Downloads and returns information about SNIRH monitoring stations for
#' surface water quality. This function can be used to check station status,
#' get available station IDs, or validate stations before data conversion.
#'
#' @param matrix Character string specifying the matrix type. Currently supports
#'   "surface.water" and "biota" (both use the same station database).
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
#' \donttest{
#' # Get all surface water stations
#' all_stations <- get_snirh_stations("surface.water")
#' print(head(all_stations))
#'
#' # Get only active stations
#' active_stations <- get_snirh_stations("surface.water", active_only = TRUE)
#' print(paste("Active stations:", nrow(active_stations)))
#'
#' # Check if specific stations are active
#' my_stations <- c("07H/50", "25G/07")
#' station_info <- get_snirh_stations("surface.water")
#' station_status <- station_info[station_id %in% my_stations]
#' print(station_status)
#'}
#'
#' @seealso \code{\link{convert_to_snirh}} for the main conversion function
#' @seealso \code{\link{check_station_status}} for checking specific stations
#'
#' @importFrom cli cli_abort
#' @import data.table
#' @export
get_snirh_stations <- function(matrix = "surface.water", active_only = FALSE) {
  # Validate inputs
  if (!matrix %in% c("surface.water", "biota")) {
    cli_abort("Matrix must be 'surface.water' or 'biota'")
  }

  # Check internet connectivity
  if (!check_internet_connection()) {
    cli_abort("Internet connection required to download station data")
  }

  # Download station data
  stations <- download_snirh_stations(matrix)

  # Filter for active stations if requested
  if (active_only) {
    stations <- stations[status == "ATIVA"]
  }

  return(stations)
}
