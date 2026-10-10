#' Get SNIRH station information
#'
#' Downloads and returns information about SNIRH water quality monitoring
#' stations. This function can be used to check station status, get available
#' station IDs, or validate stations before data conversion.
#'
#' @param matrix Character string specifying the matrix type. Currently supports
#'   "surface.water" and "biota" (both use the same station list).
#' @param active_only Logical. If TRUE, returns only active stations
#'   (status = "ATIVA"). If FALSE, returns all stations. Default is FALSE.
#' @param refresh Logical. If TRUE, downloads the station list again even if it
#'   was already downloaded in the current R session. Default is FALSE.
#'
#' @return A data.table with one row per station and the columns:
#'   \describe{
#'     \item{station_id}{Station identifier ("Código" in SNIRH)}
#'     \item{station_name}{Station name}
#'     \item{basin}{River basin}
#'     \item{altitude_m}{Altitude (m)}
#'     \item{latitude, longitude}{Coordinates (decimal degrees, WGS84)}
#'     \item{entity}{Entity responsible for the station}
#'     \item{district, municipality, parish}{Administrative location}
#'     \item{river}{River name}
#'     \item{drainage_area_km2}{Drainage area (km2)}
#'     \item{distance_to_mouth_km}{Distance to the river mouth (km)}
#'     \item{start_date}{Date the station started operating}
#'     \item{end_date}{Date the station was closed}
#'     \item{purpose}{Monitoring purpose}
#'     \item{status}{Station status (e.g., "ATIVA", "DESATIVADA", "EXTINTA")}
#'   }
#'
#' @details
#' Downloads the latest station list (CSV) from the SNIRH website
#' (<https://snirh.apambiente.pt>). It requires an internet connection. The
#' list is cached for the rest of the R session, so subsequent calls (including
#' those made by [convert_to_snirh()] and [check_station_status()]) are fast.
#' Use `refresh = TRUE` to force a new download.
#'
#' @section Station Status:
#' Stations can have different status values:
#' \describe{
#'   \item{ATIVA}{Station is active and can receive new data}
#'   \item{DESATIVADA}{Station is inactive (historical data only)}
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
get_snirh_stations <- function(matrix = "surface.water", active_only = FALSE,
                               refresh = FALSE) {
  # Validate inputs
  if (!is.character(matrix) || length(matrix) != 1L || !matrix %in% VALID_MATRICES) {
    cli_abort("{.arg matrix} must be one of: {.val {VALID_MATRICES}}")
  }
  check_flag(active_only)
  check_flag(refresh)

  # Check internet connectivity (only needed if the list is not cached)
  if ((refresh || is.null(.snirh_cache$stations)) && !check_internet_connection()) {
    cli_abort("Internet connection required to download station data")
  }

  # Download station data
  stations <- download_snirh_stations(refresh = refresh)

  # Filter for active stations if requested
  if (active_only) {
    stations <- stations[status %in% "ATIVA"]
  }

  return(stations)
}
