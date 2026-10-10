#' Download and parse the SNIRH station list
#'
#' Downloads the station list (CSV) from the SNIRH website. The list is cached
#' for the rest of the R session, so later calls do not download it again
#' unless `refresh = TRUE`.
#'
#' @param refresh Logical. Download the list again even if it is cached.
#' @return data.table with station information (see `get_snirh_stations()`)
#' @noRd
#' @keywords internal
download_snirh_stations <- function(refresh = FALSE) {
  if (!refresh && !is.null(.snirh_cache$stations)) {
    return(copy(.snirh_cache$stations))
  }

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  # The file is generated on the fly by the server and may take a while
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = max(300, old_timeout))

  status <- tryCatch(
    download.file(SNIRH_STATIONS_URL, tmp, mode = "wb", quiet = TRUE),
    error = function(e) conditionMessage(e)
  )
  if (!identical(status, 0L) || !file.exists(tmp) || file.size(tmp) == 0) {
    cli_abort(c(
      "Failed to download the SNIRH station list.",
      "i" = if (is.character(status)) "Error: {status}",
      "i" = "Verify that {.url https://snirh.apambiente.pt} is reachable and try again."
    ))
  }

  stations <- read_snirh_stations_csv(tmp)
  .snirh_cache$stations <- stations

  copy(stations)
}


#' Parse the SNIRH station CSV file
#'
#' The file has a few title lines before the header and a "Dados obtidos..."
#' line after the data, both of which are removed. Missing values are coded
#' as "-".
#'
#' @param file Path to the CSV file (Latin-1 encoded).
#' @return data.table with station information
#' @noRd
#' @keywords internal
read_snirh_stations_csv <- function(file) {
  stations <- tryCatch(
    fread(
      file = file,
      sep = ",",
      encoding = "Latin-1",
      skip = "NOME,BACIA",  # header line (the first column name has an accent)
      header = TRUE,
      fill = TRUE,
      colClasses = "character",
      na.strings = c("", "-"),
      showProgress = FALSE
    ),
    error = function(e) {
      cli_abort(c(
        "Could not read the SNIRH station list.",
        "i" = "Error: {conditionMessage(e)}"
      ))
    }
  )

  if (ncol(stations) != length(SNIRH_STATIONS_COLUMNS) ||
      toupper(names(stations)[ncol(stations)]) != "ESTADO") {
    cli_abort(c(
      "Unexpected format of the SNIRH station list.",
      "i" = "Expected {length(SNIRH_STATIONS_COLUMNS)} columns ending in 'ESTADO'.",
      "i" = "Found: {.val {names(stations)}}"
    ))
  }

  setnames(stations, SNIRH_STATIONS_COLUMNS)

  # Drop blank lines and the trailing "Dados obtidos..." line
  stations <- stations[!is.na(station_id) & !is.na(station_name)]
  stations[, c("coord_x_m", "coord_y_m") := NULL]

  chr_cols <- names(stations)[vapply(stations, is.character, logical(1))]
  stations[, (chr_cols) := lapply(.SD, trimws), .SDcols = chr_cols]

  num_cols <- c("altitude_m", "latitude", "longitude", "drainage_area_km2",
                "distance_to_mouth_km")
  stations[, (num_cols) := lapply(.SD, function(x) suppressWarnings(as.numeric(x))),
           .SDcols = num_cols]

  date_cols <- c("start_date", "end_date")
  stations[, (date_cols) := lapply(.SD, as.Date, format = "%d-%m-%Y"),
           .SDcols = date_cols]

  setorder(stations, station_id)
  stations[]
}
