# Minimal valid input for convert_to_snirh(); arguments override the defaults
lab_data <- function(...) {
  defaults <- list(
    snirh_entity = "APA",
    station_id = "01F/01",
    sampling_date = as.POSIXct("2024-01-15 10:30:00", tz = "UTC"),
    parameter = "pH - Campo",
    unit = "Escala Sorensen",
    value = "7.2"
  )
  do.call(data.table::data.table, utils::modifyList(defaults, list(...)))
}

convert_offline <- function(data, matrix = "surface.water") {
  convert_to_snirh(data, matrix, validate_stations = FALSE)
}
