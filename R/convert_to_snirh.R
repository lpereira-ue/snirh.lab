# Constants
VALID_MATRICES <- c("surface.water", "biota")
REQUIRED_COLUMNS <- c("snirh_entity", "station_name", "station_id", "sampling_date",
                      "parameter", "unit", "value")

# Network configuration
NETWORK_CONFIG <- list(
  "surface.water" = list(network = "QUAL", sample_type = "water", validate_stations = TRUE),
  "biota" = list(network = "QUAL", sample_type = "biota", validate_stations = TRUE)
)

# SNIRH Station URLs
SNIRH_STATIONS_URL <- list(
  "surface.water" = "https://sniambgeoviewer.apambiente.pt/GeoDocs/shpzips/snirh_qualsup_vw.zip",
  "biota" = "https://sniambgeoviewer.apambiente.pt/GeoDocs/shpzips/snirh_qualsup_vw.zip"
)

#' Convert data to SNIRH file format
#'
#' Cleans and converts laboratory data to the SNIRH (National
#' Information System on Water Resources) import format. It handles data
#' validation, unit conversions, station validation, and formatting according
#' to SNIRH standards.
#'
#' @param data A data.frame or data.table containing the original laboratory data.
#'   Must contain the following columns in order: snirh_entity, station_name,
#'   station_id, sampling_date, parameter, unit, value.
#' @param matrix Character string specifying the type of matrix being processed.
#'   Must be one of: "surface.water" or "biota".
#' @param validate_stations Logical. Whether to validate station IDs against
#'   the SNIRH database. Defaults to TRUE for surface.water and biota matrices.
#'   Requires internet connection.
#'
#' @return A data.table formatted for SNIRH import with the following structure:
#'   - First row contains network specification (REDE=NETWORK_NAME)
#'   - Station identifiers (ESTACAO=STATION_ID) before each group of measurements
#'   - Date/time stamps in DD/MM/YYYY HH:MM format
#'   - Parameter values in SNIRH-compatible units and symbols
#'
#' @details
#' The function performs several key operations:
#' \itemize{
#'   \item Validates input data structure and removes empty rows/columns
#'   \item Validates station IDs against SNIRH database (for surface.water and biota)
#'   \item Checks for duplicate measurements (same station, date, and parameter)
#'   \item Extracts pH temperature measurements when present
#'   \item Converts measurement values to SNIRH-compatible units
#'   \item Handles measurement flags (<, >, =) and special values
#'   \item Formats output according to SNIRH import specifications
#' }
#'
#' @section Station Validation:
#' For surface.water and biota matrices, the function validates that:
#' \itemize{
#'   \item All station IDs exist in the SNIRH database
#'   \item All stations have status "ATIVA" (active)
#'   \item Internet connection is available for downloading station data
#' }
#'
#' If validation fails, the function will stop and provide details about
#' invalid stations that need to be corrected in the database.
#'
#' @section Input Data Requirements:
#' The input data must be a data.frame/data.table with exactly these columns:
#' \describe{
#'   \item{snirh_entity}{Entity responsible for the data}
#'   \item{station_name}{Human-readable station name}
#'   \item{station_id}{Unique station identifier (must match SNIRH database)}
#'   \item{sampling_date}{Date and time of sampling (POSIXct recommended)}
#'   \item{parameter}{Parameter name as used in laboratory}
#'   \item{unit}{Unit of measurement as used in laboratory}
#'   \item{value}{Measured value (may include flags like <, >)}
#' }
#'
#' @section Parameter Conversion:
#' Relies on an internal `parameters` dataset that maps laboratory
#' parameter names and units to SNIRH equivalents. This dataset must contain
#' conversion factors and SNIRH symbols for all parameters in the input data.
#'
#' @examples
#' # Example data structure
#' \donttest{
#' lab_data <- data.table::data.table(
#'   snirh_entity = "APA",
#'   station_name = "River station 1",
#'   station_id = "01F/01",  # Must be valid SNIRH station ID
#'   sampling_date = as.POSIXct("2024-01-15 10:30:00"),
#'   parameter = "pH - Campo",
#'   unit = "Escala Sorensen",
#'   value = "7.2"
#' )
#'
#' # Convert surface water data (with station validation)
#' snirh_data <- convert_to_snirh(lab_data, "surface.water")
#'
#' # Skip station validation if needed (not recommended)
#' snirh_data <- convert_to_snirh(lab_data, "surface.water",
#'                                validate_stations = FALSE)
#' }
#'
#' @importFrom cli cli_abort cli_alert_warning
#' @importFrom utils download.file unzip
#' @import data.table
#' @export
convert_to_snirh <- function(data, matrix, validate_stations = NULL) {
  # Input validation
  validate_inputs(data, matrix)

  # Get network configuration
  config <- NETWORK_CONFIG[[matrix]]

  # Set default station validation based on matrix type
  if (is.null(validate_stations)) {
    validate_stations <- config$validate_stations
  }

  # Filter parameters for the specified sample type
  relevant_params <- parameters[sample_type == config$sample_type]

  # Convert to data.table and validate structure
  data <- as.data.table(data)
  validate_data_structure(data)

  # Validate stations against SNIRH database if required
  if (validate_stations && config$validate_stations) {
    validate_snirh_stations(data, matrix)
  }

  # Clean data (remove empty rows/columns)
  data_cleaned <- clean_empty_data(data)

  # Validate data integrity
  validate_data_integrity(data_cleaned)

  # Extract pH temperature data if present
  data_with_ph_temp <- extract_ph_temperature(data_cleaned)

  # Clean and standardize values
  data_with_ph_temp_cleaned <- clean_values(data_with_ph_temp)

  # Convert to SNIRH units
  data_converted <- convert_units(data_with_ph_temp_cleaned, relevant_params)

  # Format for SNIRH output
  data_final <- format_for_snirh(data_converted, config$network)

  return(data_final)
}


#' Validate function inputs
#' @param data Input data
#' @param matrix Matrix type
#' @noRd
#' @keywords internal
validate_inputs <- function(data, matrix) {
  if (!matrix %in% VALID_MATRICES) {
    cli_abort("Matrix type must be one of: {.val {VALID_MATRICES}}")
  }

  if (!is.data.frame(data)) {
    cli_abort("Data object must be a data.frame or data.table")
  }

  if (nrow(data) == 0) {
    cli_abort("Input data is empty")
  }
}


#' Validate station IDs against SNIRH database
#' @param data Input data.table
#' @param matrix Matrix type
#' @noRd
validate_snirh_stations <- function(data, matrix) {
  # Check internet connection
  if (!check_internet_connection()) {
    cli_abort(c(
      "Internet connection required for station validation",
      "i" = "Please check your connection or set validate_stations = FALSE"
    ))
  }

  # Get unique station IDs from data
  data_stations <- trimws(unique(data$station_id))
  data_stations <- data_stations[!is.na(data_stations)]

  if (length(data_stations) == 0) {
    cli_abort("No valid station IDs found in data")
  }

  # Download SNIRH station data
  snirh_stations <- download_snirh_stations(matrix)

  # Check which stations exist in SNIRH
  missing_stations <- setdiff(data_stations, snirh_stations$station_id)
  if (length(missing_stations) > 0) {
    cli_abort(c(
      "Station ID(s) not found in SNIRH database: {.val {missing_stations}}",
      "i" = "Please verify station IDs or update SNIRH database"
    ))
  }

  # Check station status (must be ATIVA)
  station_status <- snirh_stations[station_id %in% data_stations]
  inactive_stations <- station_status[status != "ATIVA"]

  if (nrow(inactive_stations) > 0) {
    inactive_list <- inactive_stations[, paste0(station_id, " (", status, ")")]
    cli_abort(c(
      "Station(s) not active in SNIRH database [{length(inactive_list)}]:",
      "x" = "{.val {inactive_list}}",
      "i" = "Only stations with status 'ATIVA' can receive data",
      "i" = "Please update station status in SNIRH database first"
    ))
  }

  # Report station validation results
  active_stations <- station_status[status == "ATIVA", .N]
}


#' Validate data structure
#' @param data Input data as data.table
#' @noRd
#' @keywords internal
validate_data_structure <- function(data) {
  if (!all(REQUIRED_COLUMNS == names(data))) {
    cli_abort(c(
      "Column names or order do not meet requirements",
      "i" = "Expected columns: {.val {REQUIRED_COLUMNS}}",
      "i" = "Actual columns:   {.val {names(data)}}"
    ))
  }
}


#' Clean empty rows and columns from data
#' @param data Input data.table
#' @return Cleaned data.table
#' @noRd
#' @keywords internal
clean_empty_data <- function(data) {
  initial_rows <- nrow(data)
  initial_cols <- ncol(data)

  # Remove completely empty rows
  data_cleaned <- data[rowSums(is.na(data), na.rm = TRUE) != ncol(data)]

  # Remove completely empty columns
  data_cleaned <- data_cleaned[, colSums(is.na(data_cleaned), na.rm = TRUE) != nrow(data_cleaned), with = FALSE]

  # Clean whitespace from key columns
  data_cleaned[, station_id := trimws(station_id, whitespace = "[ \\h\\v\t\n\r]")]
  data_cleaned[, value := trimws(value, whitespace = "[ \\h\\v\t\n\r]")]

  return(data_cleaned)
}


#' Validate data integrity (nulls and duplicates)
#' @param data Cleaned data.table
#' @noRd
#' @keywords internal
validate_data_integrity <- function(data) {
  # Check for null values in critical columns
  # Exclude IGA parameter which has no unit
  data_for_null_check <- data[parameter != "\u00CDNDICE DE GRUPO DE ALGAS (IGA)"]

  null_counts <- data_for_null_check[,
                                     lapply(.SD, function(x) sum(is.na(x))),
                                     .SDcols = REQUIRED_COLUMNS
  ]
  total_nulls <- sum(unlist(null_counts))

  # cols_nulls <- names(null_counts)[
  #   null_counts[, sapply(.SD, function(x) x > 0)]
  # ]
  cols_nulls <- names(null_counts)[unlist(null_counts) > 0]

  if (total_nulls > 0) {
    cli_abort(c(
      "Required columns contain null values: {.field {cols_nulls}}"
    ))
  }

  # Check for duplicates
  dup_row <- anyDuplicated(data, by = c("station_id", "sampling_date", "parameter", "unit"))
  if (dup_row > 0) {
    cli_abort("Duplicate records found for same 'station_id + sampling_date + parameter + unit' combination")
  }
}


#' Extract pH temperature measurements
#' @param data Input data.table
#' @return Data.table with pH temperature extracted as separate rows
#' @noRd
#' @keywords internal
extract_ph_temperature <- function(data) {
  # Find pH measurements with temperature data
  temp_ph <- data[parameter %like% "pH" & value %like% "a"]

  if (nrow(temp_ph) == 0) {
    return(data)
  }

  # Split pH and temperature values
  temp_ph[, c("ph", "temp_ph") := tstrsplit(value, " a ", fixed = TRUE)]
  temp_ph[, c("temp_ph", "unit") := tstrsplit(temp_ph, "\u00BA", fixed = TRUE)]
  temp_ph[, `:=`(
    ph = trimws(ph),
    temp_ph = trimws(temp_ph)
  )]

  # Validate temperature unit
  if (nrow(temp_ph[unit != "C"]) > 0) {
    cli_abort("pH temperature values must be in Celsius (\u00B0C)")
  }

  # Convert temperature data to proper format
  temp_ph[, unit := "\u00BAC"]
  temp_ph <- merge(temp_ph,
                   parameters[, .(param_lab, param_snirh)],
                   by.x = "parameter", by.y = "param_lab",
                   all.x = TRUE)

  temp_ph[, parameter := fcase(
    param_snirh == "pH - campo", "Temperatura do pH - Campo",
    param_snirh == "pH - lab.", "Temperatura do pH",
    default = paste0("Temperatura do ", parameter)
  )]

  # Clean up and prepare for binding
  temp_ph[, c("param_snirh", "ph", "value") := NULL]
  setnames(temp_ph, "temp_ph", "value")

  # Combine original data with temperature data
  data_all <- rbindlist(list(data, temp_ph), use.names = TRUE)

  return(data_all)
}


#' Clean and standardize measurement values
#' @param data Input data.table
#' @return Data.table with cleaned values
#' @noRd
#' @keywords internal
clean_values <- function(data) {
  data[, value_clean := value]

  # Apply cleaning transformations
  cleaning_rules <- list(
    "," = "\\.",              # Replace comma with period
    "\\=" = "",               # Remove equals signs
    "\\(LQ\\)" = "",          # Remove detection limit indicators
    "\u00BAC" = "",           # Remove temperature unit from values
    "Ausente" = "0",          # Convert absence indicators
    "Presente" = ">0",        # Convert presence indicators
    "\u2264" = "<",           # Convert less-than-or-equal symbols
    "\u2265" = ">"            # Convert greater-than-or-equal symbols
  )

  for (pattern in names(cleaning_rules)) {
    replacement <- cleaning_rules[[pattern]]
    data[, value_clean := gsub(pattern, replacement, value_clean)]
  }

  # Handle uncertainty indicators (\u00B1) and temperature ranges
  data[, value_clean := tstrsplit(value_clean, "\u00B1", fixed = TRUE)[[1]]]
  data[, value_clean := tstrsplit(value_clean, " a ", fixed = TRUE)[[1]]]

  # Remove all whitespace
  data[, value_clean := gsub("\\s+", "", value_clean)]

  # Reorder columns for clarity
  setcolorder(data, "value", after = "unit")

  return(data)
}


#' Convert values to SNIRH units
#' @param data Data.table with cleaned values
#' @param relevant_params Parameters dataset filtered for sample type
#' @return Data.table with converted values
#' @noRd
#' @keywords internal
convert_units <- function(data, relevant_params) {
  # Separate flags from numeric values
  data[, `:=`(
    flag = fifelse(
      substr(value_clean, 1, 1) %in% c("<", ">"),
      substr(value_clean, 1, 1),
      NA_character_
    ),
    value_clean = as.double(gsub("[\\<\\>]", "", value_clean))
  )]

  # Join with parameters for unit conversion
  data_converted <- merge(data, relevant_params,
                          by.x = c("parameter", "unit"),
                          by.y = c("param_lab", "unit_lab"),
                          all.x = TRUE)

  # Check for unconverted parameters
  data_not_converted <- data[!relevant_params,
                             on = .(parameter = param_lab, unit = unit_lab)]

  if (nrow(data_not_converted) > 0) {
    failed_params <- unique(data_not_converted[,
                                               paste(parameter, paste0("[", unit, "]"))])
    cli_abort("Parameters not found in conversion table [{length(failed_params)}]: {.val {sort(failed_params)}}")
  }

  # Apply conversion factors
  data_converted[, value_converted := value_clean * factor]

  # Create final value with flags
  data_converted[, value_final := paste0(fifelse(is.na(flag), "", flag), value_converted)]

  return(data_converted)
}


#' Format data for SNIRH output
#' @param data_converted Data.table with converted values
#' @param network Network identifier
#' @return Final formatted data.table
#' @noRd
#' @keywords internal
format_for_snirh <- function(data_converted, network) {
  # Select final columns and reshape
  final_cols <- c("snirh_entity", "station_id", "sampling_date",
                  "symbol_snirh", "value_final")

  data_final <- data_converted[, ..final_cols]

  # Pivot to wide format
  data_wide <- dcast(data_final,
                     station_id + sampling_date + snirh_entity ~ symbol_snirh,
                     value.var = "value_final")

  # Apply SNIRH template formatting
  data_formatted <- apply_snirh_template(data_wide, network)

  # Rename final columns
  setnames(data_formatted, c("std", "snirh_entity"), c("STD", "RESPONSAVEL"))

  # Validate final conversion
  nrow_final <- sum(rowSums(!is.na(data_formatted[, -(1:2)])))
  if (nrow(data_converted) != nrow_final) {
    cli_abort("Data lost during template formatting")
  }

  return(data_formatted)
}


#' Apply SNIRH template formatting
#' @param data_wide Wide format data
#' @param network Network identifier
#' @return Template-formatted data.table
#' @noRd
#' @keywords internal
apply_snirh_template <- function(data_wide, network) {
  dt_out <- copy(data_wide)
  setorder(dt_out, station_id, sampling_date)

  # Add station markers
  dt_out[, station := fifelse(
    !shift(station_id) == station_id | is.na(shift(station_id)),
    paste0("ESTACAO=", station_id),
    NA_character_
  )]

  # Add indexing and positioning
  setcolorder(dt_out, "station", before = 1)
  dt_out[, index := rowid(station_id)]
  setcolorder(dt_out, "index", before = 1)

  # Insert station headers
  dt_out <- insert_station_headers(dt_out)

  # Format timestamps
  dt_out[, std := fifelse(!is.na(std),
                          std,
                          format(sampling_date, "%d/%m/%Y %H:%M"))]

  # Clean up columns
  dt_out[, c("index", "station", "station_id", "sampling_date") := NULL]

  # Reorder columns
  setcolorder(dt_out, c("std", "snirh_entity",
                        sort(names(dt_out)[!names(dt_out) %in% c("std", "snirh_entity")])))

  # Add network header
  dt_out <- rbindlist(list(
    data.table("std" = paste0("REDE=", network)),
    dt_out
  ), fill = TRUE)

  return(dt_out)
}


#' Insert station headers into formatted data
#' @param dt_out Data.table with station information
#' @return Data.table with station headers inserted
#' @noRd
#' @keywords internal
insert_station_headers <- function(dt_out) {
  # Find positions for station headers
  breaks <- which(dt_out$index == 1)
  new_lines <- dt_out$station[breaks]

  # Add std column
  dt_out[, std := NA]
  setcolorder(dt_out, c("index", "std", names(dt_out)[!names(dt_out) %in% c("index", "std")]))

  # Insert headers
  if (length(breaks) > 0) {
    # First station
    dt_aux <- rbindlist(list(
      data.table("std" = new_lines[1]),
      dt_out
    ), fill = TRUE)

    # Additional stations
    if (length(breaks) > 1) {
      for (i in 2:length(breaks)) {
        pos <- breaks[i] + i - 1
        new_row <- data.table("std" = new_lines[i])

        # Split and recombine
        dt_before <- dt_aux[1:(pos - 1)]
        dt_after <- dt_aux[pos:.N]
        dt_aux <- rbindlist(list(dt_before, new_row, dt_after), fill = TRUE)
      }
    }

    return(dt_aux)
  }

  return(dt_out)
}
