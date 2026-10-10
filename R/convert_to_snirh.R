#' Convert data to SNIRH file format
#'
#' Cleans and converts laboratory data to the SNIRH (National
#' Information System on Water Resources) import format. It handles data
#' validation, unit conversions, station validation, and formatting according
#' to SNIRH standards.
#'
#' @param data A data.frame or data.table containing the original laboratory data.
#'   Must contain exactly these columns, in order: snirh_entity, station_id,
#'   sampling_date, parameter, unit, value, with no missing values. The
#'   station_id <-> station_name matching is expected to be done upstream,
#'   before data is submitted to conversion.
#' @param matrix Character string specifying the type of matrix being processed.
#'   Must be one of: "surface.water" or "biota".
#' @param validate_stations Logical. Whether to validate station IDs against
#'   the SNIRH database. Defaults to TRUE. Set to FALSE for offline use, testing,
#'   or matrices that don't support validation.
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
#'   \item Validates input data structure and removes empty rows
#'   \item Checks for missing values and duplicate measurements (same station,
#'     date, parameter and unit)
#'   \item Validates station IDs against SNIRH database (for surface.water and biota)
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
#' The input data must be a data.frame/data.table with exactly these columns,
#' in order, and with no missing values:
#' \describe{
#'   \item{snirh_entity}{Entity responsible for the data}
#'   \item{station_id}{Unique station identifier (must match SNIRH database)}
#'   \item{sampling_date}{Date and time of sampling. POSIXct is recommended;
#'     Date and character values (e.g. "2024-01-15 10:30" or
#'     "15/01/2024 10:30") are also accepted.}
#'   \item{parameter}{Parameter name as used in laboratory}
#'   \item{unit}{Unit of measurement as used in laboratory (may be empty only
#'     for parameters without unit in the parameter table)}
#'   \item{value}{Measured value (may include flags like <, >)}
#' }
#'
#' pH values may include the measurement temperature, e.g. "7.2 a 20.1 ºC";
#' the temperature is then reported as a separate parameter.
#'
#' @section Parameter Conversion:
#' Relies on the SNIRH parameter conversion table, which maps laboratory
#' parameter names and units to SNIRH equivalents (symbols and conversion
#' factors). The table is maintained outside the package and updated
#' automatically; see [update_snirh_parameters()]. All parameter/unit
#' combinations in the input data must exist in the table.
#'
#' @examplesIf snirh.lab:::snirh_reachable()
#' # Example data structure
#' \donttest{
#' lab_data <- data.table::data.table(
#'   snirh_entity = "APA",
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
#' @importFrom utils download.file
#' @import data.table
#' @export
convert_to_snirh <- function(data, matrix, validate_stations = TRUE) {
  # Validate inputs
  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data.frame or data.table, not {.cls {class(data)}}.")
  }
  if (!is.character(matrix) || length(matrix) != 1L || !matrix %in% VALID_MATRICES) {
    cli_abort("{.arg matrix} must be one of: {.val {VALID_MATRICES}}")
  }
  check_flag(validate_stations)

  # Convert to data.table (copy, so the user's data is never modified)
  data <- as.data.table(data)
  validate_data_structure(data)

  # Clean data (remove empty rows, trim whitespace)
  data <- clean_empty_data(data)
  if (nrow(data) == 0) {
    cli_abort("Input data is empty")
  }

  # Get network configuration and the parameters for the sample type
  config <- NETWORK_CONFIG[[matrix]]
  relevant_params <- load_snirh_parameters()[sample_type == config$sample_type]

  # Validate data integrity (missing values, duplicates, dates)
  validate_data_integrity(data, relevant_params)
  data[, sampling_date := parse_sampling_date(sampling_date)]

  # Validate stations against SNIRH database if required
  if (validate_stations && config$validate_stations) {
    validate_snirh_stations(data)
  }

  # Extract pH temperature data if present
  data <- extract_ph_temperature(data, relevant_params)

  # Clean and standardize values
  data <- clean_values(data)

  # Convert to SNIRH units
  data_converted <- convert_units(data, relevant_params)

  # Format for SNIRH output
  data_final <- format_for_snirh(data_converted, config$network)

  return(data_final)
}


#' Validate station IDs against SNIRH database
#' @param data Input data.table
#' @noRd
validate_snirh_stations <- function(data) {
  # Get unique station IDs from data
  data_stations <- unique(data$station_id)
  data_stations <- data_stations[!is.na(data_stations)]

  if (length(data_stations) == 0) {
    cli_abort("No valid station IDs found in data")
  }

  # Check internet connection (only needed if the list is not cached)
  if (is.null(.snirh_cache$stations) && !check_internet_connection()) {
    cli_abort(c(
      "Internet connection required for station validation",
      "i" = "Please check your connection or set validate_stations = FALSE"
    ))
  }

  # Download SNIRH station data
  snirh_stations <- download_snirh_stations()

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
  inactive_stations <- station_status[!status %in% "ATIVA"]

  if (nrow(inactive_stations) > 0) {
    inactive_list <- inactive_stations[, paste0(
      station_id, " (", fifelse(is.na(status), "no status", status), ")"
    )]
    cli_abort(c(
      "Station(s) not active in SNIRH database [{length(inactive_list)}]:",
      "x" = "{.val {inactive_list}}",
      "i" = "Only stations with status 'ATIVA' can receive data",
      "i" = "Please update station status in SNIRH database first"
    ))
  }

  invisible(TRUE)
}


#' Validate data structure
#' @param data Input data as data.table
#' @noRd
#' @keywords internal
validate_data_structure <- function(data) {
  if (!identical(names(data), REQUIRED_COLUMNS)) {
    cli_abort(c(
      "Column names or order do not meet requirements",
      "i" = "Expected columns: {.val {REQUIRED_COLUMNS}}",
      "i" = "Actual columns:   {.val {names(data)}}"
    ))
  }
}


#' Clean empty rows from data
#' @param data Input data.table
#' @return Cleaned data.table
#' @noRd
#' @keywords internal
clean_empty_data <- function(data) {
  # Remove completely empty rows
  # Check each column: is it NA or (if character) empty string?
  empty_mask <- data[, lapply(.SD, function(x) {
    is.na(x) | (is.character(x) & trimws(x) == "")
  })]
  all_empty <- empty_mask[, Reduce(`&`, .SD)]
  data_cleaned <- data[!all_empty]

  # Clean whitespace (including non-breaking spaces) from text columns
  ws <- "[\\h\\v]"
  text_cols <- c("snirh_entity", "station_id", "parameter", "unit", "value")
  data_cleaned[, (text_cols) := lapply(.SD, function(x) trimws(as.character(x), whitespace = ws)),
               .SDcols = text_cols]

  # Empty strings are missing values
  for (col in text_cols) {
    set(data_cleaned, i = which(data_cleaned[[col]] == ""), j = col, value = NA_character_)
  }

  return(data_cleaned)
}


#' Validate data integrity (nulls and duplicates)
#' @param data Cleaned data.table
#' @param relevant_params Parameters table filtered for sample type
#' @noRd
#' @keywords internal
validate_data_integrity <- function(data, relevant_params) {
  # Check for missing values in required columns. The unit may be missing only
  # for parameters that have no unit in the parameter table (e.g. IGA).
  no_unit_params <- relevant_params[is.na(unit_lab), unique(param_lab)]
  is_missing <- function(x) is.na(x) | trimws(as.character(x)) == ""

  null_counts <- vapply(REQUIRED_COLUMNS, function(col) {
    missing <- is_missing(data[[col]])
    if (col == "unit") {
      missing <- missing & !data$parameter %in% no_unit_params
    }
    sum(missing)
  }, numeric(1))

  if (sum(null_counts) > 0) {
    cols_nulls <- names(null_counts)[null_counts > 0]
    cli_abort("Required columns contain missing values: {.field {cols_nulls}}")
  }

  # Check for duplicates
  dup_keys <- c("station_id", "sampling_date", "parameter", "unit")
  dups <- data[duplicated(data, by = dup_keys)]
  if (nrow(dups) > 0) {
    dup_list <- unique(dups[, paste0(station_id, " | ", format(sampling_date), " | ", parameter)])
    cli_abort(c(
      "Duplicate records found for same 'station_id + sampling_date + parameter + unit' combination:",
      "x" = "{.val {utils::head(dup_list, 10)}}{if (length(dup_list) > 10) ' ...'}"
    ))
  }

  invisible(TRUE)
}


#' Convert sampling dates to POSIXct
#'
#' POSIXct values are kept as they are. Dates are converted to midnight and
#' character values are parsed with common date/time formats.
#'
#' @param x Vector of sampling dates
#' @return POSIXct vector
#' @noRd
#' @keywords internal
parse_sampling_date <- function(x) {
  if (inherits(x, "POSIXct")) {
    return(x)
  }
  if (inherits(x, "POSIXlt")) {
    return(as.POSIXct(x))
  }
  if (inherits(x, "Date")) {
    return(as.POSIXct(format(x, "%Y-%m-%d"), tz = "UTC"))
  }
  if (!is.character(x)) {
    cli_abort(c(
      "{.field sampling_date} must be a date-time (POSIXct), Date or character, not {.cls {class(x)}}."
    ))
  }

  # Accepted: YYYY-MM-DD or DD-MM-YYYY ("/" also allowed), optionally followed
  # by HH:MM or HH:MM:SS. The format is chosen for each value, so that dates
  # are never silently misread (e.g. "15-01-2024" as year 15).
  x_norm <- gsub("/", "-", trimws(x), fixed = TRUE)
  ymd <- grepl("^\\d{4}-\\d{1,2}-\\d{1,2}(\\s|$)", x_norm, perl = TRUE)
  dmy <- grepl("^\\d{1,2}-\\d{1,2}-\\d{4}(\\s|$)", x_norm, perl = TRUE)
  time <- sub("^\\S+\\s*", "", x_norm)
  hms <- grepl("^\\d{1,2}:\\d{2}:\\d{2}(\\.\\d+)?$", time, perl = TRUE)
  hm <- grepl("^\\d{1,2}:\\d{2}$", time, perl = TRUE)

  fmt <- paste0(
    fifelse(ymd, "%Y-%m-%d", "%d-%m-%Y"),
    fifelse(hms, " %H:%M:%OS", fifelse(hm, " %H:%M", ""))
  )
  valid <- (ymd | dmy) & (hms | hm | time == "")

  parsed <- as.POSIXct(rep(NA_real_, length(x)), tz = "UTC")
  for (f in unique(fmt[valid])) {
    i <- which(valid & fmt == f)
    parsed[i] <- as.POSIXct(x_norm[i], format = f, tz = "UTC")
  }

  if (anyNA(parsed)) {
    cli_abort(c(
      "Could not parse {.field sampling_date}: {.val {utils::head(unique(x[is.na(parsed)]), 5)}}",
      "i" = "Use POSIXct values or a format such as {.val 2024-01-15 10:30} or {.val 15/01/2024 10:30}."
    ))
  }

  parsed
}


#' Extract pH temperature measurements
#'
#' pH values can include the measurement temperature (e.g. "7.2 a 20.1 ºC").
#' The temperature is added as a new row ("Temperatura do pH" or
#' "Temperatura do pH - Campo").
#'
#' @param data Input data.table
#' @param relevant_params Parameters table filtered for sample type
#' @return Data.table with pH temperature extracted as separate rows
#' @noRd
#' @keywords internal
extract_ph_temperature <- function(data, relevant_params) {
  # Find pH measurements with temperature data
  has_temp <- grepl("pH", data$parameter, fixed = TRUE) &
    grepl("\\s+a\\s+", data$value)

  if (!any(has_temp)) {
    return(data)
  }

  temp_ph <- data[has_temp]

  # Temperature: text after " a ", must be in degrees Celsius
  temp_value <- trimws(sub("^.*?\\s+a\\s+", "", temp_ph$value, perl = TRUE))
  celsius <- "\\s*[\u00BA\u00B0]\\s*C$"
  invalid <- !grepl(paste0("^-?[0-9]+([.,][0-9]+)?", celsius), temp_value)

  if (any(invalid)) {
    cli_abort(c(
      "pH temperature values must be in Celsius (e.g. {.val 7.2 a 20.1 \u00baC}).",
      "x" = "Invalid value{?s}: {.val {unique(temp_ph$value[invalid])}}"
    ))
  }

  temp_ph[, `:=`(
    value = sub(celsius, "", temp_value),
    unit = "\u00BAC"
  )]

  # Name of the temperature parameter, according to the SNIRH pH parameter
  ph_snirh <- relevant_params$param_snirh[match(temp_ph$parameter, relevant_params$param_lab)]
  temp_ph[, parameter := fcase(
    ph_snirh %in% "pH - campo", "Temperatura do pH - Campo",
    ph_snirh %in% "pH - lab.", "Temperatura do pH",
    default = paste0("Temperatura do ", parameter)
  )]

  # Combine original data with temperature data
  rbindlist(list(data, temp_ph), use.names = TRUE)
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
    "," = ".",                # Replace comma with period
    "=" = "",                 # Remove equals signs
    "\\(LQ\\)" = "",          # Remove detection limit indicators
    "[\u00BA\u00B0]C" = "",   # Remove temperature unit from values
    "Ausente" = "0",          # Convert absence indicators
    "Presente" = ">0",        # Convert presence indicators
    "\u2264" = "<",           # Convert less-than-or-equal symbols
    "\u2265" = ">"            # Convert greater-than-or-equal symbols
  )

  for (pattern in names(cleaning_rules)) {
    replacement <- cleaning_rules[[pattern]]
    data[, value_clean := gsub(pattern, replacement, value_clean)]
  }

  # Handle uncertainty indicators (\u00B1) and temperature ranges ("7.2 a 20.1")
  data[, value_clean := sub("\u00B1.*$", "", value_clean)]
  data[, value_clean := sub("\\s+a\\s+.*$", "", value_clean)]

  # Remove all whitespace
  data[, value_clean := gsub("\\s+", "", value_clean)]

  return(data)
}


#' Convert values to SNIRH units
#' @param data Data.table with cleaned values
#' @param relevant_params Parameters table filtered for sample type
#' @return Data.table with converted values
#' @noRd
#' @keywords internal
convert_units <- function(data, relevant_params) {
  # Separate flags from numeric values
  data[, flag := fifelse(
    substr(value_clean, 1, 1) %in% c("<", ">"),
    substr(value_clean, 1, 1),
    NA_character_
  )]
  data[, value_num := suppressWarnings(as.double(sub("^[<>]", "", value_clean)))]

  # Check for values that are not numeric
  invalid <- data[is.na(value_num)]
  if (nrow(invalid) > 0) {
    invalid_list <- unique(invalid[, paste0(parameter, ": ", value)])
    cli_abort(c(
      "Non-numeric values found [{length(invalid_list)}]:",
      "x" = "{.val {utils::head(invalid_list, 10)}}{if (length(invalid_list) > 10) ' ...'}"
    ))
  }

  # Check for unconverted parameters
  data_not_converted <- data[!relevant_params,
                             on = .(parameter = param_lab, unit = unit_lab)]

  if (nrow(data_not_converted) > 0) {
    failed_params <- unique(data_not_converted[,
                                               paste(parameter, paste0("[", unit, "]"))])
    cli_abort("Parameters not found in conversion table [{length(failed_params)}]: {.val {sort(failed_params)}}")
  }

  # Join with parameters for unit conversion
  data_converted <- merge(data, relevant_params,
                          by.x = c("parameter", "unit"),
                          by.y = c("param_lab", "unit_lab"),
                          all.x = TRUE)

  # Different laboratory parameters can map to the same SNIRH symbol; there
  # can be only one value per station, date and symbol
  dup_keys <- c("station_id", "sampling_date", "snirh_entity", "symbol_snirh")
  dups <- data_converted[duplicated(data_converted, by = dup_keys)]
  if (nrow(dups) > 0) {
    dup_list <- unique(dups[, paste0(station_id, " | ", format(sampling_date), " | ", symbol_snirh)])
    cli_abort(c(
      "More than one value for the same SNIRH parameter (station | date | symbol):",
      "x" = "{.val {utils::head(dup_list, 10)}}{if (length(dup_list) > 10) ' ...'}",
      "i" = "Check for different laboratory parameters that map to the same SNIRH symbol."
    ))
  }

  # Apply conversion factors
  data_converted[, value_converted := value_num * factor]

  # Create final value with flags (avoid scientific notation)
  data_converted[, value_final := paste0(fifelse(is.na(flag), "", flag),
                                         format_number(value_converted))]

  return(data_converted)
}


#' Format numbers without scientific notation and floating point noise
#' @param x Numeric vector
#' @return Character vector
#' @noRd
#' @keywords internal
format_number <- function(x) {
  trimws(formatC(x, format = "fg", digits = 15))
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
  nrow_final <- sum(!is.na(as.matrix(data_formatted[, -(1:2)])))
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

  # Format timestamps
  dt_out[, std := format(sampling_date, "%d/%m/%Y %H:%M")]
  dt_out[, sampling_date := NULL]

  # One block per station, preceded by a "ESTACAO=" line
  blocks <- split(dt_out, by = "station_id", keep.by = FALSE)
  dt_out <- rbindlist(lapply(names(blocks), function(id) {
    rbindlist(list(data.table(std = paste0("ESTACAO=", id)), blocks[[id]]), fill = TRUE)
  }), fill = TRUE)

  # Reorder columns
  setcolorder(dt_out, c("std", "snirh_entity",
                        sort(setdiff(names(dt_out), c("std", "snirh_entity")))))

  # Add network header
  dt_out <- rbindlist(list(
    data.table("std" = paste0("REDE=", network)),
    dt_out
  ), fill = TRUE)

  return(dt_out)
}
