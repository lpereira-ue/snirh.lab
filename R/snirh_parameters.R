#' Update the SNIRH parameter conversion table
#'
#' Downloads the latest version of the parameter conversion table and stores
#' it in the local cache, so that it is used by [convert_to_snirh()] and
#' [list_snirh_parameters()]. The table is maintained outside the package code,
#' so new parameters or corrected conversion factors become available without
#' installing a new version of the package.
#'
#' @return Invisibly, a data.table with the parameter conversion table.
#'
#' @details
#' You usually do not need to call this function: the first time the parameter
#' table is needed in an R session, the package tries to download the latest
#' version automatically. If the download fails (e.g. no internet connection),
#' the most recent copy in the local cache is used; if there is none, the
#' version bundled with the package is used. Use this function to reload the
#' table during a session (e.g. after it was updated in the repository).
#'
#' Downloaded tables are cached in `tools::R_user_dir("snirh.lab", "cache")`.
#'
#' To use your own table (e.g. with laboratory-specific parameter names), set
#' the option `snirh.lab.parameters` to its URL or local path:
#' \preformatted{options(snirh.lab.parameters = "path/to/my_parameters.csv")}
#'
#' @section Parameter table format:
#' A CSV file (UTF-8, comma separated, `.` as decimal mark) with one row per
#' laboratory parameter/unit and the following columns:
#' \describe{
#'   \item{sample_type}{Type of sample matrix: `water`, `biota` or `sediment`.}
#'   \item{param_lab}{Parameter name as provided by the laboratory.}
#'   \item{unit_lab}{Unit of measurement as provided by the laboratory. May be
#'     empty for parameters without units.}
#'   \item{symbol_snirh}{Parameter symbol used in the SNIRH database.}
#'   \item{param_snirh}{Parameter name used in SNIRH.}
#'   \item{unit_snirh}{Unit used in the SNIRH database.}
#'   \item{factor}{Conversion factor from laboratory to SNIRH units:
#'     `snirh_value = lab_value * factor`.}
#' }
#' Additional columns (e.g. `notes`) are allowed and ignored. Each combination
#' of `sample_type`, `param_lab` and `unit_lab` must be unique.
#'
#' @examples
#' \donttest{
#' params <- update_snirh_parameters()
#' params[sample_type == "water"]
#' }
#'
#' @seealso [list_snirh_parameters()] to browse the parameter table.
#'
#' @importFrom cli cli_abort cli_alert_success cli_alert_warning
#' @export
update_snirh_parameters <- function() {
  .snirh_cache$params <- NULL
  params <- load_snirh_parameters()

  if (!isTRUE(attr(params, "fallback"))) {
    source <- .snirh_cache$params_source
    cli_alert_success("Parameter table loaded from {.path {source}} ({nrow(params)} rows).")
  }

  invisible(params)
}


#' Get the parameter conversion table
#'
#' Returns the table cached in the session. On first use, tries the configured
#' source and falls back to the local cache and then to the bundled table.
#'
#' @return data.table with the parameter conversion table
#' @noRd
#' @keywords internal
load_snirh_parameters <- function() {
  source <- getOption("snirh.lab.parameters", SNIRH_PARAMETERS_URL)
  if (!is.character(source) || length(source) != 1L || is.na(source)) {
    cli_abort("Option {.code snirh.lab.parameters} must be a single URL or file path.")
  }

  if (!is.null(.snirh_cache$params) && identical(.snirh_cache$params_source, source)) {
    return(copy(.snirh_cache$params))
  }

  if (is_url(source)) {
    params <- tryCatch(download_parameters(source), error = function(e) e)

    if (inherits(params, "error")) {
      params <- load_fallback_parameters(conditionMessage(params))
    } else {
      write_parameters_cache(params)
    }
  } else {
    # A local file set explicitly by the user: errors should not be hidden
    params <- read_parameters_file(source)
  }

  .snirh_cache$params <- params
  .snirh_cache$params_source <- source

  copy(params)
}


#' Load the cached or bundled parameter table when the download fails
#' @noRd
load_fallback_parameters <- function(reason) {
  cache_file <- parameters_cache_file()

  if (file.exists(cache_file)) {
    params <- tryCatch(read_parameters_file(cache_file), error = function(e) NULL)
    if (!is.null(params)) {
      cache_date <- format(file.mtime(cache_file), "%Y-%m-%d")
      cli_alert_warning(paste(
        "Could not download the latest parameter table ({reason}).",
        "Using the cached copy from {cache_date}."
      ))
      setattr(params, "fallback", TRUE)
      return(params)
    }
  }

  cli_alert_warning(paste(
    "Could not download the latest parameter table ({reason}).",
    "Using the table bundled with the package."
  ))
  params <- read_parameters_file(bundled_parameters_file())
  setattr(params, "fallback", TRUE)
  params
}


#' Download and validate the parameter table
#' @noRd
download_parameters <- function(url) {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = max(30, old_timeout))

  status <- tryCatch(
    download.file(url, tmp, mode = "wb", quiet = TRUE),
    error = function(e) conditionMessage(e),
    warning = function(w) conditionMessage(w)
  )
  if (!identical(status, 0L)) {
    cli_abort("Failed to download the parameter table from {.url {url}}: {status}")
  }

  read_parameters_file(tmp, label = url)
}


#' Read and validate a parameter table file
#' @noRd
read_parameters_file <- function(file, label = file) {
  if (!file.exists(file)) {
    cli_abort("Parameter table not found: {.path {label}}")
  }

  params <- tryCatch(
    fread(file, encoding = "UTF-8", colClasses = "character",
          na.strings = c("", "NA"), showProgress = FALSE),
    error = function(e) {
      cli_abort("Could not read the parameter table {.path {label}}: {conditionMessage(e)}")
    }
  )

  validate_parameters_table(params, label)
}


#' Validate the structure and content of the parameter table
#' @noRd
validate_parameters_table <- function(params, label = "parameter table") {
  missing_cols <- setdiff(PARAMETERS_COLUMNS, names(params))
  if (length(missing_cols) > 0) {
    cli_abort(c(
      "Invalid parameter table {.path {label}}.",
      "x" = "Missing columns: {.val {missing_cols}}"
    ))
  }

  params <- as.data.table(params)
  chr_cols <- setdiff(PARAMETERS_COLUMNS, "factor")
  params[, (chr_cols) := lapply(.SD, function(x) trimws(as.character(x))), .SDcols = chr_cols]
  params[unit_lab == "", unit_lab := NA_character_]
  params[, factor := suppressWarnings(as.numeric(sub(",", ".", factor, fixed = TRUE)))]

  if (anyNA(params$factor)) {
    bad <- params[is.na(factor), param_lab]
    cli_abort(c(
      "Invalid parameter table {.path {label}}.",
      "x" = "Missing or non-numeric conversion factors: {.val {bad}}"
    ))
  }

  bad_types <- setdiff(unique(params$sample_type), VALID_SAMPLE_TYPES)
  if (length(bad_types) > 0) {
    cli_abort(c(
      "Invalid parameter table {.path {label}}.",
      "x" = "Invalid sample types: {.val {bad_types}}",
      "i" = "Valid sample types: {.val {VALID_SAMPLE_TYPES}}"
    ))
  }

  incomplete <- params[is.na(param_lab) | param_lab == "" |
                         is.na(symbol_snirh) | symbol_snirh == ""]
  if (nrow(incomplete) > 0) {
    cli_abort(c(
      "Invalid parameter table {.path {label}}.",
      "x" = "{nrow(incomplete)} row{?s} without {.field param_lab} or {.field symbol_snirh}."
    ))
  }

  dups <- params[duplicated(params, by = c("sample_type", "param_lab", "unit_lab"))]
  if (nrow(dups) > 0) {
    dup_ids <- unique(dups[, paste0(param_lab, " [", unit_lab, "] (", sample_type, ")")])
    cli_abort(c(
      "Invalid parameter table {.path {label}}.",
      "x" = "Duplicated sample_type + param_lab + unit_lab: {.val {dup_ids}}"
    ))
  }

  params[]
}


#' @noRd
is_url <- function(x) {
  grepl("^(https?|ftp)://", x, ignore.case = TRUE)
}

#' @noRd
bundled_parameters_file <- function() {
  system.file("extdata", "parameters.csv", package = "snirh.lab", mustWork = TRUE)
}

#' @noRd
parameters_cache_file <- function() {
  file.path(tools::R_user_dir("snirh.lab", which = "cache"), "parameters.csv")
}

#' Save the parameter table to the local cache (failures are not fatal)
#' @noRd
write_parameters_cache <- function(params) {
  cache_file <- parameters_cache_file()
  tryCatch({
    dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
    fwrite(params, cache_file)
  }, error = function(e) {
    cli_alert_warning("Could not save the parameter table to the local cache: {conditionMessage(e)}")
  })
  invisible(cache_file)
}
