
#' Check internet connectivity
#' @return Logical indicating if internet is available
#' @noRd
check_internet_connection <- function(check_url = "https://snirh.apambiente.pt") {
  # Prefer curl::has_internet() when available
  if (requireNamespace("curl", quietly = TRUE)) {
    return(isTRUE(curl::has_internet()))
  }

  # Base-R fallback: quick connection attempt with a short timeout.
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = 5)

  tryCatch({
    con <- url(check_url, open = "rb")
    close(con)
    TRUE
  }, error = function(e) {
    FALSE
  })
}


#' Check that an argument is a single TRUE/FALSE value
#' @noRd
check_flag <- function(x, arg = deparse(substitute(x))) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    cli_abort("{.arg {arg}} must be a single TRUE/FALSE value.", call = parent.frame())
  }
  invisible(x)
}


#' Check that the SNIRH website is reachable
#'
#' Used to run the examples that download data from SNIRH only when the
#' website can be reached (it may be unreachable from some networks).
#'
#' @return Logical
#' @noRd
snirh_reachable <- function(check_url = "https://snirh.apambiente.pt", timeout = 10) {
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = timeout)

  tryCatch({
    con <- url(check_url, open = "rb")
    close(con)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
}
