
#' Check internet connectivity
#' @return Logical indicating if internet is available
#' @noRd
check_internet_connection <- function() {
  if (!requireNamespace("curl", quietly = TRUE)) {
    message("Package 'curl' is not installed. Please install it with:\n  install.packages('curl')")
    return(FALSE)
  }
  curl::has_internet()
}
