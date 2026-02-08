
#' Check internet connectivity
#' @return Logical indicating if internet is available
#' @noRd
check_internet_connection <- function(check_url = "https://sniambgeoogc.apambiente.pt") {
  # Prefer curl::has_internet() when available
  if (requireNamespace("curl", quietly = TRUE)) {
    return(isTRUE(curl::has_internet()))
  }

  # Base-R fallback: quick connection attempt with a short timeout.
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = 5)

  has_net <- tryCatch({
    con <- url(check_url, open = "rb")
    on.exit(close(con), add = TRUE)
    readBin(con, what = "raw", n = 1L)
    TRUE
  }, error = function(e) {
    FALSE
  })

  has_net
}