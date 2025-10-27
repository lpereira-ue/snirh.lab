#' Download and parse SNIRH station data from WFS service
#' @param matrix Matrix type
#' @return data.table with station_id and status
#' @noRd
#' @keywords internal
download_snirh_stations <- function(matrix) {
  if (!matrix %in% names(SNIRH_STATIONS_URL)) {
    cli::cli_abort("Station validation not available for matrix type: {.val {matrix}}")
  }
  
  wfs_url <- "https://sniambgeoogc.apambiente.pt/getogc/services/SNIAmb/Estacoes_monitorizacao_qualidade_aguas_superficias/MapServer/WFSServer?SERVICE=WFS&REQUEST=GetCapabilities"
  layer_name <- "SNIAmb_Estacoes_monitorizacao_qualidade_aguas_superficias:Est._de_monitoriza\u00e7\u00e3o_da_qualidade_das_\u00e1guas_superficiais"
  
  stations_sf <- tryCatch({
    sf::st_read(wfs_url, layer = layer_name, quiet = TRUE)
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to download SNIRH station data from WFS service.",
      "i" = "Error: {.text {e$message}}",
      "i" = "Verify the WFS is reachable and try again."
    ))
  })
  
  # Rename columns to ASCII equivalents immediately
  names(stations_sf)[names(stations_sf) == "C\u00f3digo"] <- "codigo"
  names(stations_sf)[names(stations_sf) == "Estado"] <- "estado"
  
  # Defensive check that the expected columns exist
  need <- c("codigo", "estado")
  miss <- setdiff(need, names(stations_sf))
  if (length(miss)) {
    cli::cli_abort(c(
      "Required columns not found in WFS layer.",
      "x" = "Missing: {.val {miss}}",
      "i" = "Available: {.val {names(stations_sf)}}"
    ))
  }
  
  # Build the output using ASCII column names
  dt <- data.table::as.data.table(stations_sf)[, .(
    station_id = codigo,
    status     = estado
  )][,
     let(
       station_id = trimws(station_id),
       status = trimws(status))
    ]
  
  dt[]
}
