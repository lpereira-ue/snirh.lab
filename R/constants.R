# Package constants ----

VALID_MATRICES <- c("surface.water", "biota")

REQUIRED_COLUMNS <- c("snirh_entity", "station_id", "sampling_date",
                      "parameter", "unit", "value")

# Network configuration
NETWORK_CONFIG <- list(
  "surface.water" = list(network = "QUAL", sample_type = "water", validate_stations = TRUE),
  "biota" = list(network = "QUAL", sample_type = "biota", validate_stations = TRUE)
)

# SNIRH station list (CSV export of the SNIRH website). The same list covers
# surface water and biota stations.
SNIRH_STATIONS_URL <- paste0(
  "https://snirh.apambiente.pt/snirh/_dadosbase/site/paraCSV/lista_csv.php?",
  "obj_janela=INFO_ESTACOES&s_cover=5453&tp_lista=&completa=1&formato=csv"
)

# Columns of the SNIRH station CSV, in order
SNIRH_STATIONS_COLUMNS <- c(
  "station_id", "station_name", "basin", "altitude_m", "coord_x_m",
  "coord_y_m", "latitude", "longitude", "entity", "district",
  "municipality", "parish", "river", "drainage_area_km2",
  "distance_to_mouth_km", "start_date", "end_date", "purpose", "status"
)

# Parameter conversion table: default remote location. Can be overridden with
# options(snirh.lab.parameters = "<url or local path>").
SNIRH_PARAMETERS_URL <-
  "https://raw.githubusercontent.com/lpereira-ue/snirh.lab/main/inst/extdata/parameters.csv"

PARAMETERS_COLUMNS <- c("sample_type", "param_lab", "unit_lab", "symbol_snirh",
                        "param_snirh", "unit_snirh", "factor")

VALID_SAMPLE_TYPES <- c("water", "biota", "sediment")

# Session cache (station list and parameter table)
.snirh_cache <- new.env(parent = emptyenv())
