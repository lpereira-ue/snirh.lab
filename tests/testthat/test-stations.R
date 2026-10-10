test_that("SNIRH station CSV is parsed", {
  st <- snirh.lab:::read_snirh_stations_csv(test_path("fixtures", "stations.csv"))

  # Title lines and trailing "Dados obtidos..." line are removed
  expect_equal(nrow(st), 9)
  expect_false(any(grepl("Dados obtidos", st$station_id)))
  expect_named(st, c("station_id", "station_name", "basin", "altitude_m", "latitude",
                     "longitude", "entity", "district", "municipality", "parish",
                     "river", "drainage_area_km2", "distance_to_mouth_km",
                     "start_date", "end_date", "purpose", "status"))

  valenca <- st[station_id == "01F/01"]
  expect_equal(valenca$station_name, "VALEN\u00C7A")
  expect_equal(valenca$status, "ATIVA")
  expect_equal(valenca$latitude, 42.03715)
  expect_equal(valenca$start_date, as.Date("1988-10-24"))
  expect_true(is.na(valenca$end_date))

  # "-" means missing
  expect_true(is.na(st[station_id == "21C/45", status]))
  expect_equal(st[station_id == "24F/52", status], "DESATIVADA")
})

test_that("station validation uses the cached station list", {
  cache <- snirh.lab:::.snirh_cache
  old <- cache$stations
  cache$stations <- snirh.lab:::read_snirh_stations_csv(test_path("fixtures", "stations.csv"))
  on.exit(cache$stations <- old)

  res <- check_station_status(c("01F/01", " 24F/52 ", "21C/45", "XXX", NA))
  expect_equal(res$station_id, c("01F/01", "24F/52", "21C/45", "XXX"))
  expect_equal(res$found, c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(res$active, c(TRUE, FALSE, FALSE, NA))

  expect_equal(nrow(get_snirh_stations(active_only = TRUE)), 6)

  expect_no_error(convert_to_snirh(lab_data(), "surface.water"))
  expect_error(convert_to_snirh(lab_data(station_id = "XXX"), "surface.water"), "not found")
  expect_error(convert_to_snirh(lab_data(station_id = "24F/52"), "surface.water"), "not active")
})
