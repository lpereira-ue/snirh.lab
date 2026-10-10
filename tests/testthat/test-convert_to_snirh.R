test_that("output follows the SNIRH template", {
  d <- lab_data(
    station_id = c("25G/07", "25G/07", "01F/01"),
    sampling_date = as.POSIXct(c("2024-02-15 09:00", "2024-01-15 10:30", "2024-01-15 10:30"), tz = "UTC")
  )
  out <- convert_offline(d)

  expect_named(out, c("STD", "RESPONSAVEL", "pH_C_DV"))
  expect_equal(out$STD, c("REDE=QUAL",
                          "ESTACAO=01F/01", "15/01/2024 10:30",
                          "ESTACAO=25G/07", "15/01/2024 10:30", "15/02/2024 09:00"))
  expect_equal(out$RESPONSAVEL, c(NA, NA, "APA", NA, "APA", "APA"))
  expect_equal(out$pH_C_DV, c(NA, NA, "7.2", NA, "7.2", "7.2"))
})

test_that("input data is not modified", {
  d <- lab_data(value = " 7,2 ")
  d_copy <- data.table::copy(d)
  convert_offline(d)
  expect_identical(d, d_copy)
})

test_that("values are cleaned, flagged and converted without scientific notation", {
  d <- lab_data(
    parameter = "2,4,5-Triclorofenol", unit = "ng/L",
    station_id = c("01F/01", "07G/50", "25G/07", "16H/03"),
    value = c("<0,5", "12,3 (LQ)", "= 0.07", "1.5 \u00B1 0.2")
  )
  out <- convert_offline(d)
  vals <- out[!is.na(RESPONSAVEL), `_245CLFENOL`]
  expect_equal(vals, c("<0.0005", "0.0123", "0.0015", "0.00007"))
})

test_that("presence/absence and comparison symbols are converted", {
  d <- lab_data(
    parameter = "2,4,5-Triclorofenol", unit = "ng/L",
    station_id = c("01F/01", "07G/50"),
    value = c("\u2264 2", "\u2265 3")
  )
  out <- convert_offline(d)
  expect_equal(out[!is.na(RESPONSAVEL), `_245CLFENOL`], c("<0.002", ">0.003"))
})

test_that("pH temperature is extracted (ordinal and degree signs)", {
  d <- lab_data(
    parameter = c("pH - Campo", "pH (LSA)"),
    value = c("7.2 a 20,1 \u00BAC", "6.9  a  19 \u00B0C")
  )
  out <- convert_offline(d)
  expect_equal(out[3, pH_C_DV], "7.2")
  expect_equal(out[3, pH_L_DV], "6.9")
  expect_equal(out[3, TEMP_pH_C], "20.1")
  expect_equal(out[3, TEMP_pH_L], "19")
})

test_that("pH temperature in a unit other than Celsius is rejected", {
  d <- lab_data(value = "7.2 a 68 \u00BAF")
  expect_error(convert_offline(d), "Celsius")
})

test_that("parameters without unit in the table may have an empty unit", {
  d <- lab_data(parameter = "Turva\u00E7\u00E3o (LSA)", unit = NA_character_, value = "4")
  out <- convert_offline(d)
  expect_equal(out[3, TURV_DV], "4")

  d <- lab_data(unit = "")
  expect_error(convert_offline(d), "missing values")
})

test_that("Date and character sampling dates are accepted", {
  d <- lab_data(sampling_date = as.Date("2024-01-15"))
  expect_equal(convert_offline(d)$STD[3], "15/01/2024 00:00")

  d <- lab_data(
    station_id = c("01F/01", "07G/50", "25G/07"),
    sampling_date = c("2024-01-15 10:30", "15/01/2024 10:30", "2024-01-15")
  )
  expect_equal(convert_offline(d)$STD[c(3, 5, 7)],
               c("15/01/2024 10:30", "15/01/2024 10:30", "15/01/2024 00:00"))

  d <- lab_data(sampling_date = "15-13-2024")
  expect_error(convert_offline(d), "parse")
})

test_that("invalid inputs are rejected", {
  expect_error(convert_offline(list(a = 1)), "data.frame")
  expect_error(convert_to_snirh(lab_data(), "groundwater"), "matrix")
  expect_error(convert_to_snirh(lab_data(), "surface.water", validate_stations = NA), "TRUE/FALSE")

  d <- lab_data()
  d[, extra := 1]
  expect_error(convert_offline(d), "Column names")

  expect_error(convert_offline(lab_data()[0]), "empty")
})

test_that("empty rows are removed", {
  d <- rbind(lab_data(), lab_data(snirh_entity = "", station_id = NA, sampling_date = as.POSIXct(NA),
                                  parameter = " ", unit = NA, value = ""))
  expect_equal(nrow(convert_offline(d)), 3)
})

test_that("duplicates are rejected", {
  d <- rbind(lab_data(), lab_data(value = "7.3"))
  expect_error(convert_offline(d), "Duplicate")

  # Different laboratory parameters mapping to the same SNIRH symbol
  d <- lab_data(parameter = c("pH - Campo", "pH - Campo (ARHC)"))
  expect_error(convert_offline(d), "same SNIRH parameter")
})

test_that("unknown parameters and non-numeric values are rejected", {
  expect_error(convert_offline(lab_data(parameter = "Unknown")), "not found in conversion table")
  expect_error(convert_offline(lab_data(value = "n.d.")), "Non-numeric")
})

test_that("biota uses the biota parameters", {
  p <- list_snirh_parameters("biota", include_conversion_info = TRUE)[!is.na(unit_lab)][1]
  d <- lab_data(parameter = p$param_lab, unit = p$unit_lab, value = "1")
  out <- convert_offline(d, "biota")
  expect_true(p$symbol_snirh %in% names(out))
  expect_error(convert_offline(d, "surface.water"), "not found in conversion table")
})
