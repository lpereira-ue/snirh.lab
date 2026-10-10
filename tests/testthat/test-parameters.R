test_that("bundled parameter table is valid", {
  params <- list_snirh_parameters("all", include_conversion_info = TRUE)
  expect_gt(nrow(params), 0)
  expect_true(all(params$sample_type %in% c("water", "biota", "sediment")))
  expect_type(params$factor, "double")
  expect_false(anyNA(params$factor))
})

test_that("list_snirh_parameters filters and validates arguments", {
  water <- list_snirh_parameters("water")
  expect_named(water, c("sample_type", "param_lab", "param_snirh", "symbol_snirh"))
  expect_true(all(water$sample_type == "water"))
  expect_error(list_snirh_parameters("soil"), "sample_type")
})

test_that("a custom parameter table can be used", {
  f <- tempfile(fileext = ".csv")
  writeLines(c(
    "sample_type,param_lab,unit_lab,symbol_snirh,param_snirh,unit_snirh,factor",
    "water,My pH,-,pH_C_DV,pH - campo,-,1",
    "water,My nitrate,mg/L,NO3,Nitrato,mg/l,\"0,5\""
  ), f)

  old <- options(snirh.lab.parameters = f)
  on.exit({
    options(old)
    unlink(f)
  })

  params <- list_snirh_parameters("water", include_conversion_info = TRUE)
  expect_equal(params$param_lab, c("My nitrate", "My pH"))
  expect_equal(params$factor, c(0.5, 1))

  out <- convert_offline(lab_data(parameter = "My nitrate", unit = "mg/L", value = "10"))
  expect_equal(out$NO3[3], "5")
})

test_that("invalid parameter tables are rejected", {
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f))
  header <- "sample_type,param_lab,unit_lab,symbol_snirh,param_snirh,unit_snirh,factor"

  writeLines(c("sample_type,param_lab,unit_lab", "water,a,b"), f)
  expect_error(snirh.lab:::read_parameters_file(f), "Missing columns")

  writeLines(c(header, "water,a,b,A,a,b,x"), f)
  expect_error(snirh.lab:::read_parameters_file(f), "conversion factors")

  writeLines(c(header, "soil,a,b,A,a,b,1"), f)
  expect_error(snirh.lab:::read_parameters_file(f), "sample types")

  writeLines(c(header, "water,a,b,A,a,b,1", "water,a,b,B,a,b,1"), f)
  expect_error(snirh.lab:::read_parameters_file(f), "Duplicated")
})

test_that("download failures fall back to the bundled table", {
  cache_dir <- tempfile()
  old_env <- Sys.getenv("R_USER_CACHE_DIR", unset = NA)
  Sys.setenv(R_USER_CACHE_DIR = cache_dir)
  old <- options(snirh.lab.parameters = "http://localhost:1/parameters.csv")
  on.exit({
    options(old)
    if (is.na(old_env)) Sys.unsetenv("R_USER_CACHE_DIR") else Sys.setenv(R_USER_CACHE_DIR = old_env)
    unlink(cache_dir, recursive = TRUE)
  })

  expect_message(params <- list_snirh_parameters("all"), "bundled")
  expect_gt(nrow(params), 0)
})

test_that("update_snirh_parameters reloads the table", {
  expect_message(params <- update_snirh_parameters(), "loaded")
  expect_gt(nrow(params), 0)
})
