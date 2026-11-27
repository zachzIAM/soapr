library(testthat)
library(soapr)

test_that("Country Info Service Integration", {
  skip_if_not_live()
  wsdl_path <- "http://webservices.oorsprong.org/websamples.countryinfo/CountryInfoService.wso?WSDL"

  service <- Service$new(wsdl_path)

  res_capital <- service$client$CapitalCity(sCountryISOCode = "US")

  expect_match(
    res_capital$CapitalCityResult,
    regexp = "(?i)washington"
  )

  # Test CountryCurrency
  res_currency <- service$client$CountryCurrency(sCountryISOCode = "US")
  expect_match(
    res_currency$CountryCurrencyResult$sName,
    regexp = "(?i)dollars"
  )
  expect_match(
    res_currency$CountryCurrencyResult$sISOCode,
    regexp = "(?i)usd"
  )

  # Test ListOfCountryNamesByCode
  res_list <- service$client$ListOfCountryNamesByCode()

  # We just verify we got a response
  expect_gt(length(res_list$ListOfCountryNamesByCodeResult$tCountryCodeAndName), 1)

  # Test result types
  expect_s3_class(res_list, "soap_object")
  expect_equal(
    attr(res_list, "soap_type"),
    "ListOfCountryNamesByCodeResponse"
  )

  expect_s3_class(res_list$ListOfCountryNamesByCodeResult, "soap_object")
  expect_equal(
    attr(res_list$ListOfCountryNamesByCodeResult, "soap_type"),
    "ArrayOftCountryCodeAndName"
  )

  expect_true(all(purrr::map_lgl(
    res_list$ListOfCountryNamesByCodeResult$tCountryCodeAndName,
    ~inherits(.x, "soap_object")
  )))

  expect_true(all(purrr::map_lgl(
    res_list$ListOfCountryNamesByCodeResult$tCountryCodeAndName,
    ~attr(.x, "soap_type") == "tCountryCodeAndName"
  )))

})
