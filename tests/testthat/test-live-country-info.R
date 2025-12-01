library(testthat)
library(soapr)
skip_on_ci()

test_that("Country Info Service Integration", {
  wsdl_path <- "http://webservices.oorsprong.org/websamples.countryinfo/CountryInfoService.wso?WSDL"

  service <- Service$new(wsdl_path)

  # Test CapitalCity
  # Note: The WSDL might use different parameter names, we should check,
  # but usually it's sCountryISOCode based on public docs.
  # Let's try with named arguments.

  res_capital <- service$client$CapitalCity(sCountryISOCode = "US")
  # The response might be a simple string or a wrapped object depending on how Factory handles it.
  # Based on previous experience (Calculator), simple types might be returned directly or as list.

  # If it returns a list/soap_object, we need to extract the value.
  # If it returns a simple string (due to our builtin type conversion), we check that.

  # Let's inspect what we get if it fails, but for now expect "Washington"
  # or a list containing "Washington"

  # Actually, let's just print it if it fails to help debugging
  expect_true(
    grepl("Washington", as.character(res_capital)),
    label = "CapitalCity result should contain Washington"
  )

  # Test CountryCurrency
  res_currency <- service$client$CountryCurrency(sCountryISOCode = "US")
  expect_true(
    grepl("Dollars", as.character(res_currency)),
    label = "CountryCurrency result should contain Dollars"
  )

  # Test ListOfCountryNamesByCode
  # This returns a list of objects
  res_list <- service$client$ListOfCountryNamesByCode()
  expect_s3_class(res_list, "soap_object")
  # It should have many elements
  # The structure is likely ListOfCountryNamesByCodeResult -> tCountryCodeAndName -> ...

  # We just verify we got a response
  expect_true(length(res_list) > 0)
})
