library(testthat)
library(soapr)

test_that("Temperature Conversion Service Integration", {
  skip_if_not_live()
  wsdl_path <- "https://www.w3schools.com/xml/tempconvert.asmx?WSDL"

  service <- Service$new(wsdl_path)

  # Test CelsiusToFahrenheit
  # 100 C -> 212 F
  res_f <- service$client$CelsiusToFahrenheit(Celsius = "100")

  # Expect "212"
  # It returns a string usually
  expect_equal(res_f$CelsiusToFahrenheitResult, "212")
  expect_equal(as.character(res_f), "212")

  # Test FahrenheitToCelsius
  # 212 F -> 100 C
  res_c <- service$client$FahrenheitToCelsius(Fahrenheit = "212")
  expect_equal(res_c$FahrenheitToCelsiusResult, "100")
  expect_equal(as.character(res_c), "100")
})
