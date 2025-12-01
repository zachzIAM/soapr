library(testthat)
testthat::skip_on_ci()

test_that("Calculator service works", {
  wsdl_url <- "http://www.dneonline.com/calculator.asmx?WSDL"

  service <- Service$new(wsdl_url, verbose = FALSE)
  client <- service$client

  # Add
  res_add <- client$Add(intA = 10, intB = 5)
  expect_equal(as.integer(res_add$AddResult), 15)

  # Subtract
  res_sub <- client$Subtract(intA = 10, intB = 5)
  expect_equal(as.integer(res_sub$SubtractResult), 5)

  # Multiply
  res_mul <- client$Multiply(intA = 10, intB = 5)
  expect_equal(as.integer(res_mul$MultiplyResult), 50)

  # Divide
  res_div <- client$Divide(intA = 10, intB = 5)
  expect_equal(as.integer(res_div$DivideResult), 2)
})
