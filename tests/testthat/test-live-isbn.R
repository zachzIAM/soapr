library(testthat)
library(soapr)
skip_on_ci()

test_that("ISBN Service Integration", {
  wsdl_path <- "http://webservices.daehosting.com/services/isbnservice.wso?WSDL"

  service <- Service$new(wsdl_path)

  # Test Valid ISBN
  res_valid <- service$client$IsValidISBN13(sISBN = "9780596520687")
  expect_s3_class(res_valid, "soap_object")
  expect_equal(attr(res_valid, "soap_type"), "IsValidISBN13Response")
  expect_true(res_valid$IsValidISBN13Result)

  # Test Invalid ISBN
  res_invalid <- service$client$IsValidISBN13(sISBN = "1234567890123")
  expect_s3_class(res_invalid, "soap_object")
  expect_equal(attr(res_invalid, "soap_type"), "IsValidISBN13Response")
  expect_false(res_invalid$IsValidISBN13Result)
})
