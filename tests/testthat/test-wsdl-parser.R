library(testthat)

valid_wsdl <- file.path(testthat::test_path(), "testdata", "valid_wsdls", "CountryInfoService.xml")

invalid_files <- dir(file.path(testthat::test_path(), "testdata", "invalid_wsdls"), full.names = TRUE, pattern = "\.xml$")


describe("parse_wsdl", {
  test_that("parses a valid WSDL", {
    wsdl <- parse_wsdl(valid_wsdl)
    expect_type(wsdl, "list")
    expect_s3_class(wsdl$doc, "xml_document")
    expect_true(length(wsdl$binding) > 0)
    expect_true(length(wsdl$service) > 0)
    expect_true(length(wsdl$message) > 0)
    expect_true(length(wsdl$port_type) > 0)
  })

  test_that("invalid WSDLs throw errors", {
    purrr::walk(invalid_files, function(f) {
      expect_error(parse_wsdl(f))
    })
  })
})
