library(testthat)
library(soapr)

test_that("Static Client Generator works for Calculator service", {
  skip_on_cran()

  # Use a temporary directory for generation
  temp_dir <- tempdir()
  pkg_name <- "calculator"
  pkg_path <- file.path(temp_dir, pkg_name)

  # Clean up if exists
  if (dir.exists(pkg_path)) {
    unlink(pkg_path, recursive = TRUE)
  }

  # Generate
  wsdl_url <- "http://www.dneonline.com/calculator.asmx?WSDL"
  wsdl <- WSDLParser$new(wsdl_url)
  gen <- Generator$new(wsdl)
  gen$generate_package(temp_dir, pkg_name)

  # Install and Load
  pkgload::load_all(pkg_path, quiet = TRUE)

  # Create Client
  client <- calculator::new_client()

  # Test Add
  res <- client$Add(intA = 10, intB = 20)
  expect_equal(res$AddResult, 30)

  # Test Subtract
  res <- client$Subtract(intA = 50, intB = 20)
  expect_equal(res$SubtractResult, 30)
})

test_that("Static Client Generator works for NumberConversion service", {
  skip_on_cran()

  temp_dir <- tempdir()
  pkg_name <- "numberconversion"
  pkg_path <- file.path(temp_dir, pkg_name)

  if (dir.exists(pkg_path)) {
    unlink(pkg_path, recursive = TRUE)
  }

  wsdl_url <- "http://www.dataaccess.com/webservicesserver/NumberConversion.wso?WSDL"

  wsdl <- WSDLParser$new(wsdl_url)
  gen <- Generator$new(wsdl)
  gen$generate_package(temp_dir, pkg_name)

  pkgload::load_all(pkg_path, quiet = TRUE)

  client <- numberconversion::new_client()

  # Test NumberToWords
  res <- client$NumberToWords(ubiNum = 123)
  expect_match(
    res$NumberToWordsResult,
    regexp = "(?i)one hundred and twenty three"
  )

})
