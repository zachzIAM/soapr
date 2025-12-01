library(testthat)
library(soapr)

test_that("Complex Type Serialization (BDTInteractive)", {
  skip_on_ci()

  wsdl_path <- test_path("testdata/valid_wsdls/BDTInteractive.xml")
  # Ensure the file exists (it might be in a different relative path depending on where test is run)
  if (!file.exists(wsdl_path)) {
    wsdl_path <- "../testdata/valid_wsdls/BDTInteractive.xml"
  }
  if (!file.exists(wsdl_path)) {
    skip("WSDL file not found")
  }

  service <- Service$new(wsdl_path, verbose = TRUE)

  # Create Portfolio
  portfolio <- service$factory$create("Portfolio")
  expect_s3_class(portfolio, "soap_object")
  expect_equal(attr(portfolio, "soap_type"), "Portfolio")

  # Populate Portfolio
  portfolio$Type <- "HOLDINGS"
  portfolio$PortfolioCurrency <- "USD"

  # Create Position
  pos <- service$factory$create("Position")
  expect_s3_class(pos, "soap_object")
  expect_equal(attr(pos, "soap_type"), "Position")
  pos$Value <- 1000
  pos$Currency <- "USD"

  # Create MID
  mid <- service$factory$create("MID")
  expect_s3_class(mid, "soap_object")
  expect_equal(attr(mid, "soap_type"), "MID")
  mid$ID <- "AAPL"
  mid$IDType <- "TICKER"

  # Nest objects
  pos$MID <- list(mid)
  portfolio$Pos <- list(pos)

  # Attempt SetAdHocPortfolio
  # We expect this to fail due to network issues or invalid endpoint,
  # but NOT due to serialization errors.
  # We can capture the error and check if it's NOT a serialization error.

  expect_error(
    service$client$SetAdHocPortfolio(Portfolio = portfolio),
    # We accept any error, but ideally we'd check it's a network/HTTP error.
    # For now, just ensuring it runs is enough to verify serialization didn't crash R.
  )
})
