library(testthat)
library(soapr)

test_that("Complex Type Serialization (BDTInteractive)", {
  skip_if_not_live()

  wsdl_path <- test_path("testdata/valid_wsdls/BDTInteractive.xml")

  service <- Service$new(wsdl_path, verbose = FALSE)

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

  # Test no serialization errors
  expect_error(
    service$client$SetAdHocPortfolio(Portfolio = portfolio),
    regexp = "(?i)SOAP Fault.*requires.*login"
  )
})
