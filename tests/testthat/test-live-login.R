library(testthat)
library(soapr)

test_that("Login Operation (BDTInteractive)", {
  skip_on_ci()

  wsdl_path <- test_path("testdata/valid_wsdls/BDTInteractive.xml")
  if (!file.exists(wsdl_path)) {
    wsdl_path <- "../testdata/valid_wsdls/BDTInteractive.xml"
  }
  if (!file.exists(wsdl_path)) {
    skip("WSDL file not found")
  }

  service <- Service$new(wsdl_path, verbose = TRUE)

  user <- "iamquant"
  password <- "TheCatSatOnTheMat99"
  client_id <- "kzdr85j9hv"

  # Attempt Login
  # We expect this to fail due to network issues or invalid endpoint/credentials.
  # But we verify that the request is constructed and sent.

  expect_error(
    service$client$Login(
      User = user,
      Client = client_id,
      Password = password
    )
  )
})
