library(testthat)
library(soapr)

test_that("All testdata WSDLs can be parsed and Service created", {

  wsdl_dir <- test_path("testdata/valid_wsdls")
  wsdl_files <- list.files(
    wsdl_dir,
    pattern = "\\.(xml|wsdl)$",
    full.names = TRUE
  )

  expect_gt(length(wsdl_files), 0)

  for (wsdl_file in wsdl_files) {
    file_name <- basename(wsdl_file)

    # Attempt to create Service
    expect_no_error({
      service <- Service$new(wsdl_file, verbose = FALSE)
    })

    # Basic assertions
    expect_true(inherits(service, "Service"))
    expect_true(inherits(service$wsdl, "WSDLParser"))
    expect_true(inherits(service$factory, "Factory"))
    expect_true(inherits(service$client, "Client"))

    # Check if services and ports were found
    services <- service$wsdl$get_services()
    expect_gt(length(services), 0)
  }

})
