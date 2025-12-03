library(testthat)
library(soapr)

test_that("Invalid WSDLs should fail to parse or create Service", {

  wsdl_dir <- test_path("testdata/invalid_wsdls")
  wsdl_files <- list.files(
    wsdl_dir,
    pattern = "\\.(xml|wsdl)$",
    full.names = TRUE
  )

  expect_gt(length(wsdl_files), 0)

  for (wsdl_file in wsdl_files) {
    file_name <- basename(wsdl_file)

    # Attempt to create Service
    expect_error({
      service <- Service$new(wsdl_file, verbose = FALSE)
    })

  }

})
