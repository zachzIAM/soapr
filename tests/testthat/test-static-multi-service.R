library(testthat)
library(soapr)

test_that("Static Client Generator support for multiple services and disjoint operations", {
  wsdl_path <- test_path("testdata/valid_wsdls/MultiService.xml")

  # Generate package
  temp_dir <- tempfile("MultiServicePkg")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  generator <- Generator$new(WSDLParser$new(wsdl_path))
  generator$generate_package(temp_dir, "MultiServicePkg")

  pkg_path <- file.path(temp_dir, "MultiServicePkg")
  expect_true(dir.exists(pkg_path))

  # Load generated package
  pkgload::load_all(pkg_path, quiet = TRUE)
  on.exit(pkgload::unload("MultiServicePkg"), add = TRUE)

  # Check method existence regardless of initialization
  # We should be able to create a client (defaulting to first service/port)
  client <- MultiServicePkg::new_client()

  # Both methods should exist in the client environment/class
  expect_true(exists("OpA", envir = client))
  expect_true(exists("OpB", envir = client))

  # If we call OpA it should work (default is ServiceA/PortA)
  # Mock transport
  client$transport <- function(...) {
    structure(
      list(
        status_code = 200,
        headers = list(`Content-Type` = "text/xml; charset=utf-8"),
        content = charToRaw(paste0(
          '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">',
          "<soap:Body><response>ok</response></soap:Body></soap:Envelope>"
        ))
      ),
      class = "response"
    )
  }
  expect_error(client$OpA(), NA)

  # Calling OpB on PortA should fail at runtime because PortA does not support it:
  # generated OpB looks up the operation metadata in self$port_def$operations and errors.
  expect_error(client$OpB(), "Operation 'OpB' is not supported by port")

  # Now switch to ServiceB/PortB
  clientB <- MultiServicePkg::new_client(service = "ServiceB", port = "PortB")
  clientB$transport <- client$transport

  expect_error(clientB$OpB(), NA)
  expect_error(clientB$OpA(), "Operation 'OpA' is not supported by port")
})
