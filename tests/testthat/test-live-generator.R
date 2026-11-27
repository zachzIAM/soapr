library(testthat)
library(soapr)

test_that("Static Client Generator (BDTInteractive)", {
  skip_if_not_live()
  wsdl_path <- test_path("testdata/valid_wsdls/BDTInteractive.xml")

  output_path <- tempfile("generated_packages")
  dir.create(output_path)
  on.exit(unlink(output_path, recursive = TRUE))

  package_name <- "BDTIClient"

  # Generate package
  wsdl <- WSDLParser$new(wsdl_path)
  gen <- Generator$new(wsdl)
  expect_error(gen$generate_package(output_path, package_name), NA)

  pkg_dir <- file.path(output_path, package_name)
  expect_true(dir.exists(pkg_dir))

  # Check files
  expect_true(file.exists(file.path(pkg_dir, "DESCRIPTION")))
  expect_true(file.exists(file.path(pkg_dir, "NAMESPACE")))
  expect_true(file.exists(file.path(pkg_dir, "R", "types.R")))
  expect_true(file.exists(file.path(pkg_dir, "R", "client.R")))

  # We can check if the code parses at least
  expect_error(parse(file = file.path(pkg_dir, "R", "types.R")), NA)
  expect_error(parse(file = file.path(pkg_dir, "R", "client.R")), NA)

  # Try to load it
  pkgload::load_all(pkg_dir)
  expect_true(exists(
    "new_client",
    where = as.environment(paste0("package:", package_name))
  ))

  # Create a client and check headers
  client <- new_client(config = list(foo = "bar"))
  testthat::expect_s3_class(client, "Client")
  expect_true("headers" %in% names(client))
  expect_true(is.list(client$headers))
  expect_true("config" %in% names(client))
  expect_equal(client$config$foo, "bar")

  # Test add_header
  header <- xml2::xml_new_root("TestHeader")
  client$add_header(header)
  expect_length(client$headers, 1)

  # Test transport injection
  mock_transport <- function(url, action, body, config, handle) {
    list(
      status_code = 200,
      content = charToRaw("<response>ok</response>")
    )
  }

  client_mock <- new_client(transport = mock_transport)
  expect_equal(client_mock$transport, mock_transport)

  client_mock <- new_client(transport = mock_transport)
  expect_equal(client_mock$transport, mock_transport)

  # Test Boolean Serialization
  bool_mock <- function(url, action, body, config, handle) {
    if (grepl("GetCurrentSettings", action)) {
      # Check that boolean TRUE is serialized as "true" (lowercase)
      testthat::expect_true(grepl("<InclSessionOverride>true</InclSessionOverride>", body))
    }
    structure(
      list(
        url = url,
        status_code = 200,
        headers = list(`Content-Type` = "text/xml; charset=utf-8"),
        content = charToRaw('<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/"><soap:Body><response>ok</response></soap:Body></soap:Envelope>')
      ),
      class = "response"
    )
  }
  client_bool <- new_client(transport = bool_mock)
  client_bool$GetCurrentSettings(InclSessionOverride = TRUE)

  # To fully test it, we would need to call an operation.
  # But we don't know the operation names easily here without parsing WSDL again.
  # However, we know the generated code uses it.

  pkgload::unload(package_name)
})
