library(testthat)
library(soapr)
library(xml2)

test_that("WS-Security Header Generation", {
  # Use a simple WSDL for testing
  wsdl_file <- "testdata/valid_wsdls/CountryInfoService.xml"

  # Mock transport to capture request
  captured_request <- NULL
  mock_transport <- function(url, handle, body, ...) {
    captured_request <<- body
    # Return a dummy response
    structure(
      list(
        url = url,
        status_code = 200,
        headers = list(`Content-Type` = "text/xml"),
        content = charToRaw(body)
      ),
      class = "response"
    )
  }

  # Create Client
  # We need to parse the WSDL first
  # Adjust path if running from test directory
  if (!file.exists(wsdl_file)) {
    wsdl_file <- file.path(
      "..",
      "testdata",
      "valid_wsdls",
      "CountryInfoService.xml"
    )
  }
  # If still not found (e.g. running from package root), try full path or relative
  if (!file.exists(wsdl_file)) {
    wsdl_file <- "tests/testthat/testdata/valid_wsdls/CountryInfoService.xml"
  }

  wsdl <- WSDLParser$new(wsdl_file)
  service <- wsdl$get_services()[[1]]
  port <- service$ports[[1]]

  # We need a factory too
  factory <- Factory$new(wsdl)

  client <- Client$new(wsdl, port, factory, transport = mock_transport)

  # Add WSSE Header
  wsse <- WSSecurity$new()$username_token("testuser", "secret123")
  client$add_header(wsse)

  # Call an operation
  # CountryInfoService has 'ListOfContinentsByName'
  res <- client$ListOfContinentsByName()

  # Inspect the captured request XML
  expect_false(is.null(captured_request))
  doc <- read_xml(captured_request)

  # Check for Header
  header <- xml_find_first(doc, "//soap:Header")
  expect_false(inherits(header, "xml_missing"))

  # Check for Security
  security <- xml_find_first(header, ".//*[local-name()='Security']")
  expect_false(inherits(security, "xml_missing"))

  # Check Username
  username <- xml_find_first(security, ".//*[local-name()='Username']")
  expect_equal(xml_text(username), "testuser")

  # Check Password
  password <- xml_find_first(security, ".//*[local-name()='Password']")
  expect_equal(xml_text(password), "secret123")
  expect_match(xml_attr(password, "Type"), "PasswordText")
})
