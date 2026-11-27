test_that("Client sends requests and parses responses correctly", {
  wsdl_path <- testthat::test_path("testdata/valid_wsdls/BDTInteractive.xml")
  wsdl <- WSDLParser$new(wsdl_path)
  factory <- Factory$new(wsdl)

  # Mock Transport
  mock_transport <- function(url, handle, body, ...) {
    # Verify URL
    expect_match(url, "https://www.barraone.com:443/bdti/BDTInteractive")

    # Verify Body (Request XML)
    doc <- xml2::read_xml(body)
    ns <- xml2::xml_ns(doc)

    # Check Envelope
    expect_equal(xml2::xml_name(xml2::xml_root(doc)), "Envelope")

    # Check Body
    body_node <- xml2::xml_find_first(doc, ".//soap:Body", ns)
    expect_false(inherits(body_node, "xml_missing"))

    # Check Operation
    op_node <- xml2::xml_children(body_node)[[1]]
    expect_equal(xml2::xml_name(op_node), "Login")
    expect_equal(
      xml2::xml_attr(op_node, "xmlns"),
      "http://barra.com/cp/bdti"
    )

    # Check Arguments
    # Use local-name() to avoid namespace issues in test
    user_node <- xml2::xml_find_first(op_node, "./*[local-name()='User']")
    expect_false(inherits(user_node, "xml_missing"))
    expect_equal(xml2::xml_text(user_node), "testuser")

    # Return Mock Response
    # LoginResponse is empty in WSDL, so we return empty body
    response_xml <- paste0(
      '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" ',
      'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ',
      'xmlns:xsd="http://www.w3.org/2001/XMLSchema">',
      "<soap:Body>",
      '<LoginResponse xmlns="http://barra.com/cp/bdti" />',
      "</soap:Body>",
      "</soap:Envelope>"
    )

    # Mock httr response object
    structure(
      list(
        url = url,
        status_code = 200,
        headers = list(`Content-Type` = "text/xml; charset=utf-8"),
        content = charToRaw(response_xml)
      ),
      class = "response"
    )
  }

  # Initialize Client with mock transport
  # We need to manually pick the port def as Service does
  services <- wsdl$get_services()
  port_def <- services[[1]]$ports[[1]]

  client <- Client$new(wsdl, port_def, factory, transport = mock_transport)

  # Call Operation
  result <- client$Login(
    User = "testuser",
    Password = "password",
    Client = "client"
  )

  # Verify Result
  expect_s3_class(result, "soap_object")
  expect_equal(attr(result, "soap_type"), "LoginResponse")
  # LoginResponse is empty, so result should be empty list (but with attributes)
  expect_length(result, 0)
})

test_that("Client handles SOAP Faults correctly", {
  wsdl_path <- testthat::test_path("testdata/valid_wsdls/BDTInteractive.xml")
  wsdl <- WSDLParser$new(wsdl_path)
  factory <- Factory$new(wsdl)
  services <- wsdl$get_services()
  port_def <- services[[1]]$ports[[1]]

  mock_fault_transport <- function(url, handle, body, ...) {
    response_xml <- '
    <soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
      <soap:Body>
        <soap:Fault>
          <faultcode>soap:Server</faultcode>
          <faultstring>Invalid credentials</faultstring>
        </soap:Fault>
      </soap:Body>
    </soap:Envelope>
    '

    structure(
      list(
        url = url,
        status_code = 500,
        headers = list(`Content-Type` = "text/xml; charset=utf-8"),
        content = charToRaw(response_xml)
      ),
      class = "response"
    )
  }

  client <- Client$new(
    wsdl,
    port_def,
    factory,
    transport = mock_fault_transport
  )

  expect_error(
    client$Login(User = "bad", Password = "bad", Client = "bad"),
    "SOAP Fault: \\[soap:Server\\] Invalid credentials"
  )
})
