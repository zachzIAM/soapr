library(testthat)
library(xml2)
library(httr)

test_that("SOAP Fault Handling", {
  # Mock WSDL (minimal)
  wsdl <- list(
    get_target_namespace = function() "http://example.com/",
    get_operations = function(binding) {
      list(
        list(
          name = "TestOperation",
          input = "TestInput",
          output = "TestOutput",
          soap_action = "http://example.com/TestOperation"
        )
      )
    },
    get_type_definitions = function() list()
  )

  # Mock Port
  port <- list(
    location = "http://example.com/service",
    binding = "TestBinding"
  )

  # Mock Factory
  factory <- list(
    create = function(...) list()
  )

  # Mock Transport for SOAP 1.1 Fault
  mock_transport_fault11 <- function(url, handle, body, ...) {
    response_xml <- '
    <soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
      <soap:Body>
        <soap:Fault>
          <faultcode>soap:Client</faultcode>
          <faultstring>Invalid parameters</faultstring>
          <detail>
            <error>Parameter X is missing</error>
          </detail>
        </soap:Fault>
      </soap:Body>
    </soap:Envelope>'

    structure(
      list(
        url = url,
        status_code = 500,
        headers = list(`Content-Type` = "text/xml"),
        content = charToRaw(response_xml)
      ),
      class = "response"
    )
  }

  client <- Client$new(
    wsdl,
    port,
    factory,
    transport = mock_transport_fault11
  )

  # Expect error with fault details
  expect_error(
    client$TestOperation(),
    "SOAP Fault: \\[soap:Client\\] Invalid parameters"
  )

  # Mock Transport for SOAP 1.2 Fault
  mock_transport_fault12 <- function(url, handle, body, ...) {
    response_xml <- '
    <env:Envelope xmlns:env="http://www.w3.org/2003/05/soap-envelope">
      <env:Body>
        <env:Fault>
          <env:Code>
            <env:Value>env:Sender</env:Value>
          </env:Code>
          <env:Reason>
            <env:Text xml:lang="en">Sender Timeout</env:Text>
          </env:Reason>
          <env:Detail>
            <MaxTime>1000</MaxTime>
          </env:Detail>
        </env:Fault>
      </env:Body>
    </env:Envelope>'

    structure(
      list(
        url = url,
        status_code = 500,
        headers = list(`Content-Type` = "application/soap+xml"),
        content = charToRaw(response_xml)
      ),
      class = "response"
    )
  }

  client12 <- Client$new(
    wsdl,
    port,
    factory,
    transport = mock_transport_fault12
  )

  # Expect error with fault details
  expect_error(
    client12$TestOperation(),
    "SOAP Fault: \\[env:Sender\\] Sender Timeout"
  )
})
