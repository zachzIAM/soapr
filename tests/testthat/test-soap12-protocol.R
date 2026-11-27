make_soap12_test_response <- function(
  url,
  body_text,
  status = 200L,
  content_type = "application/soap+xml; charset=utf-8"
) {
  headers <- list(`Content-Type` = content_type)
  structure(
    list(
      url = url,
      status_code = as.integer(status),
      headers = headers,
      all_headers = list(list(status = as.integer(status), version = "HTTP/1.1", headers = headers)),
      content = charToRaw(enc2utf8(body_text))
    ),
    class = "response"
  )
}

test_that("WSDLParser detects SOAP 1.1/1.2 bindings and extracts soap12 actions", {
  tmp_dir <- tempfile("soapr_soap12_wsdl_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  wsdl_xml <- '<?xml version="1.0" encoding="UTF-8"?>
<definitions xmlns="http://schemas.xmlsoap.org/wsdl/"
             xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/"
             xmlns:soap12="http://schemas.xmlsoap.org/wsdl/soap12/"
             xmlns:tns="http://example.com/dual"
             xmlns:xsd="http://www.w3.org/2001/XMLSchema"
             targetNamespace="http://example.com/dual">
  <types>
    <xsd:schema targetNamespace="http://example.com/dual" elementFormDefault="qualified">
      <xsd:element name="Ping">
        <xsd:complexType><xsd:sequence><xsd:element name="Msg" type="xsd:string"/></xsd:sequence></xsd:complexType>
      </xsd:element>
      <xsd:element name="PingResponse">
        <xsd:complexType><xsd:sequence><xsd:element name="Reply" type="xsd:string"/></xsd:sequence></xsd:complexType>
      </xsd:element>
      <xsd:element name="NoAction">
        <xsd:complexType><xsd:sequence><xsd:element name="Flag" type="xsd:boolean"/></xsd:sequence></xsd:complexType>
      </xsd:element>
      <xsd:element name="NoActionResponse">
        <xsd:complexType><xsd:sequence><xsd:element name="Ok" type="xsd:boolean"/></xsd:sequence></xsd:complexType>
      </xsd:element>
    </xsd:schema>
  </types>
  <message name="PingIn"><part name="parameters" element="tns:Ping"/></message>
  <message name="PingOut"><part name="parameters" element="tns:PingResponse"/></message>
  <message name="NoActionIn"><part name="parameters" element="tns:NoAction"/></message>
  <message name="NoActionOut"><part name="parameters" element="tns:NoActionResponse"/></message>
  <portType name="DualPortType">
    <operation name="Ping">
      <input message="tns:PingIn"/><output message="tns:PingOut"/>
    </operation>
    <operation name="NoAction">
      <input message="tns:NoActionIn"/><output message="tns:NoActionOut"/>
    </operation>
  </portType>
  <binding name="DualSoap11Binding" type="tns:DualPortType">
    <soap:binding transport="http://schemas.xmlsoap.org/soap/http" style="document"/>
    <operation name="Ping">
      <soap:operation soapAction="http://example.com/soap11/Ping" style="document"/>
      <input><soap:body use="literal"/></input><output><soap:body use="literal"/></output>
    </operation>
    <operation name="NoAction">
      <soap:operation soapAction="" style="document"/>
      <input><soap:body use="literal"/></input><output><soap:body use="literal"/></output>
    </operation>
  </binding>
  <binding name="DualSoap12Binding" type="tns:DualPortType">
    <soap12:binding transport="http://schemas.xmlsoap.org/soap/http" style="document"/>
    <operation name="Ping">
      <soap12:operation soapAction="http://example.com/soap12/Ping" style="document"/>
      <input><soap12:body use="literal"/></input><output><soap12:body use="literal"/></output>
    </operation>
    <operation name="NoAction">
      <soap12:operation soapAction="" style="document"/>
      <input><soap12:body use="literal"/></input><output><soap12:body use="literal"/></output>
    </operation>
  </binding>
  <service name="DualService">
    <port name="DualSoap11Port" binding="tns:DualSoap11Binding">
      <soap:address location="http://example.com/v11"/>
    </port>
    <port name="DualSoap12Port" binding="tns:DualSoap12Binding">
      <soap12:address location="http://example.com/v12"/>
    </port>
  </service>
</definitions>'
  wsdl_path <- file.path(tmp_dir, "dual.wsdl")
  writeLines(wsdl_xml, wsdl_path)

  parser <- WSDLParser$new(wsdl_path)
  expect_identical(parser$get_binding_soap_version("DualSoap11Binding"), "1.1")
  expect_identical(parser$get_binding_soap_version("DualSoap12Binding"), "1.2")

  svcs <- parser$get_services()
  expect_identical(svcs$DualService$ports$DualSoap11Port$soap_version, "1.1")
  expect_identical(svcs$DualService$ports$DualSoap12Port$soap_version, "1.2")
  expect_identical(
    svcs$DualService$ports$DualSoap11Port$operations$Ping$action,
    "http://example.com/soap11/Ping"
  )
  expect_identical(
    svcs$DualService$ports$DualSoap12Port$operations$Ping$action,
    "http://example.com/soap12/Ping"
  )

  # Real multi-port fixture check
  ci_parser <- WSDLParser$new(test_path("testdata/valid_wsdls/CountryInfoService.xml"))
  ci_ports <- ci_parser$get_services()$CountryInfoService$ports
  expect_identical(ci_ports$CountryInfoServiceSoap$soap_version, "1.1")
  expect_identical(ci_ports$CountryInfoServiceSoap12$soap_version, "1.2")
})

test_that("Dynamic and static clients emit compliant SOAP 1.1 and SOAP 1.2 envelopes, headers, and faults", {
  skip_if_not_installed("pkgload")

  tmp_dir <- tempfile("soapr_soap12_client_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  wsdl_xml <- '<?xml version="1.0" encoding="UTF-8"?>
<definitions xmlns="http://schemas.xmlsoap.org/wsdl/"
             xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/"
             xmlns:soap12="http://schemas.xmlsoap.org/wsdl/soap12/"
             xmlns:tns="http://example.com/dual"
             xmlns:xsd="http://www.w3.org/2001/XMLSchema"
             targetNamespace="http://example.com/dual">
  <types>
    <xsd:schema targetNamespace="http://example.com/dual" elementFormDefault="qualified">
      <xsd:element name="Ping">
        <xsd:complexType><xsd:sequence><xsd:element name="Msg" type="xsd:string"/></xsd:sequence></xsd:complexType>
      </xsd:element>
      <xsd:element name="PingResponse">
        <xsd:complexType><xsd:sequence><xsd:element name="Reply" type="xsd:string"/></xsd:sequence></xsd:complexType>
      </xsd:element>
      <xsd:element name="NoAction">
        <xsd:complexType><xsd:sequence><xsd:element name="Flag" type="xsd:boolean"/></xsd:sequence></xsd:complexType>
      </xsd:element>
      <xsd:element name="NoActionResponse">
        <xsd:complexType><xsd:sequence><xsd:element name="Ok" type="xsd:boolean"/></xsd:sequence></xsd:complexType>
      </xsd:element>
    </xsd:schema>
  </types>
  <message name="PingIn"><part name="parameters" element="tns:Ping"/></message>
  <message name="PingOut"><part name="parameters" element="tns:PingResponse"/></message>
  <message name="NoActionIn"><part name="parameters" element="tns:NoAction"/></message>
  <message name="NoActionOut"><part name="parameters" element="tns:NoActionResponse"/></message>
  <portType name="DualPortType">
    <operation name="Ping"><input message="tns:PingIn"/><output message="tns:PingOut"/></operation>
    <operation name="NoAction"><input message="tns:NoActionIn"/><output message="tns:NoActionOut"/></operation>
  </portType>
  <binding name="DualSoap11Binding" type="tns:DualPortType">
    <soap:binding transport="http://schemas.xmlsoap.org/soap/http" style="document"/>
    <operation name="Ping">
      <soap:operation soapAction="http://example.com/soap11/Ping" style="document"/>
      <input><soap:body use="literal"/></input><output><soap:body use="literal"/></output>
    </operation>
    <operation name="NoAction">
      <soap:operation soapAction="" style="document"/>
      <input><soap:body use="literal"/></input><output><soap:body use="literal"/></output>
    </operation>
  </binding>
  <binding name="DualSoap12Binding" type="tns:DualPortType">
    <soap12:binding transport="http://schemas.xmlsoap.org/soap/http" style="document"/>
    <operation name="Ping">
      <soap12:operation soapAction="http://example.com/soap12/Ping" style="document"/>
      <input><soap12:body use="literal"/></input><output><soap12:body use="literal"/></output>
    </operation>
    <operation name="NoAction">
      <soap12:operation soapAction="" style="document"/>
      <input><soap12:body use="literal"/></input><output><soap12:body use="literal"/></output>
    </operation>
  </binding>
  <service name="DualService">
    <port name="DualSoap11Port" binding="tns:DualSoap11Binding">
      <soap:address location="http://example.com/v11"/>
    </port>
    <port name="DualSoap12Port" binding="tns:DualSoap12Binding">
      <soap12:address location="http://example.com/v12"/>
    </port>
  </service>
</definitions>'
  wsdl_path <- file.path(tmp_dir, "dual.wsdl")
  writeLines(wsdl_xml, wsdl_path)

  pkg_name <- "DualSoapPkg"
  pkg_dir <- file.path(tmp_dir, pkg_name)
  Generator$new(WSDLParser$new(wsdl_path))$generate_package(tmp_dir, pkg_name)
  pkgload::load_all(pkg_dir, quiet = TRUE)
  on.exit(try(pkgload::unload(pkg_name), silent = TRUE), add = TRUE, after = FALSE)
  ns_env <- asNamespace(pkg_name)

  state <- new.env(parent = emptyenv())
  state$dyn_reqs <- list()
  state$stat_reqs <- list()
  state$next_resp <- NULL

  dyn_transport <- function(url, handle, body, ...) {
    extra <- list(...)
    headers <- unlist(lapply(extra, function(x) x$headers))
    state$dyn_reqs[[length(state$dyn_reqs) + 1L]] <- list(
      url = url,
      headers = as.list(headers),
      body = body
    )
    state$next_resp
  }

  stat_transport <- function(url, action, body, config, handle) {
    state$stat_reqs[[length(state$stat_reqs) + 1L]] <- list(
      url = url,
      action = action,
      headers = as.list(config$headers),
      body = body
    )
    state$next_resp
  }

  # SOAP 1.1 port assertions
  svc11 <- Service$new(wsdl_path, port_name = "DualSoap11Port")
  svc11$client$transport <- dyn_transport
  stat11 <- ns_env$new_client(port = "DualSoap11Port", transport = stat_transport)

  ok11_xml <- paste0(
    '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">',
    "<soap:Body><PingResponse><Reply>pong11</Reply></PingResponse></soap:Body></soap:Envelope>"
  )
  state$next_resp <- make_soap12_test_response(
    "http://example.com/v11",
    ok11_xml,
    content_type = "text/xml; charset=utf-8"
  )
  dyn11_res <- svc11$client$Ping(Msg = "hi11")
  stat11_res <- stat11$Ping(Msg = "hi11")
  expect_identical(dyn11_res$Reply, "pong11")
  expect_identical(stat11_res$Reply, "pong11")

  expect_identical(state$dyn_reqs[[1]]$headers[["Content-Type"]], "text/xml; charset=utf-8")
  expect_identical(state$dyn_reqs[[1]]$headers[["SOAPAction"]], "http://example.com/soap11/Ping")
  expect_identical(state$stat_reqs[[1]]$headers[["Content-Type"]], "text/xml; charset=utf-8")
  expect_identical(state$stat_reqs[[1]]$headers[["SOAPAction"]], "http://example.com/soap11/Ping")
  expect_identical(
    unname(xml2::xml_ns(xml2::read_xml(state$dyn_reqs[[1]]$body))[["soap"]]),
    "http://schemas.xmlsoap.org/soap/envelope/"
  )
  expect_identical(
    unname(xml2::xml_ns(xml2::read_xml(state$stat_reqs[[1]]$body))[["soap"]]),
    "http://schemas.xmlsoap.org/soap/envelope/"
  )

  # SOAP 1.2 port assertions (with action)
  svc12 <- Service$new(wsdl_path, port_name = "DualSoap12Port")
  svc12$client$transport <- dyn_transport
  stat12 <- ns_env$new_client(port = "DualSoap12Port", transport = stat_transport)

  ok12_xml <- paste0(
    '<env:Envelope xmlns:env="http://www.w3.org/2003/05/soap-envelope">',
    "<env:Body><PingResponse><Reply>pong12</Reply></PingResponse></env:Body></env:Envelope>"
  )
  state$next_resp <- make_soap12_test_response("http://example.com/v12", ok12_xml)
  dyn12_res <- svc12$client$Ping(Msg = "hi12")
  stat12_res <- stat12$Ping(Msg = "hi12")
  expect_identical(dyn12_res$Reply, "pong12")
  expect_identical(stat12_res$Reply, "pong12")

  expected_ct12 <- 'application/soap+xml; charset=utf-8; action="http://example.com/soap12/Ping"'
  expect_identical(state$dyn_reqs[[2]]$headers[["Content-Type"]], expected_ct12)
  expect_null(state$dyn_reqs[[2]]$headers[["SOAPAction"]])
  expect_identical(state$stat_reqs[[2]]$headers[["Content-Type"]], expected_ct12)
  expect_null(state$stat_reqs[[2]]$headers[["SOAPAction"]])
  expect_identical(
    unname(xml2::xml_ns(xml2::read_xml(state$dyn_reqs[[2]]$body))[["soap"]]),
    "http://www.w3.org/2003/05/soap-envelope"
  )
  expect_identical(
    unname(xml2::xml_ns(xml2::read_xml(state$stat_reqs[[2]]$body))[["soap"]]),
    "http://www.w3.org/2003/05/soap-envelope"
  )
  expect_identical(
    as.character(xml2::read_xml(state$dyn_reqs[[2]]$body)),
    as.character(xml2::read_xml(state$stat_reqs[[2]]$body))
  )

  # SOAP 1.2 empty action omits empty ; action="" parameter
  no_act_xml <- paste0(
    '<env:Envelope xmlns:env="http://www.w3.org/2003/05/soap-envelope">',
    "<env:Body><NoActionResponse><Ok>true</Ok></NoActionResponse></env:Body></env:Envelope>"
  )
  state$next_resp <- make_soap12_test_response("http://example.com/v12", no_act_xml)
  expect_true(svc12$client$NoAction(Flag = TRUE)$Ok)
  expect_true(stat12$NoAction(Flag = TRUE)$Ok)
  expect_identical(state$dyn_reqs[[3]]$headers[["Content-Type"]], "application/soap+xml; charset=utf-8")
  expect_identical(state$stat_reqs[[3]]$headers[["Content-Type"]], "application/soap+xml; charset=utf-8")

  # SOAP 1.1 and SOAP 1.2 faults on respective ports
  fault11 <- paste0(
    '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">',
    "<soap:Body><soap:Fault><faultcode>soap:Server</faultcode>",
    "<faultstring>V11 Fail</faultstring></soap:Fault></soap:Body></soap:Envelope>"
  )
  state$next_resp <- make_soap12_test_response(
    "http://example.com/v11",
    fault11,
    status = 500L,
    content_type = "text/xml; charset=utf-8"
  )
  expect_error(svc11$client$Ping(Msg = "err"), "SOAP Fault: \\[soap:Server\\] V11 Fail")
  expect_error(stat11$Ping(Msg = "err"), "SOAP Fault: \\[soap:Server\\] V11 Fail")

  fault12 <- paste0(
    '<env:Envelope xmlns:env="http://www.w3.org/2003/05/soap-envelope">',
    "<env:Body><env:Fault><env:Code><env:Value>env:Receiver</env:Value></env:Code>",
    "<env:Reason><env:Text>V12 Fail</env:Text></env:Reason>",
    "<env:Detail>Timeout</env:Detail></env:Fault></env:Body></env:Envelope>"
  )
  state$next_resp <- make_soap12_test_response("http://example.com/v12", fault12, status = 500L)
  expect_error(svc12$client$Ping(Msg = "err"), "SOAP Fault: \\[env:Receiver\\] V12 Fail\nDetail: Timeout")
  expect_error(stat12$Ping(Msg = "err"), "SOAP Fault: \\[env:Receiver\\] V12 Fail\nDetail: Timeout")
})
