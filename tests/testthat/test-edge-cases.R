build_edge_case_wsdl <- function() {
  paste0(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<wsdl:definitions xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/" ',
    'xmlns:tns="http://example.org/edge" ',
    'xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/" ',
    'xmlns:xsd="http://www.w3.org/2001/XMLSchema" ',
    'targetNamespace="http://example.org/edge">',
    "<wsdl:types>",
    '<xsd:schema targetNamespace="http://example.org/edge" elementFormDefault="qualified">',
    '<xsd:simpleType name="StatusEnum">',
    '<xsd:restriction base="xsd:string">',
    '<xsd:enumeration value="OPEN"/>',
    '<xsd:enumeration value="CLOSED"/>',
    "</xsd:restriction>",
    "</xsd:simpleType>",
    '<xsd:complexType name="ItemType">',
    "<xsd:sequence>",
    '<xsd:element name="Flag" type="xsd:boolean"/>',
    "</xsd:sequence>",
    '<xsd:attribute name="OptionalNoUse" type="xsd:string"/>',
    '<xsd:attribute name="EnumNoUse" type="tns:StatusEnum"/>',
    "</xsd:complexType>",
    '<xsd:element name="Ping">',
    "<xsd:complexType>",
    "<xsd:sequence>",
    '<xsd:element name="Item" type="tns:ItemType" minOccurs="0"/>',
    "</xsd:sequence>",
    "</xsd:complexType>",
    "</xsd:element>",
    '<xsd:element name="PingResponse">',
    "<xsd:complexType>",
    "<xsd:sequence>",
    '<xsd:element name="Ok" type="xsd:boolean"/>',
    "</xsd:sequence>",
    "</xsd:complexType>",
    "</xsd:element>",
    "</xsd:schema>",
    "</wsdl:types>",
    '<wsdl:message name="PingIn"><wsdl:part name="parameters" element="tns:Ping"/></wsdl:message>',
    '<wsdl:message name="PingOut"><wsdl:part name="parameters" element="tns:PingResponse"/></wsdl:message>',
    '<wsdl:portType name="EdgePortType">',
    '<wsdl:operation name="Ping">',
    '<wsdl:input message="tns:PingIn"/>',
    '<wsdl:output message="tns:PingOut"/>',
    "</wsdl:operation>",
    "</wsdl:portType>",
    '<wsdl:binding name="EdgeBinding" type="tns:EdgePortType">',
    '<soap:binding style="document" transport="http://schemas.xmlsoap.org/soap/http"/>',
    '<wsdl:operation name="Ping">',
    '<soap:operation soapAction="http://example.org/edge/Ping" style="document"/>',
    "<wsdl:input><soap:body use=\"literal\"/></wsdl:input>",
    "<wsdl:output><soap:body use=\"literal\"/></wsdl:output>",
    "</wsdl:operation>",
    "</wsdl:binding>",
    '<wsdl:service name="EdgeService">',
    '<wsdl:port name="EdgePort" binding="tns:EdgeBinding">',
    '<soap:address location="http://example.org/edge"/>',
    "</wsdl:port>",
    "</wsdl:service>",
    "</wsdl:definitions>"
  )
}

test_that("NA and edge-case boolean conversion works in Factory and generated utils", {
  wsdl_file <- tempfile(fileext = ".wsdl")
  pkg_dir <- tempfile("edge_bool_")
  dir.create(pkg_dir)
  writeLines(build_edge_case_wsdl(), wsdl_file)
  on.exit(unlink(c(wsdl_file, pkg_dir), recursive = TRUE), add = TRUE)

  svc <- Service$new(wsdl_file)
  factory <- svc$factory

  bool_na <- factory$create("xsd:boolean")
  expect_true(is.logical(bool_na))
  expect_true(is.na(bool_na))

  expect_identical(factory$convert_builtin_type(NA, "xsd:boolean"), NA)
  expect_identical(factory$convert_builtin_type(NA_character_, "xsd:boolean"), NA)
  expect_identical(factory$convert_builtin_type(NULL, "xsd:boolean"), NA)
  expect_identical(factory$convert_builtin_type(character(0), "xsd:boolean"), NA)
  expect_identical(factory$convert_builtin_type("true", "xsd:boolean"), TRUE)
  expect_identical(factory$convert_builtin_type("1", "xsd:boolean"), TRUE)
  expect_identical(factory$convert_builtin_type("false", "xsd:boolean"), FALSE)
  expect_identical(
    factory$convert_builtin_type(c("true", NA, "0"), "xsd:boolean"),
    c(TRUE, NA, FALSE)
  )

  gen <- Generator$new(WSDLParser$new(wsdl_file))
  gen$generate_package(pkg_dir, "EdgeCasePkg")
  pkg_path <- file.path(pkg_dir, "EdgeCasePkg")
  pkgload::load_all(pkg_path, quiet = TRUE)
  on.exit(pkgload::unload("EdgeCasePkg"), add = TRUE)

  ns_env <- asNamespace("EdgeCasePkg")
  expect_identical(ns_env$convert_builtin_type(NA, "xsd:boolean"), NA)
  expect_identical(ns_env$convert_builtin_type(NA_character_, "xsd:boolean"), NA)
  expect_identical(ns_env$convert_builtin_type(NULL, "xsd:boolean"), NA)
  expect_identical(ns_env$convert_builtin_type("1", "xsd:boolean"), TRUE)
  expect_identical(ns_env$convert_builtin_type("false", "xsd:boolean"), FALSE)

  item <- ns_env$TypeFactory$new()$ItemType(OptionalNoUse = "val", EnumNoUse = "OPEN")
  expect_s3_class(item, "soap_object")
  expect_equal(item$OptionalNoUse, "val")
  expect_equal(item$EnumNoUse, "OPEN")
})

test_that("Dynamic and static clients handle empty SOAP Body and case-insensitive Content-Type", {
  wsdl_file <- tempfile(fileext = ".wsdl")
  pkg_dir <- tempfile("edge_resp_")
  dir.create(pkg_dir)
  writeLines(build_edge_case_wsdl(), wsdl_file)
  on.exit(unlink(c(wsdl_file, pkg_dir), recursive = TRUE), add = TRUE)

  empty_body_xml <- paste0(
    '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">',
    "<soap:Body/>",
    "</soap:Envelope>"
  )
  ok_xml <- paste0(
    '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">',
    "<soap:Body>",
    '<PingResponse xmlns="http://example.org/edge"><Ok>true</Ok></PingResponse>',
    "</soap:Body>",
    "</soap:Envelope>"
  )

  make_resp <- function(url, headers, xml_text, status = 200) {
    structure(
      list(
        url = url,
        status_code = status,
        headers = headers,
        content = charToRaw(xml_text)
      ),
      class = "response"
    )
  }

  # Dynamic client
  svc <- Service$new(wsdl_file)
  svc$client$transport <- function(url, handle, body, ...) {
    make_resp(url, list(`content-type` = "text/xml; charset=utf-8"), empty_body_xml)
  }
  expect_null(svc$client$Ping())

  svc$client$transport <- function(url, handle, body, ...) {
    make_resp(url, list(`content-type` = "text/xml; charset=utf-8"), ok_xml)
  }
  dyn_res <- svc$client$Ping()
  expect_identical(dyn_res$Ok, TRUE)

  expect_error(
    svc$client$serializer$deserialize_response(
      make_resp("http://example.org", list(`content-type` = "text/plain"), "not xml")
    ),
    "Response is not XML"
  )
  expect_s3_class(
    svc$client$serializer$deserialize_response(
      make_resp("http://example.org", list(`CONTENT-TYPE` = "application/soap+xml"), ok_xml)
    ),
    "xml_document"
  )

  # Static client
  gen <- Generator$new(WSDLParser$new(wsdl_file))
  gen$generate_package(pkg_dir, "EdgeRespPkg")
  pkgload::load_all(file.path(pkg_dir, "EdgeRespPkg"), quiet = TRUE)
  on.exit(pkgload::unload("EdgeRespPkg"), add = TRUE)

  ns_env <- asNamespace("EdgeRespPkg")
  stat_client <- ns_env$new_client(
    transport = function(url, action, body, config, handle) {
      make_resp(url, list(`content-type` = "text/xml; charset=utf-8"), empty_body_xml)
    }
  )
  expect_null(stat_client$Ping())

  stat_client$transport <- function(url, action, body, config, handle) {
    make_resp(url, list(`CONTENT-TYPE` = "text/xml; charset=utf-8"), ok_xml)
  }
  stat_res <- stat_client$Ping()
  expect_identical(stat_res$Ok, TRUE)

  expect_error(
    ns_env$deserialize_response(
      make_resp("http://example.org", list(`content-type` = "text/plain"), "not xml")
    ),
    "Response is not XML"
  )

  fault_xml <- paste0(
    '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">',
    "<soap:Body><soap:Fault>",
    "<faultcode>soap:Server</faultcode><faultstring>Boom</faultstring>",
    "</soap:Fault></soap:Body></soap:Envelope>"
  )
  stat_client$transport <- function(url, action, body, config, handle) {
    make_resp(url, list(`content-type` = "text/xml; charset=utf-8"), fault_xml, status = 500)
  }
  expect_error(stat_client$Ping(), "SOAP Fault: \\[soap:Server\\] Boom")
})
