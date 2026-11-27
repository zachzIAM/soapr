build_ops_datetime_wsdl <- function() {
  paste0(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<wsdl:definitions xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/" ',
    'xmlns:tns="http://example.org/orders" ',
    'xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/" ',
    'xmlns:xsd="http://www.w3.org/2001/XMLSchema" ',
    'targetNamespace="http://example.org/orders">',
    "<wsdl:types>",
    '<xsd:schema targetNamespace="http://example.org/orders" elementFormDefault="qualified">',
    '<xsd:simpleType name="ModeEnum">',
    '<xsd:restriction base="xsd:string">',
    '<xsd:enumeration value="FAST"/>',
    '<xsd:enumeration value="SAFE"/>',
    "</xsd:restriction>",
    "</xsd:simpleType>",
    '<xsd:element name="GetOrderRequest">',
    "<xsd:complexType>",
    "<xsd:sequence>",
    '<xsd:element name="OrderId" type="xsd:string"/>',
    '<xsd:element name="Mode" type="tns:ModeEnum"/>',
    "</xsd:sequence>",
    "</xsd:complexType>",
    "</xsd:element>",
    '<xsd:element name="FetchOrderResponse">',
    "<xsd:complexType>",
    "<xsd:sequence>",
    '<xsd:element name="CreatedDate" type="xsd:date"/>',
    '<xsd:element name="UpdatedAt" type="xsd:dateTime"/>',
    "</xsd:sequence>",
    "</xsd:complexType>",
    "</xsd:element>",
    "</xsd:schema>",
    "</wsdl:types>",
    '<wsdl:message name="FetchOrderIn"><wsdl:part name="parameters" element="tns:GetOrderRequest"/></wsdl:message>',
    '<wsdl:message name="FetchOrderOut"><wsdl:part name="parameters" element="tns:FetchOrderResponse"/></wsdl:message>',
    '<wsdl:message name="NoElemIn"><wsdl:part name="Arg" type="xsd:string"/></wsdl:message>',
    '<wsdl:message name="NoElemOut"><wsdl:part name="parameters" element="tns:FetchOrderResponse"/></wsdl:message>',
    '<wsdl:portType name="OrderPortType">',
    '<wsdl:operation name="FetchOrder">',
    '<wsdl:input message="tns:FetchOrderIn"/>',
    '<wsdl:output message="tns:FetchOrderOut"/>',
    "</wsdl:operation>",
    '<wsdl:operation name="NoElemOp">',
    '<wsdl:input message="tns:NoElemIn"/>',
    '<wsdl:output message="tns:NoElemOut"/>',
    "</wsdl:operation>",
    "</wsdl:portType>",
    '<wsdl:binding name="OrderBinding" type="tns:OrderPortType">',
    '<soap:binding style="document" transport="http://schemas.xmlsoap.org/soap/http"/>',
    '<wsdl:operation name="FetchOrder">',
    '<soap:operation soapAction="http://example.org/orders/FetchOrder" style="document"/>',
    '<wsdl:input><soap:body use="literal"/></wsdl:input>',
    '<wsdl:output><soap:body use="literal"/></wsdl:output>',
    "</wsdl:operation>",
    '<wsdl:operation name="NoElemOp">',
    '<soap:operation soapAction="http://example.org/orders/NoElemOp" style="document"/>',
    '<wsdl:input><soap:body use="literal"/></wsdl:input>',
    '<wsdl:output><soap:body use="literal"/></wsdl:output>',
    "</wsdl:operation>",
    "</wsdl:binding>",
    '<wsdl:service name="OrderService">',
    '<wsdl:port name="OrderPort" binding="tns:OrderBinding">',
    '<soap:address location="http://example.org/orders"/>',
    "</wsdl:port>",
    "</wsdl:service>",
    "</wsdl:definitions>"
  )
}

test_that("Dynamic and static clients use op$input_element and handle simple enum defaults", {
  wsdl_file <- tempfile(fileext = ".wsdl")
  pkg_dir <- tempfile("order_pkg_")
  dir.create(pkg_dir)
  writeLines(build_ops_datetime_wsdl(), wsdl_file)
  on.exit(unlink(c(wsdl_file, pkg_dir), recursive = TRUE), add = TRUE)

  resp_xml <- paste0(
    '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">',
    "<soap:Body>",
    '<FetchOrderResponse xmlns="http://example.org/orders">',
    "<CreatedDate>2026-03-24</CreatedDate>",
    "<UpdatedAt>2026-03-24T12:00:00+02:00</UpdatedAt>",
    "</FetchOrderResponse>",
    "</soap:Body>",
    "</soap:Envelope>"
  )
  make_resp <- function(url) {
    structure(
      list(
        url = url,
        status_code = 200,
        headers = list(`Content-Type` = "text/xml; charset=utf-8"),
        content = charToRaw(resp_xml)
      ),
      class = "response"
    )
  }

  # Dynamic client
  dyn_body <- NULL
  dyn_no_elem_body <- NULL
  svc <- Service$new(wsdl_file)
  svc$client$transport <- function(url, handle, body, ...) {
    dyn_body <<- body
    make_resp(url)
  }

  expect_error(
    svc$client$FetchOrder(OrderId = "ORD-1", Mode = "BOGUS"),
    "Invalid value 'BOGUS' for argument 'Mode'"
  )

  dyn_res <- svc$client$FetchOrder(OrderId = "ORD-1", Mode = "FAST")
  expect_equal(dyn_res$CreatedDate, as.Date("2026-03-24"))
  expect_equal(
    as.numeric(dyn_res$UpdatedAt),
    as.numeric(as.POSIXct("2026-03-24 10:00:00", tz = "UTC"))
  )

  svc$client$transport <- function(url, handle, body, ...) {
    dyn_no_elem_body <<- body
    make_resp(url)
  }
  svc$client$NoElemOp()

  # Static client
  gen <- Generator$new(WSDLParser$new(wsdl_file))
  gen$generate_package(pkg_dir, "OrderPkg")
  pkgload::load_all(file.path(pkg_dir, "OrderPkg"), quiet = TRUE)
  on.exit(pkgload::unload("OrderPkg"), add = TRUE, after = FALSE)

  ns_env <- asNamespace("OrderPkg")
  tf <- ns_env$TypeFactory$new()

  # Simple enum constructor default NA should not crash
  default_enum <- tf$ModeEnum()
  expect_true(is.na(default_enum))
  expect_equal(attr(default_enum, "soap_type"), "ModeEnum")

  explicit_enum <- tf$ModeEnum("SAFE")
  expect_equal(as.character(explicit_enum), "SAFE")
  expect_equal(attr(explicit_enum, "soap_type"), "ModeEnum")
  expect_error(tf$ModeEnum("BOGUS"))

  stat_body <- NULL
  stat_no_elem_body <- NULL
  stat_client <- ns_env$new_client(
    transport = function(url, action, body, config, handle) {
      stat_body <<- body
      make_resp(url)
    }
  )
  stat_res <- stat_client$FetchOrder(OrderId = "ORD-1", Mode = "FAST")
  expect_equal(stat_res$CreatedDate, as.Date("2026-03-24"))
  expect_equal(
    as.numeric(stat_res$UpdatedAt),
    as.numeric(as.POSIXct("2026-03-24 10:00:00", tz = "UTC"))
  )

  stat_client$transport <- function(url, action, body, config, handle) {
    stat_no_elem_body <<- body
    make_resp(url)
  }
  stat_client$NoElemOp()

  expect_identical(dyn_body, stat_body)
  expect_identical(dyn_no_elem_body, stat_no_elem_body)

  dyn_doc <- xml2::read_xml(dyn_body)
  body_child <- xml2::xml_children(xml2::xml_find_first(dyn_doc, ".//*[local-name()='Body']"))[[1]]
  expect_equal(xml2::xml_name(body_child), "GetOrderRequest")

  no_elem_doc <- xml2::read_xml(dyn_no_elem_body)
  no_elem_child <- xml2::xml_children(xml2::xml_find_first(no_elem_doc, ".//*[local-name()='Body']"))[[1]]
  expect_equal(xml2::xml_name(no_elem_child), "NoElemOp")
})

test_that("xsd:date and xsd:dateTime parsing aligns across dynamic Factory and static utils", {
  wsdl_file <- tempfile(fileext = ".wsdl")
  pkg_dir <- tempfile("dt_pkg_")
  dir.create(pkg_dir)
  writeLines(build_ops_datetime_wsdl(), wsdl_file)
  on.exit(unlink(c(wsdl_file, pkg_dir), recursive = TRUE), add = TRUE)

  svc <- Service$new(wsdl_file)
  gen <- Generator$new(WSDLParser$new(wsdl_file))
  gen$generate_package(pkg_dir, "DateTimePkg")
  pkgload::load_all(file.path(pkg_dir, "DateTimePkg"), quiet = TRUE)
  on.exit(pkgload::unload("DateTimePkg"), add = TRUE, after = FALSE)

  dyn_conv <- svc$factory$convert_builtin_type
  stat_conv <- asNamespace("DateTimePkg")$convert_builtin_type

  for (conv in list(dyn_conv, stat_conv)) {
    # xsd:date cases
    expect_equal(conv("2026-03-24", "xsd:date"), as.Date("2026-03-24"))
    expect_equal(conv("2026-03-24Z", "xsd:date"), as.Date("2026-03-24"))
    expect_equal(conv("2026-03-24+02:00", "xsd:date"), as.Date("2026-03-24"))
    expect_true(inherits(conv(NA, "xsd:date"), "Date") && is.na(conv(NA, "xsd:date")))
    expect_true(inherits(conv("", "xsd:date"), "Date") && is.na(conv("", "xsd:date")))
    expect_true(inherits(conv(NULL, "xsd:date"), "Date") && is.na(conv(NULL, "xsd:date")))
    expect_true(inherits(conv(character(0), "xsd:date"), "Date") && is.na(conv(character(0), "xsd:date")))

    # xsd:dateTime cases
    dt_z <- conv("2026-03-24T12:00:00Z", "xsd:dateTime")
    expect_s3_class(dt_z, "POSIXct")
    expect_equal(attr(dt_z, "tzone"), "UTC")
    expect_equal(as.numeric(dt_z), as.numeric(as.POSIXct("2026-03-24 12:00:00", tz = "UTC")))

    dt_frac <- conv("2026-03-24T12:00:00.25Z", "xsd:dateTime")
    expect_equal(as.numeric(dt_frac) %% 1, 0.25, tolerance = 1e-6)

    dt_plus <- conv("2026-03-24T12:00:00+02:00", "xsd:dateTime")
    expect_equal(as.numeric(dt_plus), as.numeric(as.POSIXct("2026-03-24 10:00:00", tz = "UTC")))

    dt_minus <- conv("2026-03-24T12:00:00-05:00", "xsd:dateTime")
    expect_equal(as.numeric(dt_minus), as.numeric(as.POSIXct("2026-03-24 17:00:00", tz = "UTC")))

    dt_frac_off <- conv("2026-03-24T12:00:00.5+02:00", "xsd:dateTime")
    expect_equal(as.numeric(dt_frac_off), as.numeric(as.POSIXct("2026-03-24 10:00:00", tz = "UTC")) + 0.5)

    dt_naive <- conv("2026-03-24T12:00:00", "xsd:dateTime")
    expect_equal(as.numeric(dt_naive), as.numeric(as.POSIXct("2026-03-24 12:00:00", tz = "UTC")))

    for (empty_val in list(NA, NA_character_, "", "   ", NULL, character(0))) {
      dt_empty <- conv(empty_val, "xsd:dateTime")
      expect_s3_class(dt_empty, "POSIXct")
      expect_true(is.na(dt_empty))
    }
  }
})
