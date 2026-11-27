build_xsd13_response <- function(url, body_text) {
  headers <- list(`content-type` = "text/xml; charset=utf-8")
  structure(
    list(
      url = url,
      status_code = 200L,
      headers = headers,
      all_headers = list(list(status = 200L, version = "HTTP/1.1", headers = headers)),
      content = charToRaw(enc2utf8(body_text))
    ),
    class = "response"
  )
}

test_that("WSDLParser$parse_complex_type resolves recursive complexContent extension inheritance", {
  tmp_dir <- tempfile("soapr_inherit_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  wsdl_xml <- '<?xml version="1.0" encoding="UTF-8"?>
<definitions xmlns="http://schemas.xmlsoap.org/wsdl/"
             xmlns:tns="http://example.com/inherit"
             xmlns:xsd="http://www.w3.org/2001/XMLSchema"
             targetNamespace="http://example.com/inherit">
  <types>
    <xsd:schema targetNamespace="http://example.com/inherit">
      <xsd:complexType name="GrandBase">
        <xsd:sequence>
          <xsd:element name="CreatedBy" type="xsd:string"/>
        </xsd:sequence>
        <xsd:attribute name="tenant" type="xsd:string" use="required"/>
      </xsd:complexType>

      <xsd:complexType name="MiddleBase">
        <xsd:complexContent>
          <xsd:extension base="tns:GrandBase">
            <xsd:sequence>
              <xsd:element name="UpdatedAt" type="xsd:dateTime"/>
            </xsd:sequence>
            <xsd:attribute name="version" type="xsd:int" default="1"/>
          </xsd:extension>
        </xsd:complexContent>
      </xsd:complexType>

      <xsd:complexType name="DerivedRecord">
        <xsd:complexContent>
          <xsd:extension base="tns:MiddleBase">
            <xsd:sequence>
              <xsd:element name="Label" type="xsd:string"/>
            </xsd:sequence>
            <xsd:attribute name="active" type="xsd:boolean"/>
          </xsd:extension>
        </xsd:complexContent>
      </xsd:complexType>

      <xsd:complexType name="CyclicA">
        <xsd:complexContent>
          <xsd:extension base="tns:CyclicB">
            <xsd:sequence><xsd:element name="A" type="xsd:string"/></xsd:sequence>
          </xsd:extension>
        </xsd:complexContent>
      </xsd:complexType>
      <xsd:complexType name="CyclicB">
        <xsd:complexContent>
          <xsd:extension base="tns:CyclicA">
            <xsd:sequence><xsd:element name="B" type="xsd:int"/></xsd:sequence>
          </xsd:extension>
        </xsd:complexContent>
      </xsd:complexType>
    </xsd:schema>
  </types>
</definitions>'
  wsdl_path <- file.path(tmp_dir, "inherit.wsdl")
  writeLines(wsdl_xml, wsdl_path)

  parser <- WSDLParser$new(wsdl_path)

  # Direct parse_complex_type() call resolves base hierarchy recursively
  derived_node <- xml2::xml_find_first(parser$doc, "//xsd:complexType[@name='DerivedRecord']", parser$ns)
  direct_def <- parser$parse_complex_type(derived_node)
  expect_identical(names(direct_def$attributes), c("tenant", "version", "active"))
  expect_identical(names(direct_def$elements), c("CreatedBy", "UpdatedAt", "Label"))
  expect_identical(direct_def$elements[["CreatedBy"]]$type, "string")
  expect_identical(direct_def$elements[["UpdatedAt"]]$type, "dateTime")
  expect_identical(direct_def$elements[["Label"]]$type, "string")

  types <- parser$get_type_definitions()
  expect_identical(names(types[["DerivedRecord"]]$attributes), c("tenant", "version", "active"))
  expect_identical(names(types[["DerivedRecord"]]$elements), c("CreatedBy", "UpdatedAt", "Label"))
  expect_true(all(c("CyclicA", "CyclicB") %in% names(types)))
  expect_identical(names(types[["CyclicA"]]$elements), c("B", "A"))
})

test_that("WSDLParser parses xsd:choice, xsd:all, simpleContent, and nested anonymous types", {
  skip_if_not_installed("pkgload")

  tmp_dir <- tempfile("soapr_xsd13_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  wsdl_xml <- '<?xml version="1.0" encoding="UTF-8"?>
<definitions xmlns="http://schemas.xmlsoap.org/wsdl/"
             xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/"
             xmlns:tns="http://example.com/xsd13"
             xmlns:xsd="http://www.w3.org/2001/XMLSchema"
             targetNamespace="http://example.com/xsd13">
  <types>
    <xsd:schema targetNamespace="http://example.com/xsd13"
                elementFormDefault="qualified">
      <xsd:simpleType name="PayMethod">
        <xsd:restriction base="xsd:string">
          <xsd:enumeration value="CARD"/>
          <xsd:enumeration value="WIRE"/>
        </xsd:restriction>
      </xsd:simpleType>

      <xsd:complexType name="MoneyAmount">
        <xsd:simpleContent>
          <xsd:extension base="xsd:decimal">
            <xsd:attribute name="currency" type="xsd:string" use="required"/>
          </xsd:extension>
        </xsd:simpleContent>
      </xsd:complexType>

      <xsd:complexType name="ValueNamedAttr">
        <xsd:simpleContent>
          <xsd:extension base="xsd:string">
            <xsd:attribute name="value" type="xsd:string"/>
          </xsd:extension>
        </xsd:simpleContent>
      </xsd:complexType>

      <xsd:complexType name="PaymentAll">
        <xsd:all>
          <xsd:element name="Reference" type="xsd:string"/>
          <xsd:element name="Amount" type="tns:MoneyAmount"/>
        </xsd:all>
      </xsd:complexType>

      <xsd:complexType name="RouteChoice">
        <xsd:choice>
          <xsd:element name="Method" type="tns:PayMethod" minOccurs="1"/>
          <xsd:element name="CashDesk" type="xsd:string"/>
        </xsd:choice>
      </xsd:complexType>

      <xsd:element name="CreateOrder">
        <xsd:complexType>
          <xsd:sequence>
            <xsd:element name="Payment" type="tns:PaymentAll"/>
            <xsd:element name="Route" type="tns:RouteChoice"/>
            <xsd:element name="Shipping">
              <xsd:complexType>
                <xsd:sequence>
                  <xsd:element name="City" type="xsd:string"/>
                  <xsd:element name="Meta">
                    <xsd:complexType>
                      <xsd:sequence>
                        <xsd:element name="Gift" type="xsd:boolean"/>
                      </xsd:sequence>
                      <xsd:attribute name="priority" type="xsd:int"/>
                    </xsd:complexType>
                  </xsd:element>
                </xsd:sequence>
                <xsd:attribute name="express" type="xsd:boolean"/>
              </xsd:complexType>
            </xsd:element>
            <xsd:element name="TagColor">
              <xsd:simpleType>
                <xsd:restriction base="xsd:string">
                  <xsd:enumeration value="RED"/>
                  <xsd:enumeration value="BLUE"/>
                </xsd:restriction>
              </xsd:simpleType>
            </xsd:element>
          </xsd:sequence>
        </xsd:complexType>
      </xsd:element>

      <xsd:element name="CreateOrderResponse">
        <xsd:complexType>
          <xsd:sequence>
            <xsd:element name="Charged" type="tns:MoneyAmount"/>
            <xsd:element name="Note" type="tns:ValueNamedAttr"/>
            <xsd:element name="EchoShipping">
              <xsd:complexType>
                <xsd:sequence>
                  <xsd:element name="Postal" type="xsd:string"/>
                </xsd:sequence>
                <xsd:attribute name="verified" type="xsd:boolean"/>
              </xsd:complexType>
            </xsd:element>
          </xsd:sequence>
        </xsd:complexType>
      </xsd:element>
    </xsd:schema>
  </types>

  <message name="CreateOrderIn"><part name="parameters" element="tns:CreateOrder"/></message>
  <message name="CreateOrderOut"><part name="parameters" element="tns:CreateOrderResponse"/></message>
  <portType name="OrderPort">
    <operation name="CreateOrder">
      <input message="tns:CreateOrderIn"/>
      <output message="tns:CreateOrderOut"/>
    </operation>
  </portType>
  <binding name="OrderBinding" type="tns:OrderPort">
    <soap:binding transport="http://schemas.xmlsoap.org/soap/http" style="document"/>
    <operation name="CreateOrder">
      <soap:operation soapAction="http://example.com/xsd13/CreateOrder" style="document"/>
      <input><soap:body use="literal"/></input>
      <output><soap:body use="literal"/></output>
    </operation>
  </binding>
  <service name="OrderService">
    <port name="OrderPort" binding="tns:OrderBinding">
      <soap:address location="http://example.com/xsd13/endpoint"/>
    </port>
  </service>
</definitions>'
  wsdl_path <- file.path(tmp_dir, "xsd13.wsdl")
  writeLines(wsdl_xml, wsdl_path)

  parser <- WSDLParser$new(wsdl_path)
  types <- parser$get_type_definitions()

  expect_true(isTRUE(types[["MoneyAmount"]]$simple_content))
  expect_identical(types[["MoneyAmount"]]$base, "decimal")
  expect_identical(types[["ValueNamedAttr"]]$value_field, ".value")
  expect_identical(names(types[["PaymentAll"]]$elements), c("Reference", "Amount"))
  expect_identical(names(types[["RouteChoice"]]$elements), c("Method", "CashDesk"))
  expect_identical(types[["RouteChoice"]]$elements[["Method"]]$minOccurs, "0")

  # Nested anonymous complexTypes/simpleTypes are hoisted cleanly and do NOT leak into parent
  expect_identical(names(types[["CreateOrder"]]$elements), c("Payment", "Route", "Shipping", "TagColor"))
  expect_length(types[["CreateOrder"]]$attributes, 0L)
  expect_true(all(c("CreateOrder_Shipping", "CreateOrder_Shipping_Meta", "CreateOrder_TagColor") %in% names(types)))
  expect_identical(types[["CreateOrder_TagColor"]]$values, c("RED", "BLUE"))
  expect_identical(names(types[["CreateOrder_Shipping"]]$attributes), "express")
  expect_identical(names(types[["CreateOrder_Shipping"]]$elements), c("City", "Meta"))

  # Dynamic & Static parity for choice, all, simpleContent, and nested anonymous types
  pkg_name <- "Xsd13Pkg"
  pkg_dir <- file.path(tmp_dir, pkg_name)
  Generator$new(parser)$generate_package(tmp_dir, pkg_name)
  pkgload::load_all(pkg_dir, quiet = TRUE)
  on.exit(try(pkgload::unload(pkg_name), silent = TRUE), add = TRUE, after = FALSE)
  ns_env <- asNamespace(pkg_name)

  response_xml <- paste0(
    '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/"',
    ' xmlns:tns="http://example.com/xsd13">',
    "<soap:Body><tns:CreateOrderResponse>",
    '<Charged currency="EUR">42.50</Charged>',
    '<Note value="meta">Delivered</Note>',
    '<EchoShipping verified="true"><Postal>10001</Postal></EchoShipping>',
    "</tns:CreateOrderResponse></soap:Body></soap:Envelope>"
  )

  state <- new.env(parent = emptyenv())
  state$dyn_body <- NULL
  state$stat_body <- NULL

  svc <- Service$new(wsdl_path)
  svc$client$transport <- function(url, handle, body, ...) {
    state$dyn_body <- body
    build_xsd13_response(url, response_xml)
  }

  stat_client <- ns_env$new_client(
    transport = function(url, action, body, config, handle) {
      state$stat_body <- body
      build_xsd13_response(url, response_xml)
    }
  )

  # RouteChoice with CashDesk only (Method omitted) must work without enum error
  dyn_res <- svc$client$CreateOrder(
    Payment = svc$factory$create(
      "PaymentAll",
      Reference = "INV-1",
      Amount = svc$factory$create("MoneyAmount", currency = "EUR", value = 42.5)
    ),
    Route = svc$factory$create("RouteChoice", CashDesk = "Main"),
    Shipping = svc$factory$create(
      "CreateOrder_Shipping",
      express = TRUE,
      City = "NYC",
      Meta = svc$factory$create("CreateOrder_Shipping_Meta", priority = 2L, Gift = FALSE)
    ),
    TagColor = "RED"
  )

  stat_res <- stat_client$CreateOrder(
    Payment = stat_client$types$PaymentAll(
      Reference = "INV-1",
      Amount = stat_client$types$MoneyAmount(currency = "EUR", value = 42.5)
    ),
    Route = stat_client$types$RouteChoice(CashDesk = "Main"),
    Shipping = stat_client$types$CreateOrder_Shipping(
      express = TRUE,
      City = "NYC",
      Meta = stat_client$types$CreateOrder_Shipping_Meta(priority = 2L, Gift = FALSE)
    ),
    TagColor = "RED"
  )

  expect_identical(
    as.character(xml2::read_xml(state$dyn_body)),
    as.character(xml2::read_xml(state$stat_body))
  )
  req_doc <- xml2::read_xml(state$stat_body)
  amt_node <- xml2::xml_find_first(req_doc, ".//*[local-name()='Amount']")
  expect_identical(xml2::xml_attr(amt_node, "currency"), "EUR")
  expect_identical(xml2::xml_text(amt_node), "42.5")

  expect_equal(dyn_res, stat_res)
  expect_identical(stat_res$Charged$currency, "EUR")
  expect_equal(stat_res$Charged$value, 42.5)
  expect_identical(stat_res$Note$value, "meta")
  expect_identical(stat_res$Note$.value, "Delivered")
  expect_identical(stat_res$EchoShipping$verified, TRUE)
  expect_identical(stat_res$EchoShipping$Postal, "10001")
})

test_that("Existing real WSDLs extract choice and simpleContent types", {
  bdt <- WSDLParser$new(test_path("testdata/valid_wsdls/BDTInteractive.xml"))
  bdt_types <- bdt$get_type_definitions()
  expect_true(isTRUE(bdt_types[["GroupDefinition"]]$simple_content))
  expect_identical(names(bdt_types[["GroupDefinition"]]$attributes), c("Bucket", "Owner", "Name"))

  ec2 <- WSDLParser$new(test_path("testdata/valid_wsdls/aws-ec2.wsdl"))
  ec2_types <- ec2$get_type_definitions()
  expect_true("launchPermission" %in% names(ec2_types[["ModifyImageAttributeType"]]$elements))
  expect_identical(
    names(ec2_types[["LaunchPermissionOperationType"]]$elements),
    c("add", "remove")
  )
})
