build_parity_wsdl <- function(element_form_default, attribute_form_default, ns = "http://example.org/parity") {
  sprintf(
    '<?xml version="1.0" encoding="UTF-8"?>
<wsdl:definitions xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/"
                  xmlns:tns="%s"
                  xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/"
                  xmlns:xsd="http://www.w3.org/2001/XMLSchema"
                  targetNamespace="%s">
  <wsdl:types>
    <xsd:schema targetNamespace="%s"
                elementFormDefault="%s"
                attributeFormDefault="%s">
      <xsd:complexType name="ChildType">
        <xsd:sequence>
          <xsd:element name="Label" type="xsd:string"/>
        </xsd:sequence>
        <xsd:attribute name="Code" type="xsd:string"/>
      </xsd:complexType>
      <xsd:complexType name="PayloadType">
        <xsd:sequence>
          <xsd:element name="Title" type="xsd:string"/>
          <xsd:element name="Item" type="tns:ChildType" minOccurs="0" maxOccurs="unbounded"/>
        </xsd:sequence>
        <xsd:attribute name="Mode" type="xsd:string"/>
        <xsd:attribute name="Active" type="xsd:boolean"/>
      </xsd:complexType>
      <xsd:element name="Submit">
        <xsd:complexType>
          <xsd:sequence>
            <xsd:element name="Payload" type="tns:PayloadType"/>
            <xsd:element name="Flag" type="xsd:boolean"/>
          </xsd:sequence>
        </xsd:complexType>
      </xsd:element>
      <xsd:element name="SubmitResponse">
        <xsd:complexType>
          <xsd:sequence>
            <xsd:element name="Status" type="xsd:string"/>
          </xsd:sequence>
        </xsd:complexType>
      </xsd:element>
    </xsd:schema>
  </wsdl:types>
  <wsdl:message name="SubmitSoapIn">
    <wsdl:part name="parameters" element="tns:Submit"/>
  </wsdl:message>
  <wsdl:message name="SubmitSoapOut">
    <wsdl:part name="parameters" element="tns:SubmitResponse"/>
  </wsdl:message>
  <wsdl:portType name="ParityPortType">
    <wsdl:operation name="Submit">
      <wsdl:input message="tns:SubmitSoapIn"/>
      <wsdl:output message="tns:SubmitSoapOut"/>
    </wsdl:operation>
  </wsdl:portType>
  <wsdl:binding name="ParityBinding" type="tns:ParityPortType">
    <soap:binding style="document" transport="http://schemas.xmlsoap.org/soap/http"/>
    <wsdl:operation name="Submit">
      <soap:operation soapAction="http://example.org/parity/Submit" style="document"/>
      <wsdl:input><soap:body use="literal"/></wsdl:input>
      <wsdl:output><soap:body use="literal"/></wsdl:output>
    </wsdl:operation>
  </wsdl:binding>
  <wsdl:service name="ParityService">
    <wsdl:port name="ParityPort" binding="tns:ParityBinding">
      <soap:address location="http://example.org/service"/>
    </wsdl:port>
  </wsdl:service>
</wsdl:definitions>',
    ns,
    ns,
    ns,
    element_form_default,
    attribute_form_default
  )
}

mock_soap_ok <- function(url, ns = "http://example.org/parity") {
  response_xml <- sprintf(
    paste0(
      '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">',
      "<soap:Body>",
      '<SubmitResponse xmlns="%s"><Status>ok</Status></SubmitResponse>',
      "</soap:Body>",
      "</soap:Envelope>"
    ),
    ns
  )
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

test_that("Dynamic and static clients emit identical valid XML across all form defaults", {
  ns_uri <- "http://example.org/parity"
  combos <- list(
    c("qualified", "qualified"),
    c("qualified", "unqualified"),
    c("unqualified", "qualified"),
    c("unqualified", "unqualified")
  )

  for (idx in seq_along(combos)) {
    efd <- combos[[idx]][1]
    afd <- combos[[idx]][2]

    wsdl_file <- tempfile(fileext = ".wsdl")
    writeLines(build_parity_wsdl(efd, afd, ns_uri), wsdl_file)
    pkg_dir <- tempfile("parity_pkg_")
    dir.create(pkg_dir)
    pkg_name <- paste0("ParityPkg", idx)

    local({
      on.exit(unlink(c(wsdl_file, pkg_dir), recursive = TRUE), add = TRUE)

      # 1. Dynamic client request capture
      dyn_body <- NULL
      dyn_transport <- function(url, handle, body, ...) {
        dyn_body <<- body
        mock_soap_ok(url, ns_uri)
      }
      dyn_svc <- Service$new(wsdl_file)
      dyn_svc$client$transport <- dyn_transport

      dyn_c1 <- dyn_svc$factory$create("ChildType", Code = "C1", Label = "First")
      dyn_c2 <- dyn_svc$factory$create("ChildType", Code = "C2", Label = "Second")
      dyn_payload <- dyn_svc$factory$create(
        "PayloadType",
        Mode = "BATCH",
        Active = TRUE,
        Title = "Demo",
        Item = list(dyn_c1, dyn_c2)
      )
      dyn_res <- dyn_svc$client$Submit(Payload = dyn_payload, Flag = FALSE)
      expect_equal(dyn_res$Status, "ok")

      # 2. Static client request capture
      gen <- Generator$new(WSDLParser$new(wsdl_file))
      gen$generate_package(pkg_dir, pkg_name)
      pkg_path <- file.path(pkg_dir, pkg_name)
      pkgload::load_all(pkg_path, quiet = TRUE)
      on.exit(pkgload::unload(pkg_name), add = TRUE)

      stat_body <- NULL
      stat_transport <- function(url, action, body, config, handle) {
        stat_body <<- body
        mock_soap_ok(url, ns_uri)
      }
      ns_env <- asNamespace(pkg_name)
      stat_client <- ns_env$new_client(transport = stat_transport)
      stat_c1 <- stat_client$types$ChildType(Code = "C1", Label = "First")
      stat_c2 <- stat_client$types$ChildType(Code = "C2", Label = "Second")
      stat_payload <- stat_client$types$PayloadType(
        Mode = "BATCH",
        Active = TRUE,
        Title = "Demo",
        Item = list(stat_c1, stat_c2)
      )
      stat_res <- stat_client$Submit(Payload = stat_payload, Flag = FALSE)
      expect_equal(stat_res$Status, "ok")

      # Parity check: byte-for-byte identical XML
      expect_identical(dyn_body, stat_body)

      # Structural XML validity & namespace checks
      doc <- xml2::read_xml(dyn_body)
      doc_ns <- xml2::xml_ns(doc)
      expect_equal(unname(doc_ns["tns"]), ns_uri)

      payload_node <- xml2::xml_find_first(doc, ".//*[local-name()='Payload']")
      title_node <- xml2::xml_find_first(doc, ".//*[local-name()='Title']")
      flag_node <- xml2::xml_find_first(doc, ".//*[local-name()='Flag']")
      item_nodes <- xml2::xml_find_all(doc, ".//*[local-name()='Item']")
      expect_length(item_nodes, 2)
      expect_equal(xml2::xml_text(flag_node), "false")

      if (efd == "qualified") {
        expect_false(identical(xml2::xml_attr(payload_node, "xmlns"), ""))
        expect_true(is.na(xml2::xml_attr(title_node, "xmlns")))
        expect_true(is.na(xml2::xml_attr(flag_node, "xmlns")))
      } else {
        expect_equal(xml2::xml_attr(payload_node, "xmlns"), "")
        expect_equal(xml2::xml_attr(flag_node, "xmlns"), "")
        expect_true(is.na(xml2::xml_attr(title_node, "xmlns")))
      }

      if (afd == "qualified") {
        expect_true(grepl('tns:Mode="BATCH"', dyn_body))
        expect_true(grepl('tns:Active="true"', dyn_body))
        expect_true(grepl('tns:Code="C1"', dyn_body))
      } else {
        expect_false(grepl("tns:Mode=", dyn_body))
        expect_true(grepl('Mode="BATCH"', dyn_body))
        expect_true(grepl('Active="true"', dyn_body))
        expect_true(grepl('Code="C1"', dyn_body))
      }
    })
  }
})

test_that("XMLSerializer qualifies named lists and simple types when elementFormDefault is qualified", {
  ns_uri <- "http://example.org/direct"
  serializer <- XMLSerializer$new(
    element_form_default = "qualified",
    attribute_form_default = "qualified",
    target_namespace = ns_uri
  )

  doc <- xml2::xml_new_root("root", "xmlns:tns" = ns_uri)
  serializer$serialize(doc, "SimpleVal", "hello")
  serializer$serialize(doc, "NamedObj", list(Inner = "world", Count = 2))

  xml_str <- as.character(doc)
  expect_true(grepl(sprintf('<SimpleVal xmlns="%s">hello</SimpleVal>', ns_uri), xml_str))
  expect_true(grepl(sprintf('<NamedObj xmlns="%s">', ns_uri), xml_str))
  expect_true(grepl("<Inner>world</Inner>", xml_str))
  expect_true(grepl("<Count>2</Count>", xml_str))
})
