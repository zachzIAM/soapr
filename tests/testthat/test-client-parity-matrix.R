build_parity_response <- function(
  url,
  body_text,
  status = 200L,
  headers = list(`content-type` = "text/xml; charset=utf-8")
) {
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

write_parity_wsdl <- function(
  path,
  element_form_default = "qualified",
  attribute_form_default = "unqualified"
) {
  wsdl_xml <- sprintf(
    '<?xml version="1.0" encoding="UTF-8"?>
<definitions xmlns="http://schemas.xmlsoap.org/wsdl/"
             xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/"
             xmlns:tns="http://example.com/parity"
             xmlns:xsd="http://www.w3.org/2001/XMLSchema"
             targetNamespace="http://example.com/parity"
             name="ParityService">
  <types>
    <xsd:schema targetNamespace="http://example.com/parity"
                elementFormDefault="%s"
                attributeFormDefault="%s">
      <xsd:simpleType name="PriorityEnum">
        <xsd:restriction base="xsd:string">
          <xsd:enumeration value="LOW"/>
          <xsd:enumeration value="HIGH"/>
        </xsd:restriction>
      </xsd:simpleType>

      <xsd:element name="ScalarTag" type="xsd:int"/>

      <xsd:complexType name="TagItem">
        <xsd:sequence>
          <xsd:element name="Key" type="xsd:string"/>
          <xsd:element name="Score" type="xsd:double"/>
        </xsd:sequence>
        <xsd:attribute name="active" type="xsd:boolean"/>
      </xsd:complexType>

      <xsd:complexType name="ParityRequest">
        <xsd:sequence>
          <xsd:element name="Id" type="xsd:long"/>
          <xsd:element name="Priority" type="tns:PriorityEnum" minOccurs="1"/>
          <xsd:element name="Enabled" type="xsd:boolean"/>
          <xsd:element name="CreatedDate" type="xsd:date"/>
          <xsd:element name="Tags" type="tns:TagItem" minOccurs="0" maxOccurs="unbounded"/>
          <xsd:element name="Labels" type="xsd:string" minOccurs="0" maxOccurs="unbounded"/>
        </xsd:sequence>
        <xsd:attribute name="version" type="xsd:int" use="required"/>
      </xsd:complexType>

      <xsd:element name="SubmitItem">
        <xsd:complexType>
          <xsd:sequence>
            <xsd:element name="Payload" type="tns:ParityRequest" minOccurs="1"/>
            <xsd:element name="Mode" type="tns:PriorityEnum" minOccurs="0"/>
          </xsd:sequence>
        </xsd:complexType>
      </xsd:element>

      <xsd:element name="SubmitItemResponse">
        <xsd:complexType>
          <xsd:sequence>
            <xsd:element name="Accepted" type="xsd:boolean"/>
            <xsd:element name="SequenceId" type="xsd:long"/>
            <xsd:element name="ProcessedDate" type="xsd:date"/>
            <xsd:element name="ProcessedAt" type="xsd:dateTime"/>
            <xsd:element name="ModeUsed" type="tns:PriorityEnum"/>
            <xsd:element name="TagValue" type="tns:ScalarTag"/>
            <xsd:element name="EchoTags" type="tns:TagItem" minOccurs="0" maxOccurs="unbounded"/>
            <xsd:element name="EchoLabels" type="xsd:string" minOccurs="0" maxOccurs="unbounded"/>
          </xsd:sequence>
          <xsd:attribute name="statusCode" type="xsd:int"/>
          <xsd:attribute name="cached" type="xsd:boolean"/>
        </xsd:complexType>
      </xsd:element>
    </xsd:schema>
  </types>

  <message name="SubmitItemSoapIn">
    <part name="parameters" element="tns:SubmitItem"/>
  </message>
  <message name="SubmitItemSoapOut">
    <part name="parameters" element="tns:SubmitItemResponse"/>
  </message>

  <portType name="ParityPortType">
    <operation name="SubmitItem">
      <input message="tns:SubmitItemSoapIn"/>
      <output message="tns:SubmitItemSoapOut"/>
    </operation>
  </portType>

  <binding name="ParitySoapBinding" type="tns:ParityPortType">
    <soap:binding transport="http://schemas.xmlsoap.org/soap/http" style="document"/>
    <operation name="SubmitItem">
      <soap:operation soapAction="http://example.com/parity/SubmitItem" style="document"/>
      <input><soap:body use="literal"/></input>
      <output><soap:body use="literal"/></output>
    </operation>
  </binding>

  <service name="ParityService">
    <port name="ParityPort" binding="tns:ParitySoapBinding">
      <soap:address location="http://example.com/parity/endpoint"/>
    </port>
  </service>
</definitions>',
    element_form_default,
    attribute_form_default
  )
  writeLines(wsdl_xml, path)
}

test_that("dynamic and static clients achieve full request, deserialization, and fault parity", {
  skip_if_not_installed("pkgload")

  configs <- list(
    list(
      pkg = "ParityQualifiedPkg",
      elem_form = "qualified",
      attr_form = "qualified"
    ),
    list(
      pkg = "ParityUnqualifiedPkg",
      elem_form = "unqualified",
      attr_form = "unqualified"
    )
  )

  for (cfg in configs) {
    tmp_dir <- tempfile("soapr_parity_")
    dir.create(tmp_dir, recursive = TRUE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    wsdl_path <- file.path(tmp_dir, "parity.wsdl")
    write_parity_wsdl(wsdl_path, cfg$elem_form, cfg$attr_form)

    pkg_dir <- file.path(tmp_dir, cfg$pkg)
    gen <- Generator$new(WSDLParser$new(wsdl_path))
    gen$generate_package(tmp_dir, cfg$pkg)
    pkgload::load_all(pkg_dir, quiet = TRUE)
    on.exit(try(pkgload::unload(cfg$pkg), silent = TRUE), add = TRUE, after = FALSE)
    ns_env <- asNamespace(cfg$pkg)

    dyn_calls <- list()
    stat_calls <- list()
    next_response <- NULL

    svc <- Service$new(wsdl_path)
    svc$client$transport <- function(url, handle, body, ...) {
      extra <- list(...)
      headers <- unlist(lapply(extra, function(x) x$headers))
      dyn_calls[[length(dyn_calls) + 1L]] <<- list(
        url = url,
        soap_action = unname(headers[["SOAPAction"]]),
        body = body
      )
      next_response
    }

    stat_client <- ns_env$new_client(
      transport = function(url, action, body, config, handle) {
        stat_calls[[length(stat_calls) + 1L]] <<- list(
          url = url,
          soap_action = action,
          body = body
        )
        next_response
      }
    )

    # Custom SOAP Header parity
    auth_header <- xml2::xml_new_root("tns:AuthToken", "xmlns:tns" = "http://example.com/parity")
    xml2::xml_text(auth_header) <- "secret-token"
    svc$client$add_header(auth_header)
    stat_client$add_header(auth_header)

    # Helper to construct matching request inputs cleanly
    make_inputs <- function(factory_create, tag_ctor, req_ctor, n_tags = 2L, n_labels = 2L) {
      tags <- if (n_tags == 0L) {
        list()
      } else {
        lapply(seq_len(n_tags), function(i) {
          tag_ctor(active = (i %% 2L == 1L), Key = paste0("k", i), Score = i * 1.5)
        })
      }
      labels <- if (n_labels == 0L) list() else as.list(paste0("lbl", seq_len(n_labels)))
      req_ctor(
        version = 7L,
        Id = bit64::as.integer64("9007199254740993"),
        Priority = "HIGH",
        Enabled = TRUE,
        CreatedDate = as.Date("2025-03-10"),
        Tags = tags,
        Labels = labels
      )
    }

    dyn_payload <- make_inputs(
      svc$factory$create,
      function(...) svc$factory$create("TagItem", ...),
      function(...) svc$factory$create("ParityRequest", ...)
    )
    stat_payload <- make_inputs(
      NULL,
      function(...) stat_client$types$TagItem(...),
      function(...) stat_client$types$ParityRequest(...)
    )

    # 1. Complex nested request + multi-item unbounded response + all primitive types
    response_multi_xml <- paste0(
      '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/"',
      ' xmlns:tns="http://example.com/parity">',
      "<soap:Body>",
      '<tns:SubmitItemResponse statusCode="201" cached="true">',
      "<Accepted>1</Accepted>",
      "<SequenceId>9007199254740995</SequenceId>",
      "<ProcessedDate>2025-03-11+02:00</ProcessedDate>",
      "<ProcessedAt>2025-03-11T10:30:00+02:00</ProcessedAt>",
      "<ModeUsed>LOW</ModeUsed>",
      "<TagValue>42</TagValue>",
      '<EchoTags active="true"><Key>a</Key><Score>1.25</Score></EchoTags>',
      '<EchoTags active="false"><Key>b</Key><Score>2.5</Score></EchoTags>',
      "<EchoLabels>alpha</EchoLabels>",
      "<EchoLabels>beta</EchoLabels>",
      "</tns:SubmitItemResponse>",
      "</soap:Body></soap:Envelope>"
    )
    next_response <- build_parity_response(
      "http://example.com/parity/endpoint",
      response_multi_xml,
      headers = list(`CONTENT-TYPE` = "application/soap+xml; charset=utf-8")
    )

    dyn_res <- svc$client$SubmitItem(Payload = dyn_payload, Mode = "HIGH")
    stat_res <- stat_client$SubmitItem(Payload = stat_payload, Mode = "HIGH")

    expect_equal(length(dyn_calls), 1L)
    expect_equal(length(stat_calls), 1L)
    expect_identical(dyn_calls[[1]]$url, stat_calls[[1]]$url)
    expect_identical(dyn_calls[[1]]$soap_action, stat_calls[[1]]$soap_action)
    expect_identical(
      as.character(xml2::read_xml(dyn_calls[[1]]$body)),
      as.character(xml2::read_xml(stat_calls[[1]]$body))
    )

    expect_identical(class(dyn_res), class(stat_res))
    expect_identical(attr(dyn_res, "soap_type"), attr(stat_res, "soap_type"))
    expect_identical(attr(dyn_res, "soap_attrs"), attr(stat_res, "soap_attrs"))
    expect_equal(dyn_res, stat_res)
    expect_true(bit64::is.integer64(stat_res$SequenceId))
    expect_s3_class(stat_res$ProcessedDate, "Date")
    expect_s3_class(stat_res$ProcessedAt, "POSIXct")
    expect_identical(stat_res$TagValue, 42L)
    expect_length(stat_res$EchoTags, 2L)
    expect_length(stat_res$EchoLabels, 2L)

    # 2. Zero and single unbounded occurrences parity + missing optional fields
    dyn_empty_payload <- make_inputs(
      svc$factory$create,
      function(...) svc$factory$create("TagItem", ...),
      function(...) svc$factory$create("ParityRequest", ...),
      n_tags = 0L,
      n_labels = 0L
    )
    stat_empty_payload <- make_inputs(
      NULL,
      function(...) stat_client$types$TagItem(...),
      function(...) stat_client$types$ParityRequest(...),
      n_tags = 0L,
      n_labels = 0L
    )

    response_single_xml <- paste0(
      '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">',
      "<soap:Body>",
      '<SubmitItemResponse statusCode="200" cached="0">',
      "<Accepted>false</Accepted>",
      "<SequenceId>1</SequenceId>",
      "<ProcessedDate>2025-01-01Z</ProcessedDate>",
      "<ProcessedAt>2025-01-01T00:00:00Z</ProcessedAt>",
      "<ModeUsed>HIGH</ModeUsed>",
      "<TagValue>7</TagValue>",
      '<EchoTags active="0"><Key>only</Key><Score>3.5</Score></EchoTags>',
      "</SubmitItemResponse>",
      "</soap:Body></soap:Envelope>"
    )
    next_response <- build_parity_response("http://example.com/parity/endpoint", response_single_xml)

    dyn_single <- svc$client$SubmitItem(Payload = dyn_empty_payload)
    stat_single <- stat_client$SubmitItem(Payload = stat_empty_payload)
    expect_identical(
      as.character(xml2::read_xml(dyn_calls[[2]]$body)),
      as.character(xml2::read_xml(stat_calls[[2]]$body))
    )
    expect_equal(dyn_single, stat_single)
    expect_length(stat_single$EchoTags, 1L)
    expect_identical(stat_single$EchoLabels, list())

    # 3. Empty Body -> NULL parity
    next_response <- build_parity_response(
      "http://example.com/parity/endpoint",
      '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/"><soap:Body/></soap:Envelope>'
    )
    expect_null(svc$client$SubmitItem(Payload = dyn_empty_payload))
    expect_null(stat_client$SubmitItem(Payload = stat_empty_payload))

    # 4. Unknown wrapper element fallback -> xml_document parity
    unknown_resp_xml <- paste0(
      '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">',
      "<soap:Body><UnknownResp><A>1</A></UnknownResp></soap:Body>",
      "</soap:Envelope>"
    )
    next_response <- build_parity_response("http://example.com/parity/endpoint", unknown_resp_xml)
    dyn_unk <- svc$client$SubmitItem(Payload = dyn_empty_payload)
    stat_unk <- stat_client$SubmitItem(Payload = stat_empty_payload)
    expect_s3_class(dyn_unk, "xml_document")
    expect_s3_class(stat_unk, "xml_document")

    # 5. SOAP 1.1 & SOAP 1.2 Fault parity
    fault11_xml <- paste0(
      '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">',
      "<soap:Body><soap:Fault>",
      "<faultcode>soap:Client</faultcode>",
      "<faultstring>Invalid request</faultstring>",
      "<detail>Field version missing</detail>",
      "</soap:Fault></soap:Body></soap:Envelope>"
    )
    next_response <- build_parity_response("http://example.com/parity/endpoint", fault11_xml, status = 500L)
    dyn_err <- tryCatch(svc$client$SubmitItem(Payload = dyn_empty_payload), error = function(e) e$message)
    stat_err <- tryCatch(stat_client$SubmitItem(Payload = stat_empty_payload), error = function(e) e$message)
    expect_identical(dyn_err, stat_err)
    expect_match(stat_err, "SOAP Fault: \\[soap:Client\\] Invalid request")
    expect_match(stat_err, "Detail: Field version missing")

    fault12_xml <- paste0(
      '<env:Envelope xmlns:env="http://www.w3.org/2003/05/soap-envelope">',
      "<env:Body><env:Fault>",
      "<env:Code><env:Value>env:Sender</env:Value></env:Code>",
      "<env:Reason><env:Text>Bad payload</env:Text></env:Reason>",
      "<env:Detail>Bad TagItem</env:Detail>",
      "</env:Fault></env:Body></env:Envelope>"
    )
    next_response <- build_parity_response("http://example.com/parity/endpoint", fault12_xml, status = 500L)
    dyn_err12 <- tryCatch(svc$client$SubmitItem(Payload = dyn_empty_payload), error = function(e) e$message)
    stat_err12 <- tryCatch(stat_client$SubmitItem(Payload = stat_empty_payload), error = function(e) e$message)
    expect_identical(dyn_err12, stat_err12)

    # 6. Non-XML HTTP 500 error parity (no stray warning or debug log)
    next_response <- build_parity_response(
      "http://example.com/parity/endpoint",
      "<html><body>Server Error</body></html>",
      status = 502L,
      headers = list(`content-type` = "text/html")
    )
    expect_no_warning({
      dyn_http_err <- tryCatch(svc$client$SubmitItem(Payload = dyn_empty_payload), error = function(e) e$message)
      stat_http_err <- tryCatch(stat_client$SubmitItem(Payload = stat_empty_payload), error = function(e) e$message)
      expect_identical(dyn_http_err, stat_http_err)
    })

    # 7. convert_builtin_type list unlisting parity
    expect_identical(
      svc$factory$convert_builtin_type(list("42"), "xsd:int"),
      ns_env$convert_builtin_type(list("42"), "xsd:int")
    )
    expect_identical(
      svc$factory$convert_builtin_type(list("2025-03-10"), "xsd:date"),
      ns_env$convert_builtin_type(list("2025-03-10"), "xsd:date")
    )

    # 8. deserialize_dynamic parity on nested anonymous nodes
    anon_node <- xml2::read_xml(
      '<Wrapper flag="yes"><Item>10</Item><Item>20</Item><Nested><Name>Zed</Name></Nested></Wrapper>'
    )
    dyn_anon <- svc$factory$deserialize_dynamic(anon_node, "UnknownType")
    stat_anon <- ns_env$deserialize_dynamic(anon_node, "UnknownType", ns_env$get_schema(), stat_client$types)
    expect_equal(dyn_anon, stat_anon)
  }
})
