test_that("WSDLParser parses multiple inline schema blocks and tracks per-schema metadata", {
  tmp_dir <- tempfile("soapr_multischema_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  wsdl_path <- file.path(tmp_dir, "multi.wsdl")
  wsdl_xml <- '<?xml version="1.0" encoding="UTF-8"?>
<definitions xmlns="http://schemas.xmlsoap.org/wsdl/"
             xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/"
             xmlns:tns="http://example.com/service"
             xmlns:mod="http://example.com/model"
             xmlns:xsd="http://www.w3.org/2001/XMLSchema"
             targetNamespace="http://example.com/service">
  <types>
    <xsd:schema targetNamespace="http://example.com/model"
                elementFormDefault="unqualified"
                attributeFormDefault="qualified">
      <xsd:complexType name="ModelItem">
        <xsd:sequence>
          <xsd:element name="Label" type="xsd:string"/>
        </xsd:sequence>
        <xsd:attribute name="code" type="xsd:int"/>
      </xsd:complexType>
    </xsd:schema>
    <xsd:schema targetNamespace="http://example.com/service"
                elementFormDefault="qualified"
                attributeFormDefault="unqualified">
      <xsd:import namespace="http://example.com/model"/>
      <xsd:element name="GetItem">
        <xsd:complexType>
          <xsd:sequence>
            <xsd:element name="Item" type="mod:ModelItem"/>
          </xsd:sequence>
        </xsd:complexType>
      </xsd:element>
    </xsd:schema>
  </types>
</definitions>'
  writeLines(wsdl_xml, wsdl_path)

  parser <- WSDLParser$new(wsdl_path)
  schemas <- parser$get_schemas()
  expect_length(schemas, 2L)
  expect_identical(schemas[[1]]$target_namespace, "http://example.com/model")
  expect_identical(schemas[[1]]$element_form_default, "unqualified")
  expect_identical(schemas[[1]]$attribute_form_default, "qualified")
  expect_identical(schemas[[2]]$target_namespace, "http://example.com/service")
  expect_identical(schemas[[2]]$element_form_default, "qualified")
  expect_identical(schemas[[2]]$attribute_form_default, "unqualified")

  # Primary WSDL defaults reflect matching targetNamespace inline schema
  expect_identical(parser$element_form_default, "qualified")
  expect_identical(parser$attribute_form_default, "unqualified")

  types <- parser$get_type_definitions()
  expect_true("ModelItem" %in% names(types))
  expect_true("GetItem" %in% names(types))
  expect_identical(types[["GetItem"]]$elements[["Item"]]$type, "ModelItem")
})

test_that("WSDLParser extracts types across all schemas in real multi-schema WSDL fixtures", {
  massbank <- WSDLParser$new(test_path("testdata/valid_wsdls/MassBankAPI.wsdl"))
  expect_length(massbank$get_schemas(), 2L)
  mb_types <- massbank$get_type_definitions()
  expect_true("SearchResult" %in% names(mb_types))
  expect_true("searchSpectrum" %in% names(mb_types))
  expect_true("searchSpectrumResponse" %in% names(mb_types))

  eutils <- WSDLParser$new(test_path("testdata/valid_wsdls/eutils.wsdl"))
  expect_gt(length(eutils$get_schemas()), 1L)
  eu_types <- eutils$get_type_definitions()
  expect_true("eGQueryResultType" %in% names(eu_types))
  expect_true("DbInfoType" %in% names(eu_types))
})

test_that("WSDLParser resolves relative <xsd:import>, <xsd:include>, and cross-schema refs", {
  tmp_dir <- tempfile("soapr_imports_")
  dir.create(file.path(tmp_dir, "schemas"), recursive = TRUE)
  dir.create(file.path(tmp_dir, "shared"), recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  shared_xsd <- '<?xml version="1.0" encoding="UTF-8"?>
<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
           elementFormDefault="qualified">
  <xs:simpleType name="StatusCode">
    <xs:restriction base="xs:string">
      <xs:enumeration value="ACTIVE"/>
      <xs:enumeration value="ARCHIVED"/>
    </xs:restriction>
  </xs:simpleType>
  <xs:element name="SharedCount" type="xs:int"/>
</xs:schema>'
  writeLines(shared_xsd, file.path(tmp_dir, "shared", "common.xsd"))

  domain_xsd <- '<?xml version="1.0" encoding="UTF-8"?>
<xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema"
            xmlns:dom="http://example.com/domain"
            targetNamespace="http://example.com/domain"
            elementFormDefault="qualified"
            attributeFormDefault="unqualified">
  <xsd:include schemaLocation="../shared/common.xsd"/>
  <xsd:complexType name="Account">
    <xsd:sequence>
      <xsd:element name="Name" type="xsd:string"/>
      <xsd:element name="Status" type="dom:StatusCode"/>
      <xsd:element ref="dom:SharedCount"/>
    </xsd:sequence>
  </xsd:complexType>
</xsd:schema>'
  writeLines(domain_xsd, file.path(tmp_dir, "schemas", "domain.xsd"))

  wsdl_xml <- '<?xml version="1.0" encoding="UTF-8"?>
<definitions xmlns="http://schemas.xmlsoap.org/wsdl/"
             xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/"
             xmlns:tns="http://example.com/svc"
             xmlns:dom="http://example.com/domain"
             xmlns:xsd="http://www.w3.org/2001/XMLSchema"
             targetNamespace="http://example.com/svc">
  <types>
    <xsd:schema targetNamespace="http://example.com/svc"
                elementFormDefault="qualified">
      <xsd:import namespace="http://example.com/domain"
                  schemaLocation="schemas/domain.xsd"/>
      <xsd:element name="CreateAccount">
        <xsd:complexType>
          <xsd:sequence>
            <xsd:element name="Account" type="dom:Account"/>
          </xsd:sequence>
        </xsd:complexType>
      </xsd:element>
    </xsd:schema>
  </types>
</definitions>'
  wsdl_path <- file.path(tmp_dir, "svc.wsdl")
  writeLines(wsdl_xml, wsdl_path)

  parser <- WSDLParser$new(wsdl_path)
  schemas <- parser$get_schemas()
  expect_length(schemas, 3L)
  expect_identical(vapply(schemas, `[[`, character(1), "kind"), c("inline", "import", "include"))
  # Chameleon include inherits enclosing targetNamespace
  expect_identical(schemas[[3]]$target_namespace, "http://example.com/domain")

  types <- parser$get_type_definitions()
  expect_true(all(c("CreateAccount", "Account", "StatusCode", "SharedCount") %in% names(types)))
  expect_identical(types[["StatusCode"]]$values, c("ACTIVE", "ARCHIVED"))
  expect_identical(types[["Account"]]$elements[["SharedCount"]]$type, "int")
})

test_that("WSDLParser resolves remote URL imports and breaks circular imports cleanly", {
  tmp_dir <- tempfile("soapr_cycles_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # 1. Local mutual recursion & self-include cycle: a.xsd <-> b.xsd and missing target
  a_xsd <- '<?xml version="1.0" encoding="UTF-8"?>
<xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema"
            targetNamespace="http://example.com/a">
  <xsd:import namespace="http://example.com/b" schemaLocation="b.xsd"/>
  <xsd:include schemaLocation="a.xsd"/>
  <xsd:complexType name="TypeA">
    <xsd:sequence><xsd:element name="A" type="xsd:string"/></xsd:sequence>
  </xsd:complexType>
</xsd:schema>'

  b_xsd <- '<?xml version="1.0" encoding="UTF-8"?>
<xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema"
            targetNamespace="http://example.com/b">
  <xsd:import namespace="http://example.com/a" schemaLocation="./a.xsd"/>
  <xsd:import namespace="http://example.com/missing" schemaLocation="does_not_exist.xsd"/>
  <xsd:complexType name="TypeB">
    <xsd:sequence><xsd:element name="B" type="xsd:int"/></xsd:sequence>
  </xsd:complexType>
</xsd:schema>'

  writeLines(a_xsd, file.path(tmp_dir, "a.xsd"))
  writeLines(b_xsd, file.path(tmp_dir, "b.xsd"))

  wsdl_xml <- '<?xml version="1.0" encoding="UTF-8"?>
<definitions xmlns="http://schemas.xmlsoap.org/wsdl/"
             xmlns:xsd="http://www.w3.org/2001/XMLSchema"
             targetNamespace="http://example.com/root">
  <types>
    <xsd:schema targetNamespace="http://example.com/root">
      <xsd:import namespace="http://example.com/a" schemaLocation="a.xsd"/>
      <xsd:import namespace="http://example.com/a" schemaLocation="./a.xsd"/>
    </xsd:schema>
  </types>
</definitions>'
  wsdl_path <- file.path(tmp_dir, "root.wsdl")
  writeLines(wsdl_xml, wsdl_path)

  parser <- WSDLParser$new(wsdl_path)
  types <- parser$get_type_definitions()
  expect_true(all(c("TypeA", "TypeB") %in% names(types)))
  expect_length(parser$get_schemas(), 3L)

  # 2. Remote URL resolution and cycle tracking via schema_loader
  state <- new.env(parent = emptyenv())
  state$calls <- list()
  remote_docs <- list(
    "https://example.org/api/v1/domain.xsd" = '<?xml version="1.0" encoding="UTF-8"?>
<xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema"
            targetNamespace="https://example.org/api/v1">
  <xsd:include schemaLocation="../common/base.xsd"/>
  <xsd:complexType name="RemoteOrder">
    <xsd:sequence><xsd:element name="OrderId" type="xsd:string"/></xsd:sequence>
  </xsd:complexType>
</xsd:schema>',
    "https://example.org/api/common/base.xsd" = '<?xml version="1.0" encoding="UTF-8"?>
<xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema"
            targetNamespace="https://example.org/api/v1">
  <xsd:import namespace="https://example.org/api/v1" schemaLocation="../v1/domain.xsd"/>
  <xsd:complexType name="RemoteBase">
    <xsd:sequence><xsd:element name="BaseId" type="xsd:int"/></xsd:sequence>
  </xsd:complexType>
</xsd:schema>'
  )

  remote_wsdl <- '<?xml version="1.0" encoding="UTF-8"?>
<definitions xmlns="http://schemas.xmlsoap.org/wsdl/"
             xmlns:xsd="http://www.w3.org/2001/XMLSchema"
             targetNamespace="https://example.org/svc">
  <types>
    <xsd:schema targetNamespace="https://example.org/svc">
      <xsd:import namespace="https://example.org/api/v1"
                  schemaLocation="https://example.org/api/v1/domain.xsd"/>
    </xsd:schema>
  </types>
</definitions>'
  remote_wsdl_path <- file.path(tmp_dir, "remote.wsdl")
  writeLines(remote_wsdl, remote_wsdl_path)

  remote_parser <- WSDLParser$new(
    remote_wsdl_path,
    schema_loader = function(uri) {
      state$calls[[uri]] <- (state$calls[[uri]] %||% 0L) + 1L
      remote_docs[[uri]]
    }
  )
  remote_types <- remote_parser$get_type_definitions()
  expect_true(all(c("RemoteOrder", "RemoteBase") %in% names(remote_types)))
  expect_identical(state$calls[["https://example.org/api/v1/domain.xsd"]], 1L)
  expect_identical(state$calls[["https://example.org/api/common/base.xsd"]], 1L)
})
