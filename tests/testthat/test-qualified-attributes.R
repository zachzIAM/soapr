library(testthat)
library(soapr)

test_that("XMLSerializer handles qualified attributes", {
    # Mock WSDL with attributeFormDefault="qualified"
    wsdl_content <- '<?xml version="1.0" encoding="UTF-8"?>
<wsdl:definitions xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/"
                  xmlns:tns="http://example.org/qualified"
                  xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/"
                  xmlns:xsd="http://www.w3.org/2001/XMLSchema"
                  targetNamespace="http://example.org/qualified">
    <wsdl:types>
        <xsd:schema targetNamespace="http://example.org/qualified"
                    elementFormDefault="qualified"
                    attributeFormDefault="qualified">
            <xsd:complexType name="QualifiedType">
                <xsd:attribute name="Attr1" type="xsd:string"/>
            </xsd:complexType>
        </xsd:schema>
    </wsdl:types>
    <wsdl:message name="Msg"><wsdl:part name="p" type="tns:QualifiedType"/></wsdl:message>
    <wsdl:portType name="PT"><wsdl:operation name="Op"><wsdl:input message="tns:Msg"/></wsdl:operation></wsdl:portType>
    <wsdl:binding name="B" type="tns:PT">
      <soap:binding transport="http://schemas.xmlsoap.org/soap/http"/>
      <wsdl:operation name="Op"><soap:operation soapAction="Op"/><wsdl:input><soap:body use="literal"/></wsdl:input></wsdl:operation>
    </wsdl:binding>
    <wsdl:service name="S">
      <wsdl:port name="P" binding="tns:B"><soap:address location="http://e.org"/></wsdl:port>
    </wsdl:service>
</wsdl:definitions>'

    wsdl_file <- tempfile(fileext = ".wsdl")
    writeLines(wsdl_content, wsdl_file)
    on.exit(unlink(wsdl_file))

    service <- Service$new(wsdl_file)
    factory <- service$factory

    # Create object
    obj <- factory$create("QualifiedType", Attr1 = "Value1")

    # Serialize
    serializer <- XMLSerializer$new(
        element_form_default = "qualified",
        attribute_form_default = "qualified",
        target_namespace = "http://example.org/qualified"
    )

    doc <- xml2::xml_new_root("root", "xmlns:tns" = "http://example.org/qualified")
    serializer$serialize(doc, "Obj", obj, ns = "http://example.org/qualified")

    # Check XML
    # Expected: <tns:Obj tns:Attr1="Value1" xmlns:tns="...">...</tns:Obj>
    # Note: xml2 might use a different prefix if tns is not mapped to tns prefix in the node internally,
    # but since we defined xmlns:tns on root, using "tns:Attr1" should work if we construct it that way.

    # However, XMLSerializer currently doesn't know the prefix 'tns'. It knows the URI.
    # If we set attribute name to "tns:Attr1", xml2 resolves 'tns' against definitions.

    xml_str <- as.character(doc)
    expect_true(grepl('tns:Attr1="Value1"', xml_str) || grepl('ns.:Attr1="Value1"', xml_str))
})
