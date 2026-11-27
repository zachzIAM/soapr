test_that("WSDLParser handles ref attributes correctly", {
  wsdl_content <- '
<definitions targetNamespace="http://example.com/refs"
             xmlns:tns="http://example.com/refs"
             xmlns:xsd="http://www.w3.org/2001/XMLSchema"
             xmlns="http://schemas.xmlsoap.org/wsdl/">
  <types>
    <xsd:schema targetNamespace="http://example.com/refs">
      <xsd:element name="GlobalString" type="xsd:string"/>
      <xsd:complexType name="MyComplexType">
        <xsd:sequence>
          <xsd:element ref="tns:GlobalString"/>
        </xsd:sequence>
      </xsd:complexType>
      <xsd:element name="TopLevelElement">
        <xsd:complexType>
          <xsd:sequence>
             <xsd:element name="dummy" type="xsd:int"/>
          </xsd:sequence>
        </xsd:complexType>
      </xsd:element>
      <xsd:complexType name="AnotherComplexType">
         <xsd:sequence>
            <xsd:element ref="tns:TopLevelElement"/>
         </xsd:sequence>
      </xsd:complexType>
       <xsd:element name="MissingRefElement">
        <xsd:complexType>
          <xsd:sequence>
             <xsd:element ref="tns:RefThatDoesNotExist"/>
          </xsd:sequence>
        </xsd:complexType>
      </xsd:element>
    </xsd:schema>
  </types>
</definitions>'

  # Mock reading from file by using a temp file
  tmp <- tempfile(fileext = ".wsdl")
  writeLines(wsdl_content, tmp)

  wsdl <- WSDLParser$new(tmp)
  types <- wsdl$get_type_definitions()

  # Test MyComplexType
  # Should have element GlobalString of type string
  expect_true("MyComplexType" %in% names(types))
  my_ct <- types[["MyComplexType"]]
  expect_equal(my_ct$kind, "complex")
  expect_true("GlobalString" %in% names(my_ct$elements))
  expect_equal(my_ct$elements[["GlobalString"]]$type, "string")

  # Test AnotherComplexType
  # Should have element TopLevelElement of type TopLevelElement
  expect_true("AnotherComplexType" %in% names(types))
  another_ct <- types[["AnotherComplexType"]]
  expect_true("TopLevelElement" %in% names(another_ct$elements))
  expect_equal(
    another_ct$elements[["TopLevelElement"]]$type,
    "TopLevelElement"
  )

  # And TopLevelElement should exist as a type
  expect_true("TopLevelElement" %in% names(types))

  # Test MissingRefElement
  # Should fallback to using ref name as type
  expect_true("MissingRefElement" %in% names(types))
  missing_ct <- types[["MissingRefElement"]]
  expect_true("RefThatDoesNotExist" %in% names(missing_ct$elements))
  # Fallback: type name is same as ref name
  expect_equal(
    missing_ct$elements[["RefThatDoesNotExist"]]$type,
    "RefThatDoesNotExist"
  )
})
