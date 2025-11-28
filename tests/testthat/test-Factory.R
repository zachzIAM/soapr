test_that("Factory creates and deserializes objects correctly", {
    # Mock WSDL
    mock_wsdl <- list(
        get_type_definitions = function() {
            list(
                Person = list(
                    kind = "complex",
                    elements = list(
                        Name = list(type = "xsd:string", maxOccurs = "1"),
                        Age = list(type = "xsd:int", maxOccurs = "1"),
                        Address = list(
                            type = "Address",
                            maxOccurs = "unbounded"
                        )
                    ),
                    attributes = list(
                        ID = list(type = "xsd:string")
                    )
                ),
                Address = list(
                    kind = "complex",
                    elements = list(
                        Street = list(type = "xsd:string", maxOccurs = "1"),
                        City = list(type = "xsd:string", maxOccurs = "1")
                    ),
                    attributes = list()
                )
            )
        }
    )

    factory <- Factory$new(mock_wsdl)

    # Test create
    person <- factory$create("Person")
    expect_s3_class(person, "soap_object")
    expect_equal(attr(person, "soap_type"), "Person")
    expect_true("ID" %in% names(person))
    expect_true("Name" %in% names(person))
    expect_true("Address" %in% names(person))
    expect_true(is.list(person$Address)) # unbounded

    # Test deserialize
    xml_str <- '
  <Person ID="123">
    <Name>John Doe</Name>
    <Age>30</Age>
    <Address>
      <Street>123 Main St</Street>
      <City>New York</City>
    </Address>
    <Address>
      <Street>456 Elm St</Street>
      <City>Boston</City>
    </Address>
  </Person>
  '
    node <- xml2::read_xml(xml_str)

    obj <- factory$deserialize(node, "Person")

    expect_s3_class(obj, "soap_object")
    expect_equal(obj$ID, "123")
    expect_equal(obj$Name, "John Doe")
    expect_equal(obj$Age, "30") # Currently strings, TODO: type conversion

    expect_length(obj$Address, 2)
    expect_equal(obj$Address[[1]]$City, "New York")
    expect_equal(obj$Address[[2]]$City, "Boston")
})
