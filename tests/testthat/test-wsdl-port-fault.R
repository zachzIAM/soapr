# Helper function to create a mock XML node

describe("WSDL Fault Node Validation", {

  test_that("check_xml_wsdl_node_type handles correct node type", {
    node <- create_mock_fault_node()
    expect_true(test_xml_wsdl_node_type(node, "fault"))
  })

  test_that("check_xml_wsdl_node_type rejects incorrect node type", {
    node <- create_mock_fault_node()
    xml2::xml_name(node) <- "wrong_type"
    expect_false(test_xml_wsdl_node_type(node, "fault"))
  })

  test_that("check_xml_wsdl_fault validates mandatory attributes", {
    # Valid fault node
    node <- create_mock_fault_node()
    expect_true(test_xml_wsdl_fault(node))

    # Missing name attribute
    node_no_name <- create_mock_fault_node()
    xml2::xml_set_attr(node_no_name, "name", NULL)
    expect_false(test_xml_wsdl_fault(node_no_name))

    # Missing message attribute
    node_no_message <- create_mock_fault_node()
    xml2::xml_set_attr(node_no_message, "message", NULL)
    expect_false(test_xml_wsdl_fault(node_no_message))
  })

  test_that("assert_xml_wsdl_fault throws errors for invalid nodes", {
    # Missing node
    expect_error(assert_xml_wsdl_fault(), "argument")

    # Node without name
    node_no_name <- create_mock_fault_node()
    xml2::xml_set_attr(node_no_name, "name", NULL)
    expect_error(assert_xml_wsdl_fault(node_no_name), "'name' attribute")

    # Node without message
    node_no_message <- create_mock_fault_node()
    xml2::xml_set_attr(node_no_message, "message", NULL)
    expect_error(assert_xml_wsdl_fault(node_no_message), "'message' attribute")
  })
})

describe("wsdl_port_fault Constructor", {
  test_that("constructor creates object with correct attributes", {
    # Create a mock XML node
    mock_node <- create_mock_fault_node(
      name = "SampleFault",
      message = "tns:SampleFaultMessage"
    )

    # Create wsdl_port_fault object
    fault <- wsdl_port_fault(mock_node)

    # Validate object creation
    expect_true(S7::S7_inherits(fault, wsdl_port_fault))
    expect_equal(fault@name, "SampleFault")
    expect_equal(fault@message, "tns:SampleFaultMessage")
    expect_null(fault@documentation)
  })

  test_that("constructor handles different namespace formats", {
    # Test with fully qualified message type
    node1 <- create_mock_fault_node(
      name = "Fault1",
      message = "tns:FaultType"
    )
    fault1 <- wsdl_port_fault(node1)
    expect_equal(fault1@message, "tns:FaultType")

    # Test with local message type
    node2 <- create_mock_fault_node(
      name = "Fault2",
      message = "FaultType"
    )
    fault2 <- wsdl_port_fault(node2)
    expect_equal(fault2@message, "FaultType")
  })

  test_that("constructor handles docmentation", {
    # Create a mock XML node
    docs <- "Example documentation"

    mock_node <- create_mock_fault_node(
      name = "SampleFault",
      message = "tns:SampleFaultMessage",
      documentation = docs
    )

    # Create wsdl_port_fault object
    fault <- wsdl_port_fault(mock_node)

    # Validate object creation
    expect_true(S7::S7_inherits(fault, wsdl_port_fault))
    expect_equal(fault@name, "SampleFault")
    expect_equal(fault@message, "tns:SampleFaultMessage")
    expect_equal(fault@documentation, docs)
  })
})

describe("wsdl_port_fault Validation", {
  test_that("class prevents incorrect attribute types", {
    # Attempt to create object with incorrect types should fail

    expect_error(
      wsdl_port_fault(create_mock_fault_node(name = "")),
      regexp = "name Cannot be an empty string"
    )

    expect_error(
      wsdl_port_fault(create_mock_fault_node(name = "", message = "")),
      regexp = "name Cannot be an empty string"
    )

    expect_error(
      wsdl_port_fault(create_mock_fault_node(message = "")),
      regexp = "message Cannot be an empty string"
    )

  })

})
