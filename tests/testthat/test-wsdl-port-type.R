test_that("test parsing types from complete WSDLs", {

  files <- dir(file.path(testthat::test_path(), "testdata", "valid_wsdls"), full.names = TRUE)

  purrr::walk(files, ~{
    port_types <- parse_port_types(xml2::read_xml(.x))
    expect_type(port_types, "list")
    purrr::walk(port_types, ~{

      expect_s4_class(.x, "wsdl_port_type")
      expect_gt(length(.x@operations), 0)
      expect_false(is.null(names(.x@operations)))

      purrr::walk(.x@operations, ~expect_s4_class(.x, "wsdl_port_operation"))

    })
  })

})

# Mock parse function replacements
mock_parse_documentation <- function(node) {
  doc_node <- xml2::xml_find_first(node, ".//documentation", silent = TRUE)
  if (length(doc_node) > 0) xml2::xml_text(doc_node) else NA_character_
}

mock_parse_operation <- function(node) {
  list(name = xml2::xml_attr(node, "name"))
}

describe("WSDL PortType Node Validation", {
  test_that("check_xml_wsdl_port_type validates correct port type node", {
    # Create a valid port type node
    node <- create_mock_wsdl() |> xml2::xml_find_first(".//portType")

    # Validate the node
    expect_true(test_xml_wsdl_port_type(node))
  })

  test_that("check_xml_wsdl_port_type rejects node without name attribute", {
    # Create a port type node without name
    node <- create_mock_wsdl() |> xml2::xml_find_first(".//portType")
    xml2::xml_attr(node, "name") <- NULL

    # Validate the node
    expect_false(test_xml_wsdl_port_type(node))
  })

  test_that("check_xml_wsdl_port_type rejects incorrect node type", {
    # Create a node with wrong type
    node <- create_mock_wsdl() |> xml2::xml_find_first(".//portType")
    xml2::xml_name(node) <- "wrongType"

    # Validate the node
    expect_false(test_xml_wsdl_port_type(node))
  })
})

describe("port_type Constructor", {
  test_that("constructor creates object with correct attributes", {
    # Create a mock WSDL node
    node <- create_mock_wsdl(list(
      list(
        name = "ServicePortType",
        operations = list(
          list(name = "GetData", output_message = "Data"),
          list(name = "UpdateData", input_message = "Data")
        ),
        documentation = "Sample service documentation"
      )
    )) |> xml2::xml_find_first(".//portType")

    # Create port type object
    port_type_obj <- wsdl_port_type(node)

    # Validate object creation
    expect_true(S7::S7_inherits(port_type_obj, wsdl_port_type))
    expect_equal(port_type_obj@name, "ServicePortType")
    expect_equal(length(port_type_obj@operations), 2)
    expect_named(port_type_obj@operations, c("GetData", "UpdateData"))
  })

  test_that("constructor handles empty operations", {
    # Create a mock WSDL node with no operations
    node <- create_mock_wsdl(list(
      list(name = "EmptyPortType", operations = list())
    )) |> xml2::xml_find_first(".//portType")

    # Create port type object
    port_type_obj <- wsdl_port_type(node)

    # Validate object creation
    expect_true(S7::S7_inherits(port_type_obj, wsdl_port_type))
    expect_equal(port_type_obj@name, "EmptyPortType")
    expect_equal(length(port_type_obj@operations), 0)
  })
})

describe("parse_port_types Function", {
  test_that("parse_port_types extracts multiple port types", {
    # Create a WSDL with multiple port types
    port_data_1 <- list(
      name = "PortType1",
      operations = list(list(name = "Op1", input_message="IN_MSG"))
    )
    port_data_2 <- list(
      name = "PortType2",
      operations = list(list(name = "Op2", output_message="OUT_MSG"))
    )
    node <- create_mock_wsdl(list(
      port_data_1,
      port_data_2
    ))

    # Parse port types
    port_types <- parse_port_types(node)

    # Validate extraction
    expect_type(port_types, "list")
    expect_length(port_types, 2)
    expect_named(port_types, c("PortType1", "PortType2"))
  })

  test_that("parse_port_types throws error with no port types", {
    # Create a WSDL without port types
    node <- xml2::read_xml('<definitions xmlns:tns="http://example.com/namespace"></definitions>')

    # Expect error when no port types exist
    expect_error(parse_port_types(node), "port_type_count > 0")
  })
})

describe("parse_operations Function", {
  test_that("parse_operations extracts operations from port type", {
    # Create a mock port type node with operations
    node <- create_mock_wsdl() |> xml2::xml_find_first(".//portType")

    # Parse operations
    operations <- parse_operations(node)

    # Validate extraction
    expect_type(operations, "list")
    expect_length(operations, 3)
    expect_named(operations, c("Operation1", "Operation2", "Operation3"))
  })

  test_that("parse_operations handles port type with no operations", {
    # Create a port type node without operations
    node <- xml2::read_xml('<portType name="EmptyPortType"></portType>')

    # Parse operations
    operations <- parse_operations(node)

    # Validate extraction
    expect_type(operations, "list")
    expect_length(operations, 0)
  })
})

describe("parse_port_types validation", {
  test_that("parse_port_types fail when invalid ops", {
    # Create a WSDL with multiple port types
    node <- create_mock_wsdl(list(
      list(name = "PortType1", operations = list(list(name = "Op1")))
    ))

    # Parse port types
    expect_error(parse_port_types(node), regexp = "cannot have both.*missing")

  })

  test_that("parse_port_types throws error with no port types", {
    # Create a WSDL without port types
    node <- xml2::read_xml('<definitions xmlns:tns="http://example.com/namespace"></definitions>')

    # Expect error when no port types exist
    expect_error(parse_port_types(node), "port_type_count > 0")
  })
})
