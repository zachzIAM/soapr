

# example S7 object ---------------------------------------------------------------------------


Person <- S7::new_class(
  name = "Person",
  properties = list(
    name = strict_string_prop("name", read_only = TRUE),
    nickname = strict_string_prop("nickname"),
    description = nullable_string_prop("description"),
    commentary = nullable_string_prop("commentary", read_only = TRUE)
  )
)


# dummy fault node ----------------------------------------------------------------------------


create_mock_fault_node <- function(name = "TestFault",
                                   message = "tns:TestFaultMessage",
                                   documentation = "",
                                   ns = c(tns = "http://example.com/namespace")) {
  # Create XML with namespaces
  xml_str <- sprintf('<fault xmlns:tns="%s" name="%s" message="%s"></fault>',
                     ns["tns"], name, message)
  doc <- xml2::read_xml(xml_str)
  rv <- xml2::xml_root(doc)

  if (documentation != "" && !is.null(documentation) && !is.na(documentation)) {
    xml2::xml_add_child(rv, "documentation", documentation)
  }

  return(rv)
}


# dummy port type -----------------------------------------------------------------------------

create_mock_wsdl <- function(port_types = list(
  list(name = "TestPortType",
       operations = list(
         list(name = "Operation1", input_message = "tns:Request", output_message = "tns:Response"),
         list(name = "Operation2", input_message = "tns:Request"),
         list(name = "Operation3", output_message = "tns:Response")
       )
  )
)) {
  # Start building the XML
  xml_str <- '<definitions xmlns:tns="http://example.com/namespace">'

  for (pt in port_types) {
    xml_str <- paste0(xml_str, sprintf('<portType name="%s">', pt$name))

    # Add documentation if exists
    if (!is.null(pt$documentation)) {
      xml_str <- paste0(xml_str, sprintf('<documentation>%s</documentation>', pt$documentation))
    }

    # Add operations
    for (op in pt$operations) {
      ops_str <- sprintf(
        '<operation name="%s">%s%s</operation>',
        op$name,
        ifelse(is.null(op$input_message), "", sprintf('<input message="%s"/>', op$input_message)),
        ifelse(is.null(op$output_message), "", sprintf('<output message="%s"/>', op$output_message))
      )
      xml_str <- paste0(xml_str, ops_str)
    }

    xml_str <- paste0(xml_str, '</portType>')
  }

  xml_str <- paste0(xml_str, '</definitions>')

  # Parse and return the XML node
  xml2::read_xml(xml_str)
}
