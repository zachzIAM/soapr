

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
