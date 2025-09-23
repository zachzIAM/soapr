wsdl_port_operation <- S7::new_class(
  name = "wsdl_port_operation",
  properties = list(
    name = strict_string_prop("name", read_only = TRUE),
    input = nullable_string_prop("input", read_only = TRUE),
    output = nullable_string_prop("output", read_only = TRUE),
    fault_messages = S7::new_property(
      class = S7::class_list,
      validator = function(value) {
        if (length(value) == 0 || is.null(value)) return(NULL)
        bad <- !purrr::map_lgl(value, ~S7::S7_inherits(.x, class = wsdl_port_fault))
        if (any(bad)) {
          return("all elements of @fault_messages must be <wsdl_port_fault> objects")
        }
      }
    ),
    documentation = nullable_string_prop("documentation", read_only = TRUE),
    wsa_action = nullable_string_prop("wsa_action", read_only = TRUE)
  ),
  constructor = function(node) {
    assert_xml_wsdl_operation(node)

    nm <- xml2::xml_attr(node, "name")
    input <- parse_operation_input(node)
    output <- parse_operation_output(node)
    fault <- parse_operation_faults(node)
    docs <- parse_documentation(node)

    rv <- S7::new_object(
      S7::S7_object(),
      name = nm,
      input = input,
      output = output,
      fault_messages = fault,
      documentation = docs,
      wsa_action = NULL
    )

    return(rv)

  }
)


parse_operation_input <- function(node) {
  # TODO internal function only works on operation node, so no checks; should it use dispatch?
  op_content <- xml2::xml_contents(node)
  op_content_names <- xml2::xml_name(op_content)

  idx <- match("input", op_content_names)

  if (is.na(idx)) {
    return(NULL)
  }

  rv <- xml2::xml_attr(op_content[[idx]], "message")  # TODO should we resolve to a message object here?

  return(rv)
}


parse_operation_output <- function(node) {
  # TODO internal function only works on operation node, so no checks; should it use dispatch?
  op_content <- xml2::xml_contents(node)
  op_content_names <- xml2::xml_name(op_content)

  idx <- match("output", op_content_names)

  if (is.na(idx)) {
    return(NULL)
  }

  rv <- xml2::xml_attr(op_content[[idx]], "message")  # TODO should we resolve to a message object here?

  return(rv)
}


parse_operations <- function(node) {
  checkmate::assert_class(node, "xml_node", null.ok = FALSE)   # TODO should this be achieved with dispatch?
  checkmate::assert_true(xml2::xml_name(node) == "portType", na.ok = FALSE)

  content <- xml2::xml_contents(node)
  content_names <- xml2::xml_name(content)

  operation_nodes <- content[content_names == "operation"]

  rv <- purrr::map(operation_nodes, ~wsdl_port_operation(.x))
  names(rv) <- purrr::map_chr(rv, ~.x@name)

  return(rv)
}

# node check and assertions -------------------------------------------------------------------
check_xml_wsdl_operation <- function(x, null.ok = FALSE) {

  if (is.null(x) && null.ok)
    return(TRUE)

  type_chk <- check_xml_wsdl_node_type(x, "operation")
  if (!isTRUE(type_chk))
    return(type_chk)

  nm_chk <- xml2::xml_has_attr(x, "name", ns = xml2::xml_ns(x))
  if (!isTRUE(nm_chk)) {
    msg <- "WSDL Operation node must have a 'name' attribute."
    return(msg)
  }

  input_node <- xml2::xml_find_first(x, ".//input")
  output_node <- xml2::xml_find_first(x, ".//output")
  input_chk <- xml2::xml_has_attr(input_node, "message", ns = xml2::xml_ns(x))
  output_chk <- xml2::xml_has_attr(output_node, "message", ns = xml2::xml_ns(x))
  if (!isTRUE(input_chk) && !isTRUE(output_chk)) {
    msg <- "WSDL Operation cannot have both input and output missing"
    return(msg)
  }

  return(TRUE)
}


test_xml_wsdl_operation <- function(x, null.ok = FALSE) {
  return(isTRUE(check_xml_wsdl_operation(x = x, null.ok = null.ok)))
}


assert_xml_wsdl_operation <- function(x, null.ok = FALSE, .var.name = checkmate::vname(x), add = NULL) {
  if (missing(x))
    stop(sprintf("argument \"%s\" is missing, with no default", .var.name))
  res <- check_xml_wsdl_operation(x = x, null.ok = null.ok)
  checkmate::makeAssertion(x, res, .var.name, collection = add)
}
