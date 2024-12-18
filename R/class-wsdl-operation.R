setClass(
  Class = "wsdl_operation",
  slots = list(
    name = "character",
    input_message = "character",
    output_message = "character",
    fault_message = "list",
    documentation = "character",
    wsa_action = "character" # full URL to the Action if available
  )
)

parse_operation <- function(node) {
  checkmate::assert_class(node, "xml_node")   # TODO should this be achieved with dispatch?
  stopifnot(xml2::xml_name(node) == "operation")

  nm <- xml2::xml_attr(node, "name")
  input <- parse_operation_input(node)
  output <- parse_operation_output(node)
  fault <- parse_operation_faults(node)
  documentation <- parse_documentation(node)

  rv <- new("wsdl_operation",
    name = nm,
    input_message = input,
    output_message = output,
    fault_message = fault,
    documentation = documentation
  )

  return(rv)
}

parse_operation_input <- function(node) {
  # TODO internal function only works on operation node, so no checks; should it use dispatch?
  op_content <- xml2::xml_contents(node)
  op_content_names <- xml2::xml_name(op_content)

  idx <- match("input", op_content_names)

  if (is.na(idx)) {
    return(character())
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
    return(character())
  }

  rv <- xml2::xml_attr(op_content[[idx]], "message")  # TODO should we resolve to a message object here?

  return(rv)
}

parse_operation_faults <- function(node) {
  # TODO internal function only works on operation node, so no checks; should it use dispatch?
  op_content <- xml2::xml_contents(node)
  op_content_names <- xml2::xml_name(op_content)

  faults <- op_content[op_content_names == "fault"]

  if (length(faults) == 0) {
    return(list())
  }

  rv <- purrr::map(faults, ~wsdl_fault(.x))
  names(rv) <- purrr::map_chr(rv, ~.x@name)

  return(rv)
}

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

  input_chk <- xml2::xml_has_attr(x, "input", ns = xml2::xml_ns(x))
  output_chk <- xml2::xml_has_attr(x, "output", ns = xml2::xml_ns(x))
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
