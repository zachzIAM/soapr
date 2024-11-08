wsdl_operation <- R6::R6Class(
  classname = "wsdl_operation",
  public = list(
    initialize = function(
      name,
      input_message = NULL,
      output_message = NULL,
      fault_message = NULL,
      parameter_order = NULL,
      wsa_action = NULL,
      documentation = NULL
    ) {
      self$name = name
      self$input_message = input_message
      self$output_message = output_message
      self$fault_message = fault_message
      self$parameter_order = parameter_order  # for actions where order matters, e.g. output first, then input
      self$wsa_action = wsa_action  # full URL to the Action if available
      self$documentation = documentation
    },
    name = NULL,
    input_message = NULL,
    output_message = NULL,
    fault_message = NULL,
    parameter_order = NULL,
    wsa_action = NULL,
    documentation = NULL
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

  rv <- wsdl_operation$new(
    name = nm,
    input_message = input,
    output_message = output,
    fault_message = fault,
    documentation = documentation
  )
}

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

parse_operation_faults <- function(node) {
  # TODO internal function only works on operation node, so no checks; should it use dispatch?
  op_content <- xml2::xml_contents(node)
  op_content_names <- xml2::xml_name(op_content)

  faults <- op_content[op_content_names == "fault"]

  if (length(faults) == 0) {
    return(NULL)
  }

  rv <- purrr::map(faults, ~wsdl_element_factory(.x))
  names(rv) <- purrr::map_chr(rv, ~.x@name)

  return(rv)
}
