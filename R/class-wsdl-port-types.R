wsdl_port_type <- R6::R6Class(
  classname = "wsdl_port_type",
  public = list(
    initialize = function(name, operations, documentation = NULL) {
      self$name = name
      self$operations = operations
      self$documentation = documentation
    },
    name = NULL,
    operations = NULL,
    documentation = NULL
  )
)

parse_port_types <- function(node) {
  checkmate::assert_class(node, c("xml_document", "xml_node"), null.ok = FALSE)   # TODO should this be achieved with dispatch?
  checkmate::assert_true(xml2::xml_name(node) == "definitions", na.ok = FALSE)

  content <- xml2::xml_contents(node)
  content_names <- xml2::xml_name(content)

  port_type_count <- sum(content_names == "portType")
  stopifnot(port_type_count > 0)

  port_type_nodes <- content[content_names == "portType"]

  port_types <- purrr::map(port_type_nodes, ~parse_port_type(.x))
  names(port_types) <- purrr::map_chr(port_types, ~.x$name)

  return(port_types)

}

parse_port_type <- function(node) {
  checkmate::assert_class(node, "xml_node", null.ok = FALSE)   # TODO should this be achieved with dispatch?
  checkmate::assert_true(xml2::xml_name(node) == "portType", na.ok = FALSE)

  nm <- xml2::xml_attr(node, "name")
  docs <- parse_documentation(node)
  ops <- parse_operations(node)

  rv <- wsdl_port_type$new(
    name = nm,
    operations = ops,
    documentation = docs
  )

  return(rv)
}

parse_operations <- function(node) {
  checkmate::assert_class(node, "xml_node", null.ok = FALSE)   # TODO should this be achieved with dispatch?
  checkmate::assert_true(xml2::xml_name(node) == "portType", na.ok = FALSE)

  content <- xml2::xml_contents(node)
  content_names <- xml2::xml_name(content)

  operation_nodes <- content[content_names == "operation"]

  rv <- purrr::map(operation_nodes, ~parse_operation(.x))
  names(rv) <- purrr::map_chr(rv, ~.x$name)

  return(rv)
}
