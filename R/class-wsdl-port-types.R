wsdl_port_type <- S7::new_class(
  name = "wsdl_port_type",
  properties = list(
    name = strict_string_prop("name", read_only = TRUE),
    operations = S7::new_property(
      class = S7::class_list,
      validator = function(value) {
        if (length(value) == 0 || is.null(value)) return(NULL)
        bad <- !purrr::map_lgl(value, ~S7::S7_inherits(.x, class = wsdl_port_operation))
        if (any(bad)) {
          return("all elements of @operations must be <wsdl_port_operation> objects")
        }
      }
    ),
    documentation = nullable_string_prop("documentation", read_only = TRUE)
  ),
  constructor = function(node) {
    assert_xml_wsdl_port_type(node)

    nm <- xml2::xml_attr(node, "name")
    docs <- parse_documentation(node)
    ops <- parse_operations(node)

    rv <- S7::new_object(
      S7::S7_object(),
      name = nm,
      operations = ops,
      documentation = docs
    )

    return(rv)
  }
)


parse_port_types <- function(node) {
  checkmate::assert_class(node, c("xml_document", "xml_node"), null.ok = FALSE)   # TODO should this be achieved with dispatch?
  checkmate::assert_true(xml2::xml_name(node) == "definitions", na.ok = FALSE)

  content <- xml2::xml_contents(node)
  content_names <- xml2::xml_name(content)

  port_type_count <- sum(content_names == "portType")
  stopifnot(port_type_count > 0)

  port_type_nodes <- content[content_names == "portType"]

  port_types <- purrr::map(port_type_nodes, ~wsdl_port_type(.x))
  names(port_types) <- purrr::map_chr(port_types, ~.x@name)

  return(port_types)
}


# node checks and assertions ------------------------------------------------------------------

check_xml_wsdl_port_type <- function(x, null.ok = FALSE) {

  if (is.null(x) && null.ok)
    return(TRUE)

  type_chk <- check_xml_wsdl_node_type(x, "portType")
  if (!isTRUE(type_chk))
    return(type_chk)

  nm_chk <- xml2::xml_has_attr(x, "name", ns = xml2::xml_ns(x))
  if (!isTRUE(nm_chk)) {
    msg <- "WSDL portType node must have a 'name' attribute."
    return(msg)
  }

  return(TRUE)
}


test_xml_wsdl_port_type <- function(x, null.ok = FALSE) {
  return(isTRUE(check_xml_wsdl_port_type(x = x, null.ok = null.ok)))
}


assert_xml_wsdl_port_type <- function(x, null.ok = FALSE, .var.name = checkmate::vname(x), add = NULL) {
  if (missing(x))
    stop(sprintf("argument \"%s\" is missing, with no default", .var.name))
  res <- check_xml_wsdl_port_type(x = x, null.ok = null.ok)
  checkmate::makeAssertion(x, res, .var.name, collection = add)
}
