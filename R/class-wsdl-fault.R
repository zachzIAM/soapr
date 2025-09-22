wsdl_fault <- S7::new_class(
  name = "wsdl_fault",
  properties = list(
    name = strict_string_prop("name", read_only = TRUE),
    message = nullable_string_prop("message", read_only = TRUE),
    documentation = nullable_string_prop("documentation", read_only = TRUE)
  ),
  constructor = function(node) {
    assert_xml_wsdl_fault(node)

    nm <- xml2::xml_attr(node, "name")
    msg <- xml2::xml_attr(node, "message") # TODO should we resolve to message into an object here?
    docs <- parse_documentation(node)

    rv <- S7::new_object(
      S7::S7_object(),
      name = nm,
      message = msg,
      documentation = docs
    )

    return(rv)
  }
)

# setClass(
#   Class = "wsdl_fault",
#   slots = list(name = "character", message = "character", documentation = "character")
# )
#
# wsdl_fault <- function(node) {
#   assert_xml_wsdl_fault(node)
#
#   nm <- xml2::xml_attr(node, "name")
#   msg <- xml2::xml_attr(node, "message") # TODO should we resolve to message into an object here?
#   docs <- parse_documentation(node)
#
#   rv <- new("wsdl_fault", name = nm, message = msg, documentation = docs)
#
#   return(rv)
# }

check_xml_wsdl_fault <- function(x, null.ok = FALSE) {

  if (is.null(x) && null.ok)
    return(TRUE)

  type_chk <- check_xml_wsdl_node_type(x, "fault")
  if (!isTRUE(type_chk))
    return(type_chk)

  nm_chk <- xml2::xml_has_attr(x, "name", ns = xml2::xml_ns(x))
  if (!isTRUE(nm_chk)) {
    msg <- "WSDL Fault node must have a 'name' attribute."
    return(msg)
  }

  msg_chk <- xml2::xml_has_attr(x, "message", ns = xml2::xml_ns(x))
  if (!isTRUE(msg_chk)) {
    msg <- "WSDL Fault node must have a 'message' attribute."
    return(msg)
  }

  return(TRUE)
}

test_xml_wsdl_fault <- function(x, null.ok = FALSE) {
  return(isTRUE(check_xml_wsdl_fault(x = x, null.ok = null.ok)))
}

assert_xml_wsdl_fault <- function(x, null.ok = FALSE, .var.name = checkmate::vname(x), add = NULL) {
  if (missing(x))
    stop(sprintf("argument \"%s\" is missing, with no default", .var.name))
  res <- check_xml_wsdl_fault(x = x, null.ok = null.ok)
  checkmate::makeAssertion(x, res, .var.name, collection = add)
}
