check_xml_wsdl_node_type <- function(x, type, null.ok = FALSE) {
  checkmate::qassert(type, "s1")
  if (is.null(x) && null.ok)
    return(TRUE)

  class_chk <- checkmate::check_class(x, "xml_node")
  if (!isTRUE(class_chk))
    return(class_chk)

  node_name <- xml2::xml_name(x)
  name_chk <- node_name == type
  if (!name_chk) {
    msg <- sprintf("Expected node type '%s', but got '%s'.", type, node_name)
    return(msg)
  }

  return(TRUE)
}

test_xml_wsdl_node_type <- function(x, type, null.ok = FALSE) {
  return(isTRUE(check_xml_wsdl_node_type(x = x, type = type, null.ok = null.ok)))
}

assert_xml_wsdl_node_type <- function(x, type, null.ok = FALSE, .var.name = checkmate::vname(x), add = NULL) {
  if (missing(x))
    stop(sprintf("argument \"%s\" is missing, with no default", .var.name))
  res <- check_xml_wsdl_node_type(x = x, type = type, null.ok = null.ok)
  checkmate::makeAssertion(x, res, .var.name, collection = add)
}
