setClass(
  Class = "wsdl_fault",
  representation(name = "character", message = "character", documentation = "character"),
  prototype(name = NA_character_, message = NA_character_, documentation = NA_character_)
)

wsdl_fault <- function(node) {
  nm <- xml2::xml_attr(node, "name")
  msg <- xml2::xml_attr(node, "message") # TODO should we resolve to message into an object here?
  docs <- parse_documentation(node)

  rv <- new("wsdl_fault", name = nm, message = msg, documentation = docs)

  return(rv)
}

