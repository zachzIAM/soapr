wsdl_fault <- R6::R6Class(
  classname = "wsdl_fault",
  public = list(
    initialize = function(name, message, documentation = NULL) {
      self$name <- name
      self$message <- message
      self$documentation <- documentation
    },
    name = NULL,
    message = NULL,
    documentation = NULL
  )
)

parse_oparation_fault <- function(node) {
  # TODO internal function only works on a fault node, so no checks; should it use dispatch?
  nm <- xml2::xml_attr(node, "name")
  msg <- xml2::xml_attr(node, "message") # TODO should we resolve to message into an object here?
  docs <- parse_documentation(node)

  return(wsdl_fault$new(name = nm, message = msg, documentation = docs))
}
