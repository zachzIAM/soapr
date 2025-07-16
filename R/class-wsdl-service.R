setClass(
  "wsdl_service",
  slots = list(name = "character", ports = "list", is_resolved = "logical"),
  prototype = list(name = character(), ports = list(), is_resolved = FALSE)
)


setMethod("show", "wsdl_service", definition = function(object) {
  repr <- sprintf("<wsdl_service(name=%s, ports=%s)>", object@name, object@ports)
  cat(repr)
})

wsdl_service <- function(name, ports = list(), is_resolved = FALSE) {
  checkmate::assert_string(name, na.ok = FALSE, null.ok = FALSE, min.chars = 1)
  checkmate::assert_list(ports)
  checkmate::assert_flag(is_resolved)

  new("wsdl_service", name = name, ports = ports, is_resolved = is_resolved)
}
