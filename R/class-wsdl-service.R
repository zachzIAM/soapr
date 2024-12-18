setClass(
  "wsdl_service",
  slots = list(ports = "list", names = "character", is_resolved = "logical")
)


setMethod("show", "wsdl_service", definition = function(object) {
  repr <- sprintf("<wsdl_service(name=%s, ports=%s)>", object@name, object@ports)
  cat(repr)
})

wsdl_service <- function(name) {
  checkmate::assert_string(name, na.ok = FALSE, null.ok = FALSE, min.chars = 1)

  stop("Not implemented")
}
