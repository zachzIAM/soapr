# url <- "http://webservices.oorsprong.org/websamples.countryinfo/CountryInfoService.wso?WSDL"
# url <- "./tests/testthat/testdata/BDTInteractive.xml"

# parse a WSDL file from a URL or a path. We will need to handle each of the components
#   - types
#   - binding
#   - service
#   - message
#   - portType


WSDL <- R6::R6Class(
  classname = "WSDL",
  public = list(
    #' @description Create a new `WSDL` object from a url or path
    #' @param url Character string pointing to a WSDL document
    initialize = function(url) {
      checkmate::assert_string(url, na.ok = FALSE, null.ok = FALSE)

      wsdl_doc <- parse_wsdl(url)

      private$url <- url
      private$doc <- wsdl_doc$doc
      private$types <- wsdl_doc$types
      private$binding <- wsdl_doc$binding
      private$service <- wsdl_doc$service
      private$message <- wsdl_doc$message
      private$port_type <- wsdl_doc$port_type

      self$service <- make_service(wsdl_doc)
    }
  ),
  private = list(
    url = NULL,
    doc = NULL,
    types = NULL,
    binding = NULL,
    service = NULL,
    message = NULL,
    port_type = NULL
  ),
  active = list(
    url = function() private$url,
    doc = function() private$doc,
    types = function() private$types,
    bindings = function() private$binding,
    services = function() private$service,
    messages = function() private$message,
    port_types = function() private$port_type
  )
)

parse_wsdl <- function(url) {
  doc <- xml2::read_xml(url)
  root <- xml2::xml_root(doc)
  stopifnot(xml2::xml_name(root) == "definitions")
  if (!xml2::xml_has_attr(root, "targetNamespace", ns = xml2::xml_ns(root))) {
    stop("definitions node must have 'targetNamespace' attribute")
  }

  types <- parse_types(doc)
  binding <- parse_bindings(doc)
  service <- parse_services(doc)
  message <- parse_messages(doc)
  port_type <- parse_port_types(doc)

  rv <- list(
    doc = doc,
    types = types,
    binding = binding,
    service = service,
    message = message,
    port_type = port_type
  )

  return(rv)
}

#' @keywords internal
parse_types <- function(x) {
  checkmate::assert_class(x, c("xml_document", "xml_node"))
  ns <- xml2::xml_ns(x)
  types <- xml2::xml_find_all(x, ".//wsdl:types", ns = ns)
  if (length(types) == 0) {
    return(list())
  }
  xml2::xml_children(types[[1]])
}

#' @keywords internal
parse_bindings <- function(x) {
  checkmate::assert_class(x, c("xml_document", "xml_node"))
  ns <- xml2::xml_ns(x)
  binding_nodes <- xml2::xml_find_all(x, ".//wsdl:binding", ns = ns)
  stopifnot(length(binding_nodes) > 0)

  bindings <- purrr::map(binding_nodes, function(node) {
    ops <- xml2::xml_find_all(node, "wsdl:operation", ns = ns)
    list(
      name = xml2::xml_attr(node, "name"),
      type = xml2::xml_attr(node, "type"),
      operations = xml2::xml_attr(ops, "name")
    )
  })
  names(bindings) <- purrr::map_chr(bindings, "name")
  bindings
}

#' @keywords internal
parse_services <- function(x) {
  checkmate::assert_class(x, c("xml_document", "xml_node"))
  ns <- xml2::xml_ns(x)
  service_nodes <- xml2::xml_find_all(x, ".//wsdl:service", ns = ns)
  stopifnot(length(service_nodes) > 0)

  services <- purrr::map(service_nodes, function(node) {
    port_nodes <- xml2::xml_find_all(node, "wsdl:port", ns = ns)
    ports <- purrr::map(port_nodes, function(p) {
      addr <- xml2::xml_find_first(p, ".//*[@location]", ns = xml2::xml_ns(p))
      list(
        name = xml2::xml_attr(p, "name"),
        binding = xml2::xml_attr(p, "binding"),
        location = if (!is.na(addr)) xml2::xml_attr(addr, "location") else NA_character_
      )
    })
    names(ports) <- purrr::map_chr(ports, "name")
    list(name = xml2::xml_attr(node, "name"), ports = ports)
  })
  names(services) <- purrr::map_chr(services, "name")
  services
}

#' @keywords internal
parse_messages <- function(x) {
  checkmate::assert_class(x, c("xml_document", "xml_node"))
  ns <- xml2::xml_ns(x)
  message_nodes <- xml2::xml_find_all(x, ".//wsdl:message", ns = ns)
  stopifnot(length(message_nodes) > 0)

  messages <- purrr::map(message_nodes, function(node) {
    part_nodes <- xml2::xml_find_all(node, "wsdl:part", ns = ns)
    parts <- purrr::map(part_nodes, function(p) {
      list(
        name = xml2::xml_attr(p, "name"),
        element = xml2::xml_attr(p, "element"),
        type = xml2::xml_attr(p, "type")
      )
    })
    names(parts) <- purrr::map_chr(parts, "name")
    list(name = xml2::xml_attr(node, "name"), parts = parts)
  })
  names(messages) <- purrr::map_chr(messages, "name")
  messages
}

make_service <- function(wsdl_doc) {
  # For now simply return the parsed service definition list.
  wsdl_doc$service
}
