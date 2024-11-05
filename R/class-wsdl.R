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
  private = NULL,
  active = NULL,

)

parse_wsdl <- function(url) {
  doc <- xml2::read_xml(url)
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
  stop("Not implemented")
}

#' @keywords internal
parse_bindings <- function(x) {

}

#' @keywords internal
parse_services <- function(x) {

}

#' @keywords internal
parse_messages <- function(x) {

}

#' @keywords internal
parse_port_types <- function(x) {

}
