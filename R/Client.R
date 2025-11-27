#' Client Class
#'
#' Handles SOAP requests.
#'
#' @importFrom R6 R6Class
#' @importFrom httr POST add_headers content_type content
#' @importFrom xml2 xml_new_root xml_add_child xml_set_attr xml_set_text as_xml_document
#' @export
Client <- R6::R6Class("Client",
  lock_objects = FALSE,
  public = list(
    wsdl = NULL,
    port_def = NULL,
    factory = NULL,
    operations = NULL,
    initialize = function(wsdl, port_def, factory) {
      self$wsdl <- wsdl
      self$port_def <- port_def
      self$factory <- factory
      self$operations <- self$wsdl$get_operations(port_def$binding)

      # Dynamically add methods for each operation
      for (op in self$operations) {
        self$add_operation_method(op)
      }
    },
    add_operation_method = function(op) {
      self[[op$name]] <- function(...) {
        args <- list(...)
        self$call_operation(op, args)
      }
    },
    call_operation = function(op, args) {
      # Construct SOAP Envelope
      envelope <- xml2::xml_new_root("soap:Envelope",
        "xmlns:soap" = "http://schemas.xmlsoap.org/soap/envelope/",
        "xmlns:tns" = "http://barra.com/cp/bdti"
      ) # TODO: Get TNS dynamically

      body <- xml2::xml_add_child(envelope, "soap:Body")
      op_node <- xml2::xml_add_child(body, paste0("tns:", op$name))

      # Add arguments to operation node
      # This is a simplification. Real SOAP serialization is complex.
      # We assume args are named list matching the expected elements.
      for (arg_name in names(args)) {
        child <- xml2::xml_add_child(op_node, paste0("tns:", arg_name))
        xml2::xml_set_text(child, as.character(args[[arg_name]]))
      }

      # Make Request
      response <- httr::POST(
        url = self$port_def$location,
        httr::add_headers(SOAPAction = op$action),
        httr::content_type("text/xml; charset=utf-8"),
        body = as.character(envelope)
      )

      httr::stop_for_status(response)

      # Parse Response
      content <- httr::content(response, as = "parsed", encoding = "UTF-8")
      # TODO: Extract actual result from Body
      return(content)
    }
  )
)
