#' Service Class
#'
#' Represents a SOAP service.
#'
#' @importFrom R6 R6Class
#' @export
Service <- R6::R6Class(
  "Service",
  public = list(
    #' @field wsdl The WSDLParser instance.
    wsdl = NULL,
    #' @field client The Client instance.
    client = NULL,
    #' @field factory The Factory instance.
    factory = NULL,

    #' @description
    #' Initialize the Service.
    #' @param wsdl_path Path or URL to the WSDL file.
    #' @param verbose Logical, whether to print debug info.
    initialize = function(wsdl_path, verbose = FALSE) {
      self$wsdl <- WSDLParser$new(wsdl_path)

      # For simplicity, pick the first service and first port
      services <- self$wsdl$get_services()
      if (length(services) == 0) {
        stop("No services found in WSDL")
      }

      service_def <- services[[1]]
      if (length(service_def$ports) == 0) {
        stop("No ports found in service")
      }

      port_def <- service_def$ports[[1]]

      self$factory <- Factory$new(self$wsdl)
      self$client <- Client$new(
        self$wsdl,
        port_def,
        self$factory,
        verbose = verbose
      )
    }
  )
)
