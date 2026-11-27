#' Service Class
#'
#' Represents a SOAP service.
#'
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
    #' @field service_name The name of the selected service.
    service_name = NULL,
    #' @field port_name The name of the selected port.
    port_name = NULL,

    #' @description
    #' Initialize the Service.
    #' @param wsdl_path Path or URL to the WSDL file.
    #' @param service_name Name of the service to use (optional).
    #' @param port_name Name of the port to use (optional).
    #' @param verbose Logical, whether to print debug info.
    initialize = function(wsdl_path,
                          service_name = NULL,
                          port_name = NULL,
                          verbose = FALSE) {
      self$wsdl <- WSDLParser$new(wsdl_path)

      services <- self$wsdl$get_services()
      if (length(services) == 0) {
        stop("No services found in WSDL")
      }

      # Select Service
      if (is.null(service_name)) {
        service_def <- services[[1]]
      } else {
        service_def <- purrr::detect(services, ~ identical(.x$name, service_name))
        if (is.null(service_def)) {
          stop(sprintf("Service '%s' not found", service_name))
        }
      }
      self$service_name <- service_def$name

      if (length(service_def$ports) == 0) {
        stop("No ports found in service")
      }

      # Select Port
      if (is.null(port_name)) {
        port_def <- service_def$ports[[1]]
      } else {
        port_def <- purrr::detect(service_def$ports, ~ identical(.x$name, port_name))
        if (is.null(port_def)) {
          stop(sprintf(
            "Port '%s' not found in service '%s'",
            port_name,
            self$service_name
          ))
        }
      }
      self$port_name <- port_def$name

      self$factory <- Factory$new(self$wsdl)
      self$client <- Client$new(
        self$wsdl,
        port_def,
        self$factory,
        verbose = verbose
      )
    },

    #' @description
    #' List all available services.
    #' @return A character vector of service names.
    list_services = function() {
      unname(purrr::map_chr(self$wsdl$get_services(), "name"))
    },

    #' @description
    #' List all available ports for the current service.
    #' @return A character vector of port names.
    list_ports = function() {
      svc <- purrr::detect(
        self$wsdl$get_services(),
        ~ identical(.x$name, self$service_name)
      )
      if (is.null(svc)) {
        return(character(0))
      }
      unname(purrr::map_chr(svc$ports, "name"))
    }
  )
)
