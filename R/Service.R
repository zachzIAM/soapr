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
        self$service_name <- service_def$name
      } else {
        # Find service by name
        found <- FALSE
        for (svc in services) {
          if (svc$name == service_name) {
            service_def <- svc
            self$service_name <- svc$name
            found <- TRUE
            break
          }
        }
        if (!found) {
          stop(sprintf("Service '%s' not found", service_name))
        }
      }

      if (length(service_def$ports) == 0) {
        stop("No ports found in service")
      }

      # Select Port
      if (is.null(port_name)) {
        port_def <- service_def$ports[[1]]
        self$port_name <- port_def$name
      } else {
        # Find port by name
        found <- FALSE
        for (port in service_def$ports) {
          if (port$name == port_name) {
            port_def <- port
            self$port_name <- port$name
            found <- TRUE
            break
          }
        }
        if (!found) {
          stop(sprintf(
            "Port '%s' not found in service '%s'",
            port_name,
            self$service_name
          ))
        }
      }

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
      unname(sapply(self$wsdl$get_services(), function(x) x$name))
    },

    #' @description
    #' List all available ports for the current service.
    #' @return A character vector of port names.
    list_ports = function() {
      services <- self$wsdl$get_services()
      for (svc in services) {
        if (svc$name == self$service_name) {
          return(unname(sapply(svc$ports, function(x) x$name)))
        }
      }
      return(character(0))
    }
  )
)
