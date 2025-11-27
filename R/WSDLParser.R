#' WSDL Parser
#'
#' Parses a WSDL file to extract services, ports, operations, and types.
#'
#' @importFrom xml2 read_xml xml_ns xml_find_all xml_attr xml_name xml_children xml_text
#' @importFrom R6 R6Class
#' @export
WSDLParser <- R6::R6Class("WSDLParser",
  public = list(
    doc = NULL,
    ns = NULL,
    initialize = function(path_or_url) {
      self$doc <- xml2::read_xml(path_or_url)
      self$ns <- xml2::xml_ns(self$doc)
    },
    get_services = function() {
      # Find all service elements
      services <- xml2::xml_find_all(self$doc, "//wsdl:service", self$ns)
      lapply(services, function(svc) {
        list(
          name = xml2::xml_attr(svc, "name"),
          ports = self$get_ports(svc)
        )
      })
    },
    get_ports = function(service_node) {
      ports <- xml2::xml_find_all(service_node, "./wsdl:port", self$ns)
      lapply(ports, function(port) {
        binding_name <- self$strip_ns(xml2::xml_attr(port, "binding"))
        address_node <- xml2::xml_find_first(port, ".//soap:address", self$ns)
        location <- if (!is.na(address_node)) xml2::xml_attr(address_node, "location") else NULL

        # Fix for WSDLs that specify http but use port 443
        if (!is.null(location) && grepl(":443", location) && grepl("^http:", location)) {
          location <- sub("^http:", "https:", location)
        }

        list(
          name = xml2::xml_attr(port, "name"),
          binding = binding_name,
          location = location
        )
      })
    },
    get_operations = function(binding_name) {
      # Find the binding
      xpath <- sprintf("//wsdl:binding[@name='%s']", binding_name)
      binding <- xml2::xml_find_first(self$doc, xpath, self$ns)

      if (inherits(binding, "xml_missing")) {
        return(list())
      }

      port_type_name <- self$strip_ns(xml2::xml_attr(binding, "type"))

      # Find operations in binding (for soapAction)
      binding_ops <- xml2::xml_find_all(binding, "./wsdl:operation", self$ns)
      ops_meta <- list()
      for (op in binding_ops) {
        name <- xml2::xml_attr(op, "name")
        soap_op <- xml2::xml_find_first(op, "./soap:operation", self$ns)
        action <- if (!is.na(soap_op)) xml2::xml_attr(soap_op, "soapAction") else ""
        ops_meta[[name]] <- list(action = action)
      }

      # Find portType to get input/output details
      pt_xpath <- sprintf("//wsdl:portType[@name='%s']", port_type_name)
      port_type <- xml2::xml_find_first(self$doc, pt_xpath, self$ns)

      pt_ops <- xml2::xml_find_all(port_type, "./wsdl:operation", self$ns)
      lapply(pt_ops, function(op) {
        name <- xml2::xml_attr(op, "name")
        input <- xml2::xml_find_first(op, "./wsdl:input", self$ns)
        output <- xml2::xml_find_first(op, "./wsdl:output", self$ns)

        input_msg <- self$strip_ns(xml2::xml_attr(input, "message"))
        output_msg <- self$strip_ns(xml2::xml_attr(output, "message"))

        list(
          name = name,
          action = ops_meta[[name]]$action,
          input_message = input_msg,
          output_message = output_msg
        )
      })
    },
    strip_ns = function(str) {
      if (is.null(str)) {
        return(NULL)
      }
      parts <- strsplit(str, ":")[[1]]
      if (length(parts) > 1) parts[2] else parts[1]
    }
  )
)
