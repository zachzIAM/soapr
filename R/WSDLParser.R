#' WSDL Parser
#'
#' Parses a WSDL file to extract services, ports, operations, and types.
#'
#' @export
WSDLParser <- R6::R6Class(
  "WSDLParser",
  public = list(
    #' @field doc The parsed XML document.
    doc = NULL,
    #' @field ns The namespace definitions.
    ns = NULL,
    #' @field element_form_default The elementFormDefault value (qualified/unqualified).
    element_form_default = "unqualified",

    #' @description
    #' Initialize the WSDLParser.
    #' @param path_or_url Path or URL to the WSDL file.
    initialize = function(path_or_url) {
      self$doc <- xml2::read_xml(path_or_url)
      self$ns <- self$extend_namespace(xml2::xml_ns(self$doc))
      self$get_target_namespace()
    },

    #' @description
    #' Add standard namespaces if not present. We create a new vector combining document namespace
    #' and standard ones, prioritizing the document's namespace but ensure standard prefixes exist.
    #' @param ns namespace node of WSDL document (e.g. obtained with `xml2::xml_ns`)
    extend_namespace = function(ns) {
      std_ns <- c(
        xsd = "http://www.w3.org/2001/XMLSchema",
        wsdl = "http://schemas.xmlsoap.org/wsdl/",
        soap = "http://schemas.xmlsoap.org/wsdl/soap/",
        soap12 = "http://schemas.xmlsoap.org/wsdl/soap12/",
        http = "http://schemas.xmlsoap.org/wsdl/http/"
      )

      current_ns <- as.character(ns)

      # Merge
      combined <- c(current_ns, std_ns)

      # Remove duplicates by name
      combined <- combined[!duplicated(names(combined))]

      # Remove empty names
      combined <- combined[names(combined) != ""]

      return(combined)
    },

    #' @description
    #' Get the target namespace of the WSDL.
    #' @return The target namespace URI.
    get_target_namespace = function() {
      tns <- xml2::xml_attr(self$doc, "targetNamespace")
      if (is.na(tns)) {
        stop("targetNamespace attribute missing from WSDL definitions")
      }
      return(tns)
    },

    #' @description
    #' Get a list of services defined in the WSDL.
    #' @return A list of services.
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

    #' @description
    #' Get a list of ports for a service.
    #' @param service_node The service xml_node.
    #' @return A list of ports.
    get_ports = function(service_node) {
      ports <- xml2::xml_find_all(service_node, "./wsdl:port", self$ns)
      lapply(ports, function(port) {
        list(
          name = xml2::xml_attr(port, "name"),
          binding = self$strip_ns(xml2::xml_attr(port, "binding")),
          location = self$get_port_location(port)
        )
      })
    },

    #' @description
    #' Get the location of a service port.
    #' @param port_node The port xml_node.
    #' @return A list of ports.
    get_port_location = function(port_node) {
      address_node <- xml2::xml_find_first(
        port_node,
        ".//soap:address",
        self$ns
      )
      if (is_xml_missing(address_node)) {
        address_node <- xml2::xml_find_first(
          port_node,
          ".//soap12:address",
          self$ns
        )
      }
      if (is_xml_missing(address_node)) {
        address_node <- xml2::xml_find_first(
          port_node,
          ".//http:address",
          self$ns
        )
      }
      location <- if (!is.na(address_node)) {
        xml2::xml_attr(address_node, "location")
      } else {
        NULL
      }

      # Fix for WSDLs that specify http but use port 443
      if (
        !is.null(location) &&
          grepl(":443", location) &&
          grepl("^http:", location)
      ) {
        location <- sub("^http:", "https:", location)
      }
      return(location)
    },

    #' @description
    #' Get a list of operations for a binding.
    #' @param binding_name The name of the binding.
    #' @return A list of operations.
    get_operations = function(binding_name) {
      # Find the binding
      xpath <- sprintf("//wsdl:binding[@name='%s']", binding_name)
      binding <- xml2::xml_find_first(self$doc, xpath, self$ns)

      if (is_xml_missing(binding)) {
        stop(sprintf("Binding '%s' not found in WSDL", binding_name))
      }

      port_type_name <- self$strip_ns(xml2::xml_attr(binding, "type"))

      # Find operations in binding (for soapAction)
      binding_ops <- xml2::xml_find_all(binding, "./wsdl:operation", self$ns)
      ops_meta <- list()
      for (op in binding_ops) {
        name <- xml2::xml_attr(op, "name")
        soap_op <- xml2::xml_find_first(op, "./soap:operation", self$ns)
        action <- if (!is.na(soap_op)) {
          xml2::xml_attr(soap_op, "soapAction")
        } else {
          ""
        }
        ops_meta[[name]] <- list(action = action)
      }

      # Find portType to get input/output details
      pt_xpath <- sprintf("//wsdl:portType[@name='%s']", port_type_name)
      port_type <- xml2::xml_find_first(self$doc, pt_xpath, self$ns)

      if (is_xml_missing(port_type)) {
        stop(sprintf("PortType '%s' not found in WSDL", port_type_name))
      }

      pt_ops <- xml2::xml_find_all(port_type, "./wsdl:operation", self$ns)
      ops <- lapply(pt_ops, function(op_node) {
        op_name <- xml2::xml_attr(op_node, "name")
        input <- xml2::xml_find_first(op_node, "./wsdl:input", self$ns)
        output <- xml2::xml_find_first(op_node, "./wsdl:output", self$ns)

        input_msg <- self$strip_ns(xml2::xml_attr(input, "message"))
        output_msg <- self$strip_ns(xml2::xml_attr(output, "message"))

        # Validate messages exist
        if (!is.na(input_msg)) {
          msg_xpath <- sprintf("//wsdl:message[@name='%s']", input_msg)
          msg_node <- xml2::xml_find_first(self$doc, msg_xpath, self$ns)
          if (is_xml_missing(msg_node)) {
            stop(sprintf("Message '%s' not found in WSDL", input_msg))
          }
        }
        if (!is.na(output_msg)) {
          msg_xpath <- sprintf("//wsdl:message[@name='%s']", output_msg)
          msg_node <- xml2::xml_find_first(self$doc, msg_xpath, self$ns)
          if (is_xml_missing(msg_node)) {
            stop(sprintf("Message '%s' not found in WSDL", output_msg))
          }
        }

        # Get documentation
        doc <- self$parse_documentation(op_node)

        list(
          name = op_name,
          action = ops_meta[[op_name]]$action,
          input_message = input_msg,
          output_message = output_msg,
          documentation = doc
        )
      })

      if (length(ops) == 0) {
        stop(sprintf("No operations found for PortType '%s'", port_type_name))
      }

      names(ops) <- vapply(ops, "[[", character(1), "name")

      return(ops)
    },

    #' @description
    #' Get all type definitions from the WSDL.
    #' @return A list of type definitions.
    get_type_definitions = function() {
      # Find schema node
      schema <- xml2::xml_find_first(self$doc, "//xsd:schema", self$ns)
      if (is_xml_missing(schema)) {
        return(list())
      }

      # Get elementFormDefault
      element_form_default <- xml2::xml_attr(schema, "elementFormDefault")
      if (is.na(element_form_default)) {
        element_form_default <- "unqualified" # Default per XSD spec
      }
      self$element_form_default <- element_form_default

      types <- list()

      # Parse complexTypes
      complex_types <- xml2::xml_find_all(schema, "./xsd:complexType", self$ns)
      for (ct in complex_types) {
        name <- xml2::xml_attr(ct, "name")
        if (!is.na(name)) {
          types[[name]] <- self$parse_complex_type(ct)
        }
      }

      # Parse simpleTypes (enumerations)
      simple_types <- xml2::xml_find_all(schema, "./xsd:simpleType", self$ns)
      for (st in simple_types) {
        name <- xml2::xml_attr(st, "name")
        if (!is.na(name)) {
          types[[name]] <- self$parse_simple_type(st)
        }
      }

      # Parse elements that define types directly
      elements <- xml2::xml_find_all(schema, "./xsd:element", self$ns)
      for (el in elements) {
        name <- xml2::xml_attr(el, "name")
        ct <- xml2::xml_find_first(el, "./xsd:complexType", self$ns)
        if (!is_xml_missing(ct)) {
          types[[name]] <- self$parse_complex_type(ct)
        }
      }

      return(types)
    },

    #' @description
    #' Parse a complexType node.
    #' @param node The complexType xml_node.
    #' @return A list representing the complex type.
    parse_complex_type = function(node) {
      # Get attributes
      attrs <- xml2::xml_find_all(node, ".//xsd:attribute", self$ns)
      attr_defs <- list()
      for (a in attrs) {
        name <- xml2::xml_attr(a, "name")
        if (!is.na(name)) {
          attr_defs[[name]] <- list(
            name = name,
            type = self$strip_ns(xml2::xml_attr(a, "type")),
            use = xml2::xml_attr(a, "use"),
            documentation = self$parse_documentation(a)
          )
        }
      }

      # Get sequence elements
      seq_elements <- xml2::xml_find_all(
        node,
        ".//xsd:sequence/xsd:element",
        self$ns
      )
      elem_defs <- list()
      for (e in seq_elements) {
        name <- xml2::xml_attr(e, "name")
        type <- self$strip_ns(xml2::xml_attr(e, "type"))

        # Handle ref
        if (is.na(name)) {
          ref <- self$strip_ns(xml2::xml_attr(e, "ref"))
          if (!is.na(ref)) {
            name <- ref
            # Resolve ref: look up the global element definition
            global_elem_xpath <- sprintf("//xsd:element[@name='%s']", ref)
            global_elem <- xml2::xml_find_first(
              self$doc,
              global_elem_xpath,
              self$ns
            )

            if (!is_xml_missing(global_elem)) {
              # Check for type attribute on global element
              ref_type <- self$strip_ns(xml2::xml_attr(global_elem, "type"))
              if (!is.na(ref_type)) {
                type <- ref_type
              } else {
                # If no type attribute, check if it has an inline complexType
                # In that case, the type name effectively becomes the element name
                # (which our get_type_definitions() handles by creating a type with that name)
                if (
                  !is_xml_missing(xml2::xml_find_first(
                    global_elem,
                    "./xsd:complexType",
                    self$ns
                  ))
                ) {
                  type <- ref
                }
              }
            }

            if (is.na(type)) {
              # Fallback: assume the type name matches the ref name if we couldn't resolve it clearly
              # or if it's a simple type defined inline (which we don't fully support parsing inline yet,
              # but often the ref name is used as the type name in generated code anyway).
              type <- ref
            }
          }
        }

        if (!is.na(name)) {
          elem_defs[[name]] <- list(
            name = name,
            type = type,
            minOccurs = xml2::xml_attr(e, "minOccurs"),
            maxOccurs = xml2::xml_attr(e, "maxOccurs"),
            documentation = self$parse_documentation(e)
          )
        }
      }

      doc <- self$parse_documentation(node)

      list(
        kind = "complex",
        attributes = attr_defs,
        elements = elem_defs,
        documentation = doc
      )
    },

    #' @description
    #' Parse a simpleType node.
    #' @param node The simpleType xml_node.
    #' @return A list representing the simple type.
    parse_simple_type = function(node) {
      restriction <- xml2::xml_find_first(node, "./xsd:restriction", self$ns)
      base <- self$strip_ns(xml2::xml_attr(restriction, "base"))

      enums <- xml2::xml_find_all(restriction, "./xsd:enumeration", self$ns)
      values <- xml2::xml_attr(enums, "value")

      list(
        kind = "simple",
        base = base,
        values = values
      )
    },

    #' @description
    #' Strip namespace prefix from a string.
    #' @param str The string to strip.
    #' @return The string without the namespace prefix.
    strip_ns = function(str) {
      if (is.null(str)) {
        return(NULL)
      }
      if (is.na(str)) {
        return(NA)
      }

      parts <- strsplit(str, ":")[[1]]
      if (length(parts) > 1) parts[2] else parts[1]
    },

    #' @description
    #' Parse documentation from a node.
    #' @param node The xml_node.
    #' @return The documentation text, or NULL.
    parse_documentation = function(node) {
      doc_node <- xml2::xml_find_first(node, "./wsdl:documentation", self$ns)
      if (is_xml_missing(doc_node)) {
        # Try xsd:annotation/xsd:documentation for types
        doc_node <- xml2::xml_find_first(
          node,
          ".//xsd:annotation/xsd:documentation",
          self$ns
        )
      }

      if (!is_xml_missing(doc_node)) {
        return(trimws(xml2::xml_text(doc_node)))
      }
      return(NULL)
    }
  )
)
