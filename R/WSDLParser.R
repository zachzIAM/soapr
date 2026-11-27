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
    #' @field source The original WSDL file path or URL.
    source = NULL,
    #' @field schemas Per-schema metadata discovered across inline, imported, and included schemas.
    schemas = list(),
    #' @field schema_loader Optional custom loader function(uri) for resolving external schemas.
    schema_loader = NULL,
    #' @field element_form_default The elementFormDefault value (qualified/unqualified).
    element_form_default = "unqualified",
    #' @field attribute_form_default The attributeFormDefault value (qualified/unqualified).
    attribute_form_default = "unqualified",

    #' @description
    #' Initialize the WSDLParser.
    #' @param path_or_url Path or URL to the WSDL file.
    #' @param schema_loader Optional function taking a URI/path and returning an xml_document or XML string.
    initialize = function(path_or_url, schema_loader = NULL) {
      self$source <- path_or_url
      self$schema_loader <- schema_loader
      self$doc <- xml2::read_xml(path_or_url)
      self$ns <- self$extend_namespace(xml2::xml_ns(self$doc))
      self$get_target_namespace()
      private$collect_schemas()
      self$parse_schema_defaults()
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
    #' Parse schema defaults (elementFormDefault, attributeFormDefault) from the WSDL schemas.
    parse_schema_defaults = function() {
      if (length(self$schemas) == 0L) {
        self$element_form_default <- "unqualified"
        self$attribute_form_default <- "unqualified"
        return(invisible(NULL))
      }

      wsdl_tns <- self$get_target_namespace()
      inline_schemas <- Filter(function(s) identical(s$kind, "inline"), self$schemas)
      primary <- NULL

      if (length(inline_schemas) > 0L) {
        tns_matches <- Filter(function(s) identical(s$target_namespace, wsdl_tns), inline_schemas)
        primary <- if (length(tns_matches) > 0L) tns_matches[[1]] else inline_schemas[[1]]
      } else {
        primary <- self$schemas[[1]]
      }

      self$element_form_default <- primary$element_form_default
      self$attribute_form_default <- primary$attribute_form_default
      invisible(NULL)
    },

    #' @description
    #' Get metadata for all discovered schemas (inline, imported, and included).
    #' @return A list of schema metadata records with `target_namespace`,
    #'   `element_form_default`, `attribute_form_default`, `source`, and `kind`.
    get_schemas = function() {
      lapply(self$schemas, function(s) {
        list(
          target_namespace = s$target_namespace,
          element_form_default = s$element_form_default,
          attribute_form_default = s$attribute_form_default,
          source = s$source,
          kind = s$kind
        )
      })
    },

    #' @description
    #' Get a list of services defined in the WSDL.
    #' @return A list of services.
    get_services = function() {
      # Find all service elements
      services <- xml2::xml_find_all(self$doc, "//wsdl:service", self$ns)
      res <- purrr::map(services, function(svc) {
        list(
          name = xml2::xml_attr(svc, "name"),
          ports = self$get_ports(svc)
        )
      })
      if (length(res) > 0) {
        res <- purrr::set_names(res, purrr::map_chr(res, "name"))
      }
      return(res)
    },

    #' @description
    #' Get a list of ports for a service.
    #' @param service_node The service xml_node.
    #' @return A list of ports.
    get_ports = function(service_node) {
      ports <- xml2::xml_find_all(service_node, "./wsdl:port", self$ns)
      res <- purrr::map(ports, function(port) {
        binding_name <- self$strip_ns(xml2::xml_attr(port, "binding"))
        list(
          name = xml2::xml_attr(port, "name"),
          binding = binding_name,
          location = self$get_port_location(port),
          operations = self$get_operations(binding_name)
        )
      })
      if (length(res) > 0) {
        res <- purrr::set_names(res, purrr::map_chr(res, "name"))
      }
      return(res)
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
      ops <- purrr::map(pt_ops, function(op_node) {
        op_name <- xml2::xml_attr(op_node, "name")
        input <- xml2::xml_find_first(op_node, "./wsdl:input", self$ns)
        output <- xml2::xml_find_first(op_node, "./wsdl:output", self$ns)

        input_msg <- self$strip_ns(xml2::xml_attr(input, "message"))
        output_msg <- self$strip_ns(xml2::xml_attr(output, "message"))

        input_element <- NULL
        if (!is.na(input_msg)) {
          msg_xpath <- sprintf("//wsdl:message[@name='%s']", input_msg)
          msg_node <- xml2::xml_find_first(self$doc, msg_xpath, self$ns)
          if (is_xml_missing(msg_node)) {
            stop(sprintf("Message '%s' not found in WSDL", input_msg))
          }
          # Try to find part 'parameters' (Doc/Lit Wrapped) or first part
          part <- xml2::xml_find_first(msg_node, "./wsdl:part[@name='parameters']", self$ns)
          if (is_xml_missing(part)) {
            part <- xml2::xml_find_first(msg_node, "./wsdl:part", self$ns)
          }

          if (!is_xml_missing(part)) {
            input_element <- self$strip_ns(xml2::xml_attr(part, "element"))
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
          input_element = input_element,
          output_message = output_msg,
          documentation = doc
        )
      })

      if (length(ops) == 0) {
        stop(sprintf("No operations found for PortType '%s'", port_type_name))
      }

      ops <- purrr::set_names(ops, purrr::map_chr(ops, "name"))

      return(ops)
    },

    #' @description
    #' Get all type definitions across all WSDL, imported, and included schemas.
    #' @return A list of type definitions.
    get_type_definitions = function() {
      if (length(self$schemas) == 0L) {
        return(list())
      }

      self$parse_schema_defaults()
      types <- list()
      nested_collector <- new.env(parent = emptyenv())
      nested_collector$types <- list()

      for (entry in self$schemas) {
        schema <- entry$node
        schema_ns <- entry$ns

        complex_types <- xml2::xml_find_all(schema, "./xsd:complexType", schema_ns)
        for (ct in complex_types) {
          name <- xml2::xml_attr(ct, "name")
          if (!is.na(name)) {
            types[[name]] <- self$parse_complex_type(
              ct,
              schema_ns = schema_ns,
              type_name = name,
              nested_collector = nested_collector
            )
          }
        }

        simple_types <- xml2::xml_find_all(schema, "./xsd:simpleType", schema_ns)
        for (st in simple_types) {
          name <- xml2::xml_attr(st, "name")
          if (!is.na(name)) {
            types[[name]] <- self$parse_simple_type(st, schema_ns = schema_ns)
          }
        }

        elements <- xml2::xml_find_all(schema, "./xsd:element", schema_ns)
        for (el in elements) {
          name <- xml2::xml_attr(el, "name")
          if (is.na(name)) {
            next
          }
          ct <- xml2::xml_find_first(el, "./xsd:complexType", schema_ns)
          if (!is_xml_missing(ct)) {
            types[[name]] <- self$parse_complex_type(
              ct,
              schema_ns = schema_ns,
              type_name = name,
              nested_collector = nested_collector
            )
          } else {
            st <- xml2::xml_find_first(el, "./xsd:simpleType", schema_ns)
            if (!is_xml_missing(st)) {
              types[[name]] <- self$parse_simple_type(st, schema_ns = schema_ns)
            } else {
              type_attr <- self$strip_ns(xml2::xml_attr(el, "type"))
              if (!is.na(type_attr)) {
                types[[name]] <- list(
                  kind = "element",
                  name = name,
                  type = type_attr,
                  documentation = self$parse_documentation(el, schema_ns = schema_ns)
                )
              }
            }
          }
        }
      }

      for (nested_name in names(nested_collector$types)) {
        if (is.null(types[[nested_name]])) {
          types[[nested_name]] <- nested_collector$types[[nested_name]]
        }
      }

      types
    },

    #' @description
    #' Parse a complexType node.
    #' @param node The complexType xml_node.
    #' @param schema_ns Namespace vector to use for XPath queries on this schema.
    #' @param type_name Optional enclosing type name for scoping inline anonymous subtypes.
    #' @param visited Visited base type names to prevent inheritance cycles.
    #' @param nested_collector Optional environment collecting inline anonymous subtypes.
    #' @return A list representing the complex type.
    parse_complex_type = function(
      node,
      schema_ns = self$ns,
      type_name = NULL,
      visited = character(0),
      nested_collector = NULL
    ) {
      effective_name <- if (
        !is.null(type_name) &&
          length(type_name) == 1L &&
          !is.na(type_name) &&
          nzchar(type_name)
      ) {
        type_name
      } else {
        xml2::xml_attr(node, "name")
      }
      if (!is.na(effective_name) && nzchar(effective_name)) {
        visited <- unique(c(visited, effective_name))
      }

      attr_defs <- list()
      elem_defs <- list()
      is_simple_content <- FALSE
      simple_base <- NULL

      # Check simpleContent (text value + attributes)
      sc_node <- xml2::xml_find_first(node, "./xsd:simpleContent/xsd:extension", schema_ns)
      if (is_xml_missing(sc_node)) {
        sc_node <- xml2::xml_find_first(node, "./xsd:simpleContent/xsd:restriction", schema_ns)
      }

      if (!is_xml_missing(sc_node)) {
        is_simple_content <- TRUE
        raw_base <- self$strip_ns(xml2::xml_attr(sc_node, "base"))
        simple_base <- if (!is.na(raw_base) && nzchar(raw_base)) raw_base else "string"

        if (!simple_base %in% visited) {
          base_res <- private$find_complex_type(simple_base)
          if (!is.null(base_res)) {
            base_def <- self$parse_complex_type(
              base_res$node,
              schema_ns = base_res$ns,
              type_name = simple_base,
              visited = c(visited, simple_base),
              nested_collector = nested_collector
            )
            attr_defs <- c(attr_defs, base_def$attributes)
            if (isTRUE(base_def$simple_content) && !is.null(base_def$base)) {
              simple_base <- base_def$base
            }
          }
        }

        own_attrs <- private$parse_attribute_list(sc_node, schema_ns)
        for (an in names(own_attrs)) {
          attr_defs[[an]] <- own_attrs[[an]]
        }

        doc <- self$parse_documentation(node, schema_ns = schema_ns)
        value_field <- if ("value" %in% names(attr_defs)) ".value" else "value"

        return(list(
          kind = "complex",
          attributes = attr_defs,
          elements = list(),
          simple_content = TRUE,
          base = simple_base,
          value_field = value_field,
          documentation = doc
        ))
      }

      # Check complexContent inheritance (extension / restriction)
      cc_node <- xml2::xml_find_first(node, "./xsd:complexContent/xsd:extension", schema_ns)
      if (is_xml_missing(cc_node)) {
        cc_node <- xml2::xml_find_first(node, "./xsd:complexContent/xsd:restriction", schema_ns)
      }

      content_owner <- if (!is_xml_missing(cc_node)) cc_node else node

      if (!is_xml_missing(cc_node)) {
        base_name <- self$strip_ns(xml2::xml_attr(cc_node, "base"))
        if (!is.null(base_name) && !is.na(base_name) && nzchar(base_name) && !base_name %in% visited) {
          base_res <- private$find_complex_type(base_name)
          if (!is.null(base_res)) {
            base_def <- self$parse_complex_type(
              base_res$node,
              schema_ns = base_res$ns,
              type_name = base_name,
              visited = c(visited, base_name),
              nested_collector = nested_collector
            )
            for (an in names(base_def$attributes)) {
              attr_defs[[an]] <- base_def$attributes[[an]]
            }
            for (en in names(base_def$elements)) {
              elem_defs[[en]] <- base_def$elements[[en]]
            }
            if (isTRUE(base_def$simple_content)) {
              is_simple_content <- TRUE
              simple_base <- base_def$base
            }
          }
        }
      }

      # Parse own attributes
      own_attrs <- private$parse_attribute_list(content_owner, schema_ns)
      for (an in names(own_attrs)) {
        attr_defs[[an]] <- own_attrs[[an]]
      }

      # Parse own elements from sequence / choice / all compositors
      elem_items <- private$collect_compositor_elements(content_owner)
      for (item in elem_items) {
        e <- item$node
        name <- xml2::xml_attr(e, "name")
        type <- self$strip_ns(xml2::xml_attr(e, "type"))

        # Handle ref across all collected schemas (inline, imported, included)
        if (is.na(name)) {
          ref <- self$strip_ns(xml2::xml_attr(e, "ref"))
          if (!is.na(ref)) {
            name <- ref
            resolved <- private$find_global_element(ref)

            if (!is.null(resolved)) {
              global_elem <- resolved$node
              global_ns <- resolved$ns
              ref_type <- self$strip_ns(xml2::xml_attr(global_elem, "type"))
              if (!is.na(ref_type)) {
                type <- ref_type
              } else if (
                !is_xml_missing(xml2::xml_find_first(global_elem, "./xsd:complexType", global_ns)) ||
                  !is_xml_missing(xml2::xml_find_first(global_elem, "./xsd:simpleType", global_ns))
              ) {
                type <- ref
              }
            }

            if (is.na(type)) {
              type <- ref
            }
          }
        } else if (is.na(type)) {
          # Handle inline anonymous <xsd:complexType> / <xsd:simpleType>
          nested_prefix <- if (!is.na(effective_name) && nzchar(effective_name)) {
            paste0(effective_name, "_", name)
          } else {
            name
          }
          inline_ct <- xml2::xml_find_first(e, "./xsd:complexType", schema_ns)
          inline_st <- xml2::xml_find_first(e, "./xsd:simpleType", schema_ns)
          if (!is_xml_missing(inline_ct)) {
            type <- nested_prefix
            nested_def <- self$parse_complex_type(
              inline_ct,
              schema_ns = schema_ns,
              type_name = nested_prefix,
              visited = visited,
              nested_collector = nested_collector
            )
            if (!is.null(nested_collector)) {
              nested_collector$types[[nested_prefix]] <- nested_def
            }
          } else if (!is_xml_missing(inline_st)) {
            type <- nested_prefix
            nested_st <- self$parse_simple_type(inline_st, schema_ns = schema_ns)
            if (!is.null(nested_collector)) {
              nested_collector$types[[nested_prefix]] <- nested_st
            }
          }
        }

        if (!is.na(name)) {
          elem_min <- xml2::xml_attr(e, "minOccurs")
          if (isTRUE(item$in_choice)) {
            elem_min <- "0"
          }
          elem_max <- xml2::xml_attr(e, "maxOccurs")
          if (is.na(elem_max) && !is.null(item$inherited_max) && !is.na(item$inherited_max)) {
            elem_max <- item$inherited_max
          }
          elem_defs[[name]] <- list(
            name = name,
            type = type,
            minOccurs = elem_min,
            maxOccurs = elem_max,
            default = xml2::xml_attr(e, "default"),
            documentation = self$parse_documentation(e, schema_ns = schema_ns)
          )
        }
      }

      doc <- self$parse_documentation(node, schema_ns = schema_ns)
      res <- list(
        kind = "complex",
        attributes = attr_defs,
        elements = elem_defs,
        documentation = doc
      )
      if (is_simple_content) {
        res$simple_content <- TRUE
        res$base <- simple_base
        res$value_field <- if ("value" %in% names(attr_defs)) ".value" else "value"
      }
      res
    },

    #' @description
    #' Parse a simpleType node.
    #' @param node The simpleType xml_node.
    #' @param schema_ns Namespace vector to use for XPath queries on this schema.
    #' @return A list representing the simple type.
    parse_simple_type = function(node, schema_ns = self$ns) {
      restriction <- xml2::xml_find_first(node, "./xsd:restriction", schema_ns)
      base <- self$strip_ns(xml2::xml_attr(restriction, "base"))

      enums <- xml2::xml_find_all(restriction, "./xsd:enumeration", schema_ns)
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
    #' @param schema_ns Namespace vector to use for XPath queries on this schema.
    #' @return The documentation text, or NULL.
    parse_documentation = function(node, schema_ns = self$ns) {
      doc_node <- xml2::xml_find_first(node, "./wsdl:documentation", schema_ns)
      if (is_xml_missing(doc_node)) {
        # Try xsd:annotation/xsd:documentation for types
        doc_node <- xml2::xml_find_first(
          node,
          ".//xsd:annotation/xsd:documentation",
          schema_ns
        )
      }

      if (!is_xml_missing(doc_node)) {
        return(trimws(xml2::xml_text(doc_node)))
      }
      return(NULL)
    }
  ),
  private = list(
    normalize_schema_key = function(uri) {
      if (is.null(uri) || length(uri) == 0L || is.na(uri)) {
        return("")
      }
      uri <- trimws(as.character(uri))
      if (grepl("^https?://", uri, ignore.case = TRUE)) {
        return(sub("#.*$", "", uri))
      }
      local_path <- sub("^file://", "", uri)
      normalizePath(local_path, winslash = "/", mustWork = FALSE)
    },

    resolve_schema_location = function(location, base_uri) {
      if (is.null(location) || is.na(location) || !nzchar(trimws(location))) {
        return(NULL)
      }
      location <- trimws(location)
      if (grepl("^https?://", location, ignore.case = TRUE)) {
        return(location)
      }
      if (
        !is.null(base_uri) &&
          length(base_uri) == 1L &&
          !is.na(base_uri) &&
          grepl("^https?://", base_uri, ignore.case = TRUE)
      ) {
        return(xml2::url_absolute(location, base_uri))
      }
      if (grepl("^file://", location, ignore.case = TRUE)) {
        return(private$normalize_schema_key(location))
      }
      if (grepl("^(/|[A-Za-z]:[/\\\\])", location)) {
        return(private$normalize_schema_key(location))
      }
      base_dir <- if (!is.null(base_uri) && length(base_uri) == 1L && !is.na(base_uri)) {
        dirname(sub("^file://", "", base_uri))
      } else {
        "."
      }
      private$normalize_schema_key(file.path(base_dir, location))
    },

    fetch_schema_doc = function(uri) {
      tryCatch(
        {
          if (!is.null(self$schema_loader)) {
            res <- self$schema_loader(uri)
            if (inherits(res, "xml_document")) {
              return(res)
            }
            if (is.character(res) && length(res) > 0L) {
              return(xml2::read_xml(paste(res, collapse = "\n")))
            }
            return(NULL)
          }
          if (grepl("^https?://", uri, ignore.case = TRUE)) {
            resp <- httr::GET(uri, httr::timeout(2))
            httr::stop_for_status(resp)
            txt <- httr::content(resp, "text", encoding = "UTF-8")
            if (is.null(txt) || is.na(txt) || !nzchar(trimws(txt))) {
              return(NULL)
            }
            return(xml2::read_xml(txt))
          }
          if (!file.exists(uri)) {
            return(NULL)
          }
          xml2::read_xml(uri)
        },
        error = function(e) NULL
      )
    },

    collect_schemas = function() {
      state <- new.env(parent = emptyenv())
      state$schemas <- list()
      state$visited <- character(0)

      if (!is.null(self$source) && nzchar(trimws(as.character(self$source)))) {
        state$visited <- private$normalize_schema_key(self$source)
      }

      inline_nodes <- xml2::xml_find_all(self$doc, "//xsd:schema", self$ns)
      for (node in inline_nodes) {
        private$process_schema_node(
          schema_node = node,
          schema_ns = self$ns,
          source_uri = self$source,
          kind = "inline",
          inherited_tns = NA_character_,
          state = state
        )
      }

      self$schemas <- state$schemas
      invisible(NULL)
    },

    process_schema_node = function(
      schema_node,
      schema_ns,
      source_uri,
      kind,
      inherited_tns = NA_character_,
      state
    ) {
      tns <- xml2::xml_attr(schema_node, "targetNamespace")
      if (is.na(tns) || !nzchar(tns)) {
        tns <- inherited_tns
      }
      efd <- xml2::xml_attr(schema_node, "elementFormDefault")
      if (is.na(efd) || !nzchar(efd)) {
        efd <- "unqualified"
      }
      afd <- xml2::xml_attr(schema_node, "attributeFormDefault")
      if (is.na(afd) || !nzchar(afd)) {
        afd <- "unqualified"
      }

      state$schemas[[length(state$schemas) + 1L]] <- list(
        node = schema_node,
        ns = schema_ns,
        target_namespace = tns,
        element_form_default = efd,
        attribute_form_default = afd,
        source = source_uri,
        kind = kind
      )

      imports <- xml2::xml_find_all(schema_node, "./xsd:import", schema_ns)
      for (imp in imports) {
        loc <- xml2::xml_attr(imp, "schemaLocation")
        imp_tns <- xml2::xml_attr(imp, "namespace")
        resolved <- private$resolve_schema_location(loc, source_uri)
        if (!is.null(resolved)) {
          private$load_external_schema(resolved, kind = "import", inherited_tns = imp_tns, state = state)
        }
      }

      includes <- xml2::xml_find_all(schema_node, "./xsd:include", schema_ns)
      for (inc in includes) {
        loc <- xml2::xml_attr(inc, "schemaLocation")
        resolved <- private$resolve_schema_location(loc, source_uri)
        if (!is.null(resolved)) {
          private$load_external_schema(resolved, kind = "include", inherited_tns = tns, state = state)
        }
      }
    },

    load_external_schema = function(uri, kind, inherited_tns, state) {
      key <- private$normalize_schema_key(uri)
      if (!nzchar(key) || key %in% state$visited) {
        return(invisible(NULL))
      }
      state$visited <- c(state$visited, key)

      ext_doc <- private$fetch_schema_doc(uri)
      if (is.null(ext_doc)) {
        return(invisible(NULL))
      }

      ext_ns <- self$extend_namespace(xml2::xml_ns(ext_doc))
      schema_nodes <- xml2::xml_find_all(ext_doc, "//xsd:schema", ext_ns)
      for (s_node in schema_nodes) {
        private$process_schema_node(
          schema_node = s_node,
          schema_ns = ext_ns,
          source_uri = uri,
          kind = kind,
          inherited_tns = inherited_tns,
          state = state
        )
      }
      invisible(NULL)
    },

    find_global_element = function(elem_name) {
      xpath <- sprintf("./xsd:element[@name='%s']", elem_name)
      for (entry in self$schemas) {
        node <- xml2::xml_find_first(entry$node, xpath, entry$ns)
        if (!is_xml_missing(node)) {
          return(list(node = node, ns = entry$ns))
        }
      }
      NULL
    },

    find_complex_type = function(type_name) {
      xpath <- sprintf("./xsd:complexType[@name='%s']", type_name)
      for (entry in self$schemas) {
        node <- xml2::xml_find_first(entry$node, xpath, entry$ns)
        if (!is_xml_missing(node)) {
          return(list(node = node, ns = entry$ns))
        }
      }
      NULL
    },

    parse_attribute_list = function(owner_node, schema_ns) {
      attrs <- xml2::xml_find_all(owner_node, "./xsd:attribute", schema_ns)
      attr_defs <- list()
      for (a in attrs) {
        name <- xml2::xml_attr(a, "name")
        if (!is.na(name)) {
          attr_defs[[name]] <- list(
            name = name,
            type = self$strip_ns(xml2::xml_attr(a, "type")),
            use = xml2::xml_attr(a, "use"),
            default = xml2::xml_attr(a, "default"),
            documentation = self$parse_documentation(a, schema_ns = schema_ns)
          )
        }
      }
      attr_defs
    },

    collect_compositor_elements = function(container, in_choice = FALSE, inherited_max = NULL) {
      items <- list()
      children <- xml2::xml_children(container)
      for (child in children) {
        local_name <- xml2::xml_name(child)
        if (local_name == "element") {
          items[[length(items) + 1L]] <- list(
            node = child,
            in_choice = in_choice,
            inherited_max = inherited_max
          )
        } else if (local_name %in% c("sequence", "all", "choice")) {
          child_in_choice <- in_choice || identical(local_name, "choice")
          comp_max <- xml2::xml_attr(child, "maxOccurs")
          next_max <- if (!is.na(comp_max)) comp_max else inherited_max
          sub_items <- private$collect_compositor_elements(
            child,
            in_choice = child_in_choice,
            inherited_max = next_max
          )
          if (length(sub_items) > 0L) {
            items <- c(items, sub_items)
          }
        }
      }
      items
    }
  )
)
