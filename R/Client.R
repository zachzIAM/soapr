#' Client Class
#'
#' Handles SOAP requests.
#'
#' @export
Client <- R6::R6Class(
  "Client",
  lock_objects = FALSE,
  public = list(
    #' @field wsdl The WSDLParser instance.
    wsdl = NULL,
    #' @field port_def The port definition.
    port_def = NULL,
    #' @field factory The TypeFactory instance.
    factory = NULL,
    #' @field operations List of operations.
    operations = NULL,
    #' @field target_namespace The target namespace.
    target_namespace = NULL,
    #' @field handle The httr handle.
    handle = NULL,
    #' @field serializer The XMLSerializer instance.
    serializer = NULL,
    #' @field transport The transport function.
    transport = NULL,
    #' @field headers List of SOAP headers.
    headers = NULL,
    #' @field verbose Logical, whether to print debug info.
    verbose = FALSE,

    #' @description
    #' Initialize the Client.
    #' @param wsdl A WSDLParser instance.
    #' @param port_def A port definition list (optional).
    #' @param factory A TypeFactory instance (optional).
    #' @param transport A function to handle the HTTP request (default: httr::POST).
    #' @param verbose Logical, whether to print debug info (default: FALSE).
    initialize = function(wsdl,
                          port_def = NULL,
                          factory = NULL,
                          transport = httr::POST,
                          verbose = FALSE) {
      self$wsdl <- wsdl
      if (is.null(port_def)) {
        # Default to first port of first service
        self$port_def <- self$wsdl$get_services()[[1]]$ports[[1]]
      } else {
        self$port_def <- port_def
      }
      if (is.null(factory)) {
        self$factory <- Factory$new(wsdl)
      } else {
        self$factory <- factory
      }
      self$target_namespace <- self$wsdl$get_target_namespace()
      self$verbose <- verbose
      self$handle <- httr::handle(self$port_def$location)
      self$operations <- self$wsdl$get_operations(self$port_def$binding)
      self$serializer <- XMLSerializer$new(
        element_form_default = self$wsdl$element_form_default,
        attribute_form_default = self$wsdl$attribute_form_default,
        target_namespace = self$target_namespace
      )
      self$transport <- transport
      self$headers <- list()

      # Dynamically add methods for each operation
      for (op in self$operations) {
        self$add_operation_method(op)
      }
    },

    #' @description
    #' Add a SOAP header.
    #' @param node An xml_node object representing the header.
    add_header = function(node) {
      self$headers <- c(self$headers, list(node))
    },

    #' @description
    #' Add a method for an operation to the client.
    #' @param op The operation definition.
    add_operation_method = function(op) {
      self[[op$name]] <- function(...) {
        args <- list(...)
        self$call_operation(op, args)
      }
    },

    #' @description
    #' Call a SOAP operation.
    #' @param op The operation definition.
    #' @param args A list of arguments for the operation.
    #' @return The result of the operation.
    call_operation = function(op, args) {
      # Construct SOAP Envelope
      envelope <- xml2::xml_new_root(
        "soap:Envelope",
        "xmlns:soap" = "http://schemas.xmlsoap.org/soap/envelope/",
        "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
        "xmlns:xsd" = "http://www.w3.org/2001/XMLSchema"
      )

      # Add Headers if present
      if (length(self$headers) > 0) {
        header_node <- xml2::xml_add_child(envelope, "soap:Header")
        for (h in self$headers) {
          xml2::xml_add_child(header_node, h)
        }
      }

      body <- xml2::xml_add_child(envelope, "soap:Body")

      # Use default namespace for the operation and its children
      op_node <- xml2::xml_add_child(body, op$name)
      xml2::xml_attr(op_node, "xmlns") <- self$target_namespace

      # Get input message type definition if available
      # This is tricky because the operation input message points to an element
      # We need to find the complexType of that element

      # For now, we assume args match the expected structure
      for (arg_name in names(args)) {
        if (
          is.list(args[[arg_name]]) &&
            !inherits(args[[arg_name]], "soap_object") &&
            !is.null(self$factory$types[[arg_name]])
        ) {
          # Attempt coercion to soap_object
          obj <- self$factory$create(arg_name)
          input_list <- args[[arg_name]]

          # Copy values
          for (n in names(input_list)) {
            obj[[n]] <- input_list[[n]]
          }
          # Replace argument with coerced object
          args[[arg_name]] <- obj
        }

        # Validate Enums (Existing logic...)
        # Find the element definition for this argument
        # The operation input message name is op$input_message
        # We need to find the message part, then the element, then the type.
        # This is complex to traverse at runtime efficiently without a full schema map.
        # However, we can try a best-effort lookup if we have the type definitions.

        # Simplified approach: Check if any type in the WSDL matches the argument name (common convention)
        # Or better, iterate through all types and see if any simpleType has this name? No, that's slow.

        # Let's rely on the fact that we have `self$wsdl$get_type_definitions()` available.
        # But we need to know the TYPE of the argument.
        # In document/literal, the arg_name IS the element name.
        # So we look up the element in the schema.

        # We need a way to look up element -> type.
        # The WSDLParser doesn't expose a direct element map, but get_type_definitions returns types.
        # Some types are elements.

        # Let's try to find the type definition for the element 'arg_name'
        type_def <- self$wsdl$get_type_definitions()[[arg_name]]

        # If not found directly, it might be a complex type's element.
        # But for top-level arguments to an operation, they are usually elements of the input message wrapper.

        # If we found a type definition and it's a simple type with values (enum)
        if (
          !is.null(type_def) &&
            type_def$kind == "simple" &&
            !is.null(type_def$values)
        ) {
          val <- args[[arg_name]]
          if (!val %in% type_def$values) {
            stop(sprintf(
              "Invalid value '%s' for argument '%s'. Allowed values: %s",
              val,
              arg_name,
              paste(type_def$values, collapse = ", ")
            ))
          }
        } else {
          # It might be an element that refers to a simpleType
          # In our parser, elements that define types are in the types list.
          # This only covers elements that have INLINE complexTypes.

          # If an element refers to a named simpleType, it might not be in the top-level types list as an element.
          # It would be in the complexType definition of the operation's input wrapper.

          # To do this correctly, we need the operation's input type definition.
          # self$operations[[op$name]] doesn't have the full type tree.

          # Let's try to get the input type definition for the operation.
          # The operation name usually matches the input element name in wrapped doc/lit.
          op_input_type <- self$wsdl$get_type_definitions()[[op$name]]

          if (
            !is.null(op_input_type) &&
              !is.null(op_input_type$elements[[arg_name]])
          ) {
            elem_def <- op_input_type$elements[[arg_name]]
            elem_type_name <- elem_def$type

            # Look up the type of the element
            elem_type_def <- self$wsdl$get_type_definitions()[[elem_type_name]]

            if (
              !is.null(elem_type_def) &&
                elem_type_def$kind == "simple" &&
                !is.null(elem_type_def$values)
            ) {
              val <- args[[arg_name]]
              if (!val %in% elem_type_def$values) {
                stop(sprintf(
                  "Invalid value '%s' for argument '%s'. Allowed values: %s",
                  val,
                  arg_name,
                  paste(elem_type_def$values, collapse = ", ")
                ))
              }
            }
          }
        }

        self$serializer$serialize(op_node, arg_name, args[[arg_name]])
      }

      # Make Request
      if (self$verbose) {
        cat("Request XML:\n")
        print(envelope)
      }

      response <- self$transport(
        url = self$port_def$location,
        handle = self$handle,
        httr::add_headers(SOAPAction = op$action),
        httr::content_type("text/xml; charset=utf-8"),
        body = as.character(envelope)
      )

      # Check for SOAP Fault
      self$handle_fault(response)

      # Parse Response
      content <- httr::content(response, as = "parsed", encoding = "UTF-8")

      if (self$verbose) {
        cat("Response XML:\n")
        print(content)
      }

      # Extract Body
      body_node <- xml2::xml_find_first(content, ".//*[local-name()='Body']")

      # The body should contain the response element
      # We expect the first child of Body to be the response
      # But we should check op$output to be sure what we are looking for

      # op$output is the output message name.
      # We need to find the element name associated with that message.
      # This info is not easily available here without looking up the message definition again.
      # However, usually it's just the first child.

      response_node <- xml2::xml_children(body_node)[[1]]

      # We need the type name for this response node.
      # In document/literal wrapped, the element name IS the type name (or close to it).
      # Let's try to use the local name of the response node.
      response_name <- xml2::xml_name(response_node)

      # Remove namespace prefix if present (xml_name usually returns local name if not qualified, but let's be safe)
      # xml_name returns the name with prefix if it has one? No, it returns local name usually.
      # Wait, xml_name returns "tns:SetAdHocPortfolioResponse" or just "SetAdHocPortfolioResponse"?
      # It depends on how it was parsed.
      # Let's assume it matches a type in our factory.

      # If we can't find the type, we return the xml_node (backward compatibility)
      if (!is.null(self$factory$types[[response_name]])) {
        return(self$factory$deserialize(response_node, response_name))
      } else {
        # Try stripping namespace just in case
        local_name <- gsub("^.*:", "", response_name)
        if (!is.null(self$factory$types[[local_name]])) {
          return(self$factory$deserialize(response_node, local_name))
        }
      }

      return(content)
    },
    #' @description
    #' Handle SOAP Faults.
    #' @param response The httr response object.
    handle_fault = function(response) {
      # Parse the response content
      doc <- tryCatch(
        {
          self$serializer$deserialize_response(response)
        },
        error = function(e) {
          logfile <- file.path(tempdir(), "soap_debug.log")
          msg <- sprintf("Deserialization error: %s\n", e$message)
          cat(msg, file = logfile, append = TRUE)
          warning(sprintf("%sSee log %s", msg, logfile))
          # If not XML, let httr handle the status
          return(NULL)
        }
      )

      if (is.null(doc)) {
        httr::stop_for_status(response)
        return()
      }

      ns <- xml2::xml_ns(doc)

      # Check for SOAP 1.1 Fault
      # Only search for soap:Fault if soap prefix is defined or we provide it
      # Ideally we should use the namespace URI, but xml2 makes that verbose in XPath
      # Let's try to find it safely
      fault_node <- tryCatch(
        xml2::xml_find_first(doc, ".//soap:Fault", ns),
        warning = function(w) xml2::xml_missing()
      )

      # Check for SOAP 1.2 Fault
      if (is_xml_missing(fault_node)) {
        fault_node <- tryCatch(
          xml2::xml_find_first(doc, ".//env:Fault", ns),
          warning = function(w) xml2::xml_missing()
        )
      }

      if (is_xml_missing(fault_node)) {
        # Fallback for when prefix is different but local name is Fault
        fault_node <- xml2::xml_find_first(doc, ".//*[local-name()='Fault']")
      }

      if (!is_xml_missing(fault_node)) {
        # Extract Fault Code
        fault_code <- xml2::xml_text(xml2::xml_find_first(
          fault_node,
          ".//*[local-name()='faultcode' or local-name()='Code']"
        ))

        # Extract Fault String/Reason
        fault_string <- xml2::xml_text(xml2::xml_find_first(
          fault_node,
          ".//*[local-name()='faultstring' or local-name()='Reason']"
        ))

        # Extract Detail
        fault_detail <- xml2::xml_text(xml2::xml_find_first(
          fault_node,
          ".//*[local-name()='detail' or local-name()='Detail']"
        ))

        msg <- sprintf("SOAP Fault: [%s] %s", fault_code, fault_string)
        if (!is.na(fault_detail) && nchar(fault_detail) > 0) {
          msg <- paste0(msg, "\nDetail: ", fault_detail)
        }
        stop(msg, call. = FALSE)
      }

      # If no fault found, let httr handle it
      httr::stop_for_status(response)
    }
  )
)
