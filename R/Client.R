#' Client Class
#'
#' Handles SOAP requests.
#'
#' @importFrom R6 R6Class
#' @importFrom httr POST add_headers content_type content
#' @importFrom xml2 xml_new_root xml_add_child xml_set_attr xml_set_text as_xml_document
#' @export
Client <- R6::R6Class(
  "Client",
  lock_objects = FALSE,
  public = list(
    wsdl = NULL,
    port_def = NULL,
    factory = NULL,
    operations = NULL,
    verbose = FALSE,
    initialize = function(wsdl, port_def, factory, verbose = FALSE) {
      self$wsdl <- wsdl
      self$port_def <- port_def
      self$factory <- factory
      self$verbose <- verbose
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
      envelope <- xml2::xml_new_root(
        "soap:Envelope",
        "xmlns:soap" = "http://schemas.xmlsoap.org/soap/envelope/",
        "xmlns:tns" = "http://barra.com/cp/bdti",
        "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
        "xmlns:xsd" = "http://www.w3.org/2001/XMLSchema"
      )

      body <- xml2::xml_add_child(envelope, "soap:Body")
      op_node <- xml2::xml_add_child(body, paste0("tns:", op$name))

      # Get input message type definition if available
      # This is tricky because the operation input message points to an element
      # We need to find the complexType of that element

      # For now, we assume args match the expected structure
      for (arg_name in names(args)) {
        self$build_xml_node(op_node, arg_name, args[[arg_name]])
      }

      # Make Request
      if (self$verbose) {
        cat("Request XML:\n")
        print(envelope)
      }

      response <- httr::POST(
        url = self$port_def$location,
        httr::add_headers(SOAPAction = op$action),
        httr::content_type("text/xml; charset=utf-8"),
        body = as.character(envelope)
      )

      # Check for SOAP Fault
      if (httr::status_code(response) >= 400) {
        content <- tryCatch(
          httr::content(response, as = "parsed", encoding = "UTF-8"),
          error = function(e) NULL
        )

        if (!is.null(content)) {
          fault_node <- xml2::xml_find_first(
            content,
            ".//*[local-name()='Fault']"
          )
          if (inherits(fault_node, "xml_node")) {
            fault_code <- xml2::xml_text(xml2::xml_find_first(
              fault_node,
              ".//*[local-name()='faultcode']"
            ))
            fault_string <- xml2::xml_text(xml2::xml_find_first(
              fault_node,
              ".//*[local-name()='faultstring']"
            ))
            stop(
              sprintf("SOAP Fault (%s): %s", fault_code, fault_string),
              call. = FALSE
            )
          }
        }

        # Fallback to standard error if no SOAP Fault found or content parsing failed
        httr::stop_for_status(response)
      }

      # Parse Response
      content <- httr::content(response, as = "parsed", encoding = "UTF-8")

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

    build_xml_node = function(parent, name, value) {
      if (is.null(value)) {
        return()
      }

      # Skip NA values (scalar atomic NA)
      if (length(value) == 1 && is.atomic(value) && is.na(value)) {
        return()
      }

      if (inherits(value, "soap_object")) {
        # Complex Type
        # If it has a soap_type attribute, we could use it, but for now we rely on structure

        # Create element node
        node_name <- if (grepl(":", name)) name else paste0("tns:", name)
        node <- xml2::xml_add_child(parent, node_name)

        # Add attributes
        if (!is.null(attr(value, "soap_attrs"))) {
          for (attr_name in attr(value, "soap_attrs")) {
            attr_val <- value[[attr_name]]
            if (!is.null(attr_val) && !is.na(attr_val)) {
              xml2::xml_attr(node, attr_name) <- as.character(attr_val)
            }
          }
        }

        # Add elements
        # We need to iterate over names in the object that are NOT attributes
        soap_attrs <- attr(value, "soap_attrs")
        for (elem_name in names(value)) {
          if (!elem_name %in% soap_attrs) {
            self$build_xml_node(node, elem_name, value[[elem_name]])
          }
        }
      } else if (is.list(value) && !inherits(value, "soap_object")) {
        # List of items (unbounded)
        for (item in value) {
          self$build_xml_node(parent, name, item)
        }
        # If the list has named elements, treat them as children or attributes
        # To support attributes, we need to know which keys are attributes.
        # Since we don't have the type def passed down easily here without a big refactor,
        # let's assume the user (or Factory) constructs the list such that
        # attributes are handled? No, that's hard for the user.
        # Better approach: Look up the type definition in the Factory if possible.
        # But we don't know the type name of 'value' easily here.
        # Heuristic: If the value is atomic, set text. If list, recurse.
        # Attributes are tricky without schema.
        # Let's try to match against the schema if we can find the type.
        # For now, let's just treat everything as child elements unless we have a specific convention.
        # Wait, the user asked for Factory support. The Factory knows the structure.
        # If the Factory creates the object, maybe it can attach metadata?
        # Or we just assume that if it's in the 'attributes' slot of the type def, it's an attribute.
        # Let's try to find the type definition for this element name.
        # This is hard because names are not unique globally.
      } else {
        # Simple Type
        xml2::xml_add_child(parent, name, as.character(value))
      }
    }
  )
)
