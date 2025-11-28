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
      return(content)
    },

    build_xml_node = function(parent, name, value) {
      if (is.null(value)) {
        return()
      }

      # Handle lists (maxOccurs="unbounded")
      if (is.list(value) && is.null(names(value))) {
        for (item in value) {
          self$build_xml_node(parent, name, item)
        }
        return()
      }

      # Create node
      # Check if name is namespaced
      node_name <- if (grepl(":", name)) name else paste0("tns:", name)
      child <- xml2::xml_add_child(parent, node_name)

      if (is.list(value)) {
        # Complex type
        # We need to distinguish between attributes and child elements
        # Ideally we check the type definition, but for now we can use a heuristic:
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

        # Let's stick to a simple recursive strategy first:
        # Iterate over keys.
        for (key in names(value)) {
          val <- value[[key]]
          # Check if it should be an attribute?
          # We can check if the key matches a known attribute for this type?
          # Without type info, we can't be sure.

          # Let's assume for now that we treat everything as elements
          # UNLESS the key starts with "attr_" (convention?)
          # OR we look up the type definition.

          # Let's try to look up the element in the schema.
          # We can search for an element with this name in the schema types.
          # This is slow and maybe inaccurate.

          # Alternative: The user passes an object created by Factory, which has class "soap_object"
          # and carries its type name.
          if (inherits(value, "soap_object")) {
            type_name <- attr(value, "soap_type")
            type_def <- self$factory$types[[type_name]]

            if (!is.null(type_def) && !is.null(type_def$attributes[[key]])) {
              xml2::xml_set_attr(child, key, as.character(val))
              next
            }
          }

          self$build_xml_node(child, key, val)
        }
      } else {
        # Simple type
        xml2::xml_set_text(child, as.character(value))
      }
    }
  )
)
