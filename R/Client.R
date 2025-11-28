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
    target_namespace = NULL,
    handle = NULL,
    serializer = NULL,
    transport = NULL,
    initialize = function(
      wsdl,
      port_def,
      factory,
      transport = httr::POST,
      verbose = FALSE
    ) {
      self$wsdl <- wsdl
      self$port_def <- port_def
      self$factory <- factory
      self$target_namespace <- self$wsdl$get_target_namespace()
      self$verbose <- verbose
      self$handle <- httr::handle(self$port_def$location)
      self$operations <- self$wsdl$get_operations(port_def$binding)
      self$serializer <- XMLSerializer$new()
      self$transport <- transport
      self$headers <- list()

      # Dynamically add methods for each operation
      for (op in self$operations) {
        self$add_operation_method(op)
      }
    },
    add_header = function(node) {
      self$headers <- c(self$headers, list(node))
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

    handle_fault = function(response) {
      # Parse the response content
      doc <- tryCatch(
        {
          self$serializer$deserialize_response(response)
        },
        error = function(e) {
          cat(
            "Deserialization error:",
            e$message,
            "\n",
            file = "/tmp/soapr_debug.log",
            append = TRUE
          )
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
      if (inherits(fault_node, "xml_missing")) {
        fault_node <- tryCatch(
          xml2::xml_find_first(doc, ".//env:Fault", ns),
          warning = function(w) xml2::xml_missing()
        )
      }

      if (inherits(fault_node, "xml_missing")) {
        # Fallback for when prefix is different but local name is Fault
        fault_node <- xml2::xml_find_first(doc, ".//*[local-name()='Fault']")
      }

      if (!inherits(fault_node, "xml_missing")) {
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
