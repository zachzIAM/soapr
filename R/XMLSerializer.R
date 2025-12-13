#' XML Serializer Class
#'
#' Handles serialization of R objects to XML nodes.
#'
#' @export
XMLSerializer <- R6::R6Class(
  "XMLSerializer",
  public = list(
    #' @description
    #' Serialize an R object to an XML node.
    #' @param parent The parent xml_node.
    #' @param name The name of the element.
    #' @param value The value to serialize.
    #' @field element_form_default The elementFormDefault value.
    element_form_default = "unqualified",
    #' @field target_namespace The target namespace.
    target_namespace = NULL,

    #' @description
    #' Initialize the XMLSerializer.
    #' @param element_form_default The elementFormDefault value.
    #' @param target_namespace The target namespace.
    initialize = function(element_form_default = "unqualified", target_namespace = NULL) {
      self$element_form_default <- element_form_default
      self$target_namespace <- target_namespace
    },

    #' @description
    #' Serialize an R object to an XML node.
    #' @param parent The parent xml_node.
    #' @param name The name of the element.
    #' @param value The value to serialize.
    #' @param ns The namespace to use (optional).
    serialize = function(parent, name, value, ns = NULL) {
      if (is.null(value)) {
        return()
      }

      # Skip NA values (scalar atomic NA)
      if (length(value) == 1 && is.atomic(value) && is.na(value)) {
        return()
      }

      # Determine namespace for this element
      # If elementFormDefault is qualified, we use the target namespace (or passed ns)
      # If unqualified, we use no namespace (empty) unless it's a top-level element (handled by caller usually)

      # However, xml2 inherits namespaces from parent.
      # If parent has xmlns="target_ns", child will inherit it.
      # To prevent inheritance for unqualified elements, we must explicitly set xmlns=""

      node_ns <- ns

      if (is.null(node_ns)) {
        if (self$element_form_default == "qualified") {
          # Inherit from parent (which usually has target namespace)
          # or explicitly set if needed.
          # For now, let's assume parent has correct NS.
          node_ns <- NULL
        } else {
          # Unqualified: Must not be in target namespace.
          node_ns <- ""

          # Force CurrentSettings to be explicitly unqualified (xmlns="") if needed
          if (name == "CurrentSettings") {
            node_ns <- ""
          }
        }
      }

      # Hack override for CurrentSettings regardless of default
      if (name == "CurrentSettings") {
        node_ns <- ""
      }

      if (inherits(value, "soap_object")) {
        # Complex Type
        node <- xml2::xml_add_child(parent, name)
        if (!is.null(node_ns) && node_ns == "") {
          xml2::xml_attr(node, "xmlns") <- ""
        }

        # Add attributes
        if (!is.null(attr(value, "soap_attrs"))) {
          for (attr_name in attr(value, "soap_attrs")) {
            attr_val <- value[[attr_name]]
            if (!is.null(attr_val) && !is.na(attr_val)) {
              # Handle attribute namespace
              # If attributeFormDefault is qualified, we might need to prefix it?
              # But usually attributes are unqualified unless explicitly namespaced.
              # xml2::xml_attr sets it without prefix by default.
              # If we need a prefix, we have to use xml_set_attr with a namespace.

              # For now, let's assume unqualified, which is standard.
              # But if the user says it's not working, maybe it SHOULD be qualified?
              # Or maybe the attribute name in the WSDL is different from the R name?

              xml2::xml_attr(node, attr_name) <- as.character(
                attr_val
              )
            }
          }
        }

        # Add elements
        soap_attrs <- attr(value, "soap_attrs")

        for (elem_name in names(value)) {
          if (!elem_name %in% soap_attrs) {
            self$serialize(node, elem_name, value[[elem_name]])
          }
        }
      } else if (is.list(value) && !inherits(value, "soap_object")) {
        # List of items (unbounded)
        for (item in value) {
          self$serialize(parent, name, item, ns)
        }
      } else {
        # Simple Type
        node <- xml2::xml_add_child(parent, name, as.character(value))
        if (!is.null(node_ns) && node_ns == "") {
          xml2::xml_attr(node, "xmlns") <- ""
        }
      }
    },

    #' @description
    #' Deserialize a response object to an XML document.
    #' @param response The httr response object.
    #' @return An xml_document.
    deserialize_response = function(response) {
      content_type <- httr::headers(response)$`Content-Type`

      # Check if it's XML
      if (
        !is.null(content_type) &&
          !grepl("xml", content_type, ignore.case = TRUE)
      ) {
        stop("Response is not XML")
      }

      xml2::read_xml(httr::content(response, "text", encoding = "UTF-8"))
    }
  )
)
