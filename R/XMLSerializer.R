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
    #' @field attribute_form_default The attributeFormDefault value.
    attribute_form_default = "unqualified",
    #' @field target_namespace The target namespace.
    target_namespace = NULL,
    #' @field target_namespace_prefix The prefix for target namespace (default "tns").
    target_namespace_prefix = "tns",

    #' @description
    #' Initialize the XMLSerializer.
    #' @param element_form_default The elementFormDefault value.
    #' @param attribute_form_default The attributeFormDefault value.
    #' @param target_namespace The target namespace.
    #' @param target_namespace_prefix The prefix for target namespace.
    initialize = function(
      element_form_default = "unqualified",
      attribute_form_default = "unqualified",
      target_namespace = NULL,
      target_namespace_prefix = "tns"
    ) {
      self$element_form_default <- element_form_default
      self$attribute_form_default <- attribute_form_default
      self$target_namespace <- target_namespace
      self$target_namespace_prefix <- target_namespace_prefix
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
      node_ns <- ns

      if (is.null(node_ns)) {
        if (self$element_form_default == "qualified") {
          if (!is.null(self$target_namespace)) {
            node_ns <- self$target_namespace
          } else {
            node_ns <- NULL
          }
        } else {
          node_ns <- "" # Unqualified - Explicitly remove namespace
        }
      }

      if (inherits(value, "soap_object")) {
        # Complex Type
        if (!is.null(node_ns) && node_ns != "") {
          # Qualified: Use xmlns to force prefix usage
          node <- xml2::xml_add_child(parent, name, xmlns = node_ns)
        } else {
          # Unqualified or Inherit
          node <- xml2::xml_add_child(parent, name)
        }

        if (!is.null(node_ns) && node_ns == "") {
          xml2::xml_attr(node, "xmlns") <- ""
        }

        # Add attributes
        if (!is.null(attr(value, "soap_attrs"))) {
          for (attr_name in attr(value, "soap_attrs")) {
            attr_val <- value[[attr_name]]
            if (!is.null(attr_val) && !is.na(attr_val)) {
              # Check attribute qualification
              full_attr_name <- attr_name
              if (self$attribute_form_default == "qualified") {
                # Prefix with target namespace prefix
                full_attr_name <- paste0(self$target_namespace_prefix, ":", attr_name)
              }
              xml2::xml_attr(node, full_attr_name) <- as.character(attr_val)
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
        if (!is.null(names(value)) && any(names(value) != "")) {
          # Named list -> Treat as fields of a complex type (structure)
          # Create a container node for this complex type
          node <- xml2::xml_add_child(parent, name)
          if (!is.null(node_ns) && node_ns == "") {
            xml2::xml_attr(node, "xmlns") <- ""
          }

          # Recursively serialize fields
          # Note: We don't know which are attributes vs elements here without schema.
          # Defaulting to elements is safer for generic named lists.
          for (field_name in names(value)) {
            self$serialize(node, field_name, value[[field_name]])
          }
        } else {
          # Unnamed list -> Repeated items (maxOccurs > 1)
          for (item in value) {
            self$serialize(parent, name, item, ns)
          }
        }
      } else {
        # Simple Type
        val_str <- if (is.logical(value)) {
          tolower(as.character(value))
        } else {
          as.character(value)
        }
        node <- xml2::xml_add_child(parent, name, val_str)
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
