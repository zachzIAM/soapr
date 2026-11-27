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
      node_ns <- private$resolve_node_ns(ns)

      if (inherits(value, "soap_object")) {
        node <- private$add_namespaced_child(parent, name, node_ns)
        private$apply_soap_attrs(node, value)

        soap_attrs <- attr(value, "soap_attrs")
        for (elem_name in names(value)) {
          if (!elem_name %in% soap_attrs) {
            self$serialize(node, elem_name, value[[elem_name]])
          }
        }
      } else if (is.list(value)) {
        if (!is.null(names(value)) && any(names(value) != "")) {
          # Named list -> Treat as fields of a complex type (structure)
          node <- private$add_namespaced_child(parent, name, node_ns)
          private$apply_soap_attrs(node, value)

          soap_attrs <- attr(value, "soap_attrs")
          for (field_name in names(value)) {
            if (!field_name %in% soap_attrs) {
              self$serialize(node, field_name, value[[field_name]])
            }
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
        private$add_namespaced_child(parent, name, node_ns, text = val_str)
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
  ),
  private = list(
    get_active_default_ns = function(node) {
      cur <- node
      while (inherits(cur, "xml_node") && xml2::xml_type(cur) == "element") {
        ns_attr <- xml2::xml_attr(cur, "xmlns")
        if (!is.na(ns_attr)) {
          return(ns_attr)
        }
        cur <- xml2::xml_parent(cur)
      }
      ""
    },

    resolve_node_ns = function(ns = NULL) {
      if (!is.null(ns)) {
        return(ns)
      }
      if (identical(self$element_form_default, "qualified")) {
        if (!is.null(self$target_namespace) && nzchar(self$target_namespace)) {
          return(self$target_namespace)
        }
        return(NULL)
      }
      ""
    },

    add_namespaced_child = function(parent, name, node_ns, text = NULL) {
      parent_ns <- private$get_active_default_ns(parent)
      needs_xmlns <- !is.null(node_ns) && !identical(node_ns, parent_ns)

      if (is.null(text)) {
        if (needs_xmlns && nzchar(node_ns)) {
          return(xml2::xml_add_child(parent, name, xmlns = node_ns))
        }
        node <- xml2::xml_add_child(parent, name)
      } else {
        if (needs_xmlns && nzchar(node_ns)) {
          return(xml2::xml_add_child(parent, name, text, xmlns = node_ns))
        }
        node <- xml2::xml_add_child(parent, name, text)
      }

      if (needs_xmlns && identical(node_ns, "")) {
        xml2::xml_attr(node, "xmlns") <- ""
      }
      node
    },

    apply_soap_attrs = function(node, value) {
      soap_attrs <- attr(value, "soap_attrs")
      if (is.null(soap_attrs)) {
        return()
      }
      for (attr_name in soap_attrs) {
        attr_val <- value[[attr_name]]
        if (!is.null(attr_val) && length(attr_val) > 0 && !is.na(attr_val[[1]])) {
          attr_str <- if (is.logical(attr_val)) {
            tolower(as.character(attr_val))
          } else {
            as.character(attr_val)
          }
          full_attr_name <- if (identical(self$attribute_form_default, "qualified")) {
            paste0(self$target_namespace_prefix, ":", attr_name)
          } else {
            attr_name
          }
          xml2::xml_attr(node, full_attr_name) <- attr_str
        }
      }
    }
  )
)
