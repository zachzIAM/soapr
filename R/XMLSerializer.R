#' XML Serializer Class
#'
#' Handles serialization of R objects to XML nodes.
#'
#' @export
XMLSerializer <- R6::R6Class(
  "XMLSerializer",
  public = list(
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
      soap_build_xml_node(
        parent = parent,
        name = name,
        value = value,
        ns = ns,
        element_form_default = self$element_form_default,
        attribute_form_default = self$attribute_form_default,
        target_namespace = self$target_namespace,
        target_namespace_prefix = self$target_namespace_prefix
      )
    },

    #' @description
    #' Deserialize a response object to an XML document.
    #' @param response The httr response object.
    #' @return An xml_document.
    deserialize_response = function(response) {
      soap_deserialize_response(response)
    }
  )
)
