#' XML Serializer Class
#'
#' Handles serialization of R objects to XML nodes.
#'
#' @importFrom R6 R6Class
#' @importFrom xml2 xml_add_child xml_attr<- xml_set_text
#' @export
XMLSerializer <- R6::R6Class(
    "XMLSerializer",
    public = list(
        serialize = function(parent, name, value) {
            if (is.null(value)) {
                return()
            }

            # Skip NA values (scalar atomic NA)
            if (length(value) == 1 && is.atomic(value) && is.na(value)) {
                return()
            }

            if (inherits(value, "soap_object")) {
                # Complex Type
                # Create element node
                # Since we set the default namespace on the operation node, we don't need prefixes here
                node <- xml2::xml_add_child(parent, name)

                # Add attributes
                if (!is.null(attr(value, "soap_attrs"))) {
                    for (attr_name in attr(value, "soap_attrs")) {
                        attr_val <- value[[attr_name]]
                        if (!is.null(attr_val) && !is.na(attr_val)) {
                            xml2::xml_attr(node, attr_name) <- as.character(
                                attr_val
                            )
                        }
                    }
                }

                # Add elements
                # We need to iterate over names in the object that are NOT attributes
                soap_attrs <- attr(value, "soap_attrs")
                for (elem_name in names(value)) {
                    if (!elem_name %in% soap_attrs) {
                        self$serialize(node, elem_name, value[[elem_name]])
                    }
                }
            } else if (is.list(value) && !inherits(value, "soap_object")) {
                # List of items (unbounded)
                for (item in value) {
                    self$serialize(parent, name, item)
                }
            } else {
                # Simple Type
                # Since we set the default namespace on the operation node, we don't need prefixes here
                xml2::xml_add_child(parent, name, as.character(value))
            }
        }
    )
)
