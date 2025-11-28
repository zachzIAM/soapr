#' Factory Class
#'
#' Creates objects for SOAP requests.
#'
#' @importFrom R6 R6Class
#' @export
Factory <- R6::R6Class(
  "Factory",
  public = list(
    wsdl = NULL,
    types = NULL,

    initialize = function(wsdl) {
      self$wsdl <- wsdl
      self$types <- wsdl$get_type_definitions()
    },

    create = function(type_name) {
      if (is.null(self$types[[type_name]])) {
        stop(paste("Type not found:", type_name))
      }

      type_def <- self$types[[type_name]]

      if (type_def$kind == "simple") {
        return(NULL) # Or default value from enum?
      }

      obj <- list()

      # Add attributes
      for (attr_name in names(type_def$attributes)) {
        # We can use a prefix or just document that these are attributes
        # For now, let's just add them as NA so they show up in the list
        obj[[attr_name]] <- NA
      }

      # Add elements
      for (elem_name in names(type_def$elements)) {
        elem_def <- type_def$elements[[elem_name]]

        # Check if it's a complex type that needs recursion
        # Be careful with infinite recursion if types refer to themselves
        # For now, we'll just set it to NA or a list if maxOccurs > 1

        if (!is.na(elem_def$maxOccurs) && elem_def$maxOccurs == "unbounded") {
          obj[[elem_name]] <- list()
        } else {
          obj[[elem_name]] <- NA
        }
      }

      # Add class and type attribute
      class(obj) <- c("soap_object", "list")
      attr(obj, "soap_type") <- type_name
      attr(obj, "soap_attrs") <- names(type_def$attributes)

      return(obj)
    },

    get_type_names = function() {
      return(names(self$types))
    },

    deserialize = function(node, type_name) {
      if (is.null(self$types[[type_name]])) {
        # If type not found, return text content or list of children
        if (length(xml2::xml_children(node)) == 0) {
          return(xml2::xml_text(node))
        } else {
          return(xml2::as_list(node))
        }
      }

      type_def <- self$types[[type_name]]

      if (type_def$kind == "simple") {
        # TODO: Handle type conversion (int, double, etc.) based on type_def$base
        return(xml2::xml_text(node))
      }

      # Complex Type
      obj <- list()

      # Attributes
      for (attr_name in names(type_def$attributes)) {
        attr_val <- xml2::xml_attr(node, attr_name)
        if (!is.na(attr_val)) {
          # TODO: Type conversion
          obj[[attr_name]] <- attr_val
        } else {
          obj[[attr_name]] <- NA
        }
      }

      # Elements
      for (elem_name in names(type_def$elements)) {
        elem_def <- type_def$elements[[elem_name]]

        # Find children with this name
        # Namespace handling is tricky. We'll try local name first.
        # xpath = paste0("./*[local-name()='", elem_name, "']")
        # children <- xml2::xml_find_all(node, xpath)

        # Simpler approach: iterate over all children and match names
        # This is faster than repeated xpath queries

        children <- xml2::xml_children(node)
        matching_children <- children[xml2::xml_name(children) == elem_name]

        if (length(matching_children) == 0) {
          if (!is.na(elem_def$maxOccurs) && elem_def$maxOccurs == "unbounded") {
            obj[[elem_name]] <- list()
          } else {
            obj[[elem_name]] <- NA
          }
          next
        }

        # Deserialize children
        child_vals <- lapply(matching_children, function(child) {
          self$deserialize(child, elem_def$type)
        })

        if (!is.na(elem_def$maxOccurs) && elem_def$maxOccurs == "unbounded") {
          obj[[elem_name]] <- child_vals
        } else {
          # If maxOccurs is 1, but we found multiple, what to do? Take first?
          # If maxOccurs is 1, we expect only 1.
          obj[[elem_name]] <- child_vals[[1]]
        }
      }

      class(obj) <- c("soap_object", "list")
      attr(obj, "soap_type") <- type_name
      attr(obj, "soap_attrs") <- names(type_def$attributes)

      return(obj)
    }
  )
)
