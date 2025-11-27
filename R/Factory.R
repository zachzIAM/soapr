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

      return(obj)
    },

    get_type_names = function() {
      names(self$types)
    }
  )
)
