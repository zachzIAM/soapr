#' Factory Class
#'
#' Creates objects for SOAP requests.
#'
#' @export
Factory <- R6::R6Class(
  "Factory",
  public = list(
    #' @field wsdl The WSDLParser instance.
    wsdl = NULL,
    #' @field types List of type definitions.
    types = NULL,

    #' @description
    #' Initialize the Factory.
    #' @param wsdl A WSDLParser instance.
    initialize = function(wsdl) {
      self$wsdl <- wsdl
      self$types <- wsdl$get_type_definitions()
    },

    #' @description
    #' Create a new object of a given type.
    #' @param type_name The name of the type to create.
    #' @param ... Named arguments to initialize the object fields.
    #' @return A list representing the object, or NA/default for simple type.
    create = function(type_name, ...) {
      args <- list(...)

      # Handle standard XSD types if requested directly
      if (grepl("^xsd:", type_name)) {
        return(self$convert_builtin_type(NA, type_name))
      }

      if (is.null(self$types[[type_name]])) {
        stop(paste("Type not found:", type_name))
      }

      type_def <- self$types[[type_name]]

      if (type_def$kind == "simple") {
        # Check if we have a default value in the definition
        # (rare for simpleType defs themselves, usually on element/attr)
        # But for Enums (restrictions), we might just return NA or the first allowed value?
        # Returning NA is safer to indicate "not set".
        # We can use convert_builtin_type to get the NA of correct type (e.g. integer)
        base <- type_def$base %||% "xsd:string"

        val <- if (length(args) > 0) args[[1]] else NA
        if (!is.null(names(args)) && !is.null(args[["value"]])) {
          val <- args[["value"]]
        }
        return(self$convert_builtin_type(val, base))
      }

      if (type_def$kind == "element") {
        arg_name <- type_def$name
        val <- if (length(args) > 0) args[[1]] else NA
        if (!is.null(names(args)) && !is.null(args[[arg_name]])) {
          val <- args[[arg_name]]
        }
        return(self$convert_builtin_type(val, type_def$type %||% "xsd:string"))
      }

      obj <- list()

      # Add attributes
      for (attr_name in names(type_def$attributes)) {
        attr_def <- type_def$attributes[[attr_name]]

        # Check initialization args
        if (!is.null(args[[attr_name]])) {
          obj[[attr_name]] <- args[[attr_name]]
        } else {
          # Check default
          # Use 'default' or 'fixed' attribute from XSD? WSDLParser might name it 'default'.
          default_val <- attr_def$default
          if (!is.null(default_val)) {
            # Convert default string to proper type
            obj[[attr_name]] <- self$convert_builtin_type(default_val, attr_def$type)
          } else {
            obj[[attr_name]] <- NA
          }
        }
      }

      # Add elements
      for (elem_name in names(type_def$elements)) {
        elem_def <- type_def$elements[[elem_name]]

        # Check initialization args
        if (!is.null(args[[elem_name]])) {
          obj[[elem_name]] <- args[[elem_name]]
        } else {
          # Check default
          default_val <- elem_def$default

          if (!is.null(default_val)) {
            obj[[elem_name]] <- self$convert_builtin_type(default_val, elem_def$type)
          } else if (!is.na(elem_def$maxOccurs) && elem_def$maxOccurs == "unbounded") {
            obj[[elem_name]] <- list()
          } else {
            obj[[elem_name]] <- NA
          }
        }
      }

      # Add text content field for simpleContent types
      if (isTRUE(type_def$simple_content)) {
        val_field <- if (!is.null(type_def$value_field)) type_def$value_field else "value"
        if (!is.null(args[[val_field]])) {
          obj[[val_field]] <- args[[val_field]]
        } else if (length(args) > 0L && (is.null(names(args)) || !nzchar(names(args)[1]))) {
          obj[[val_field]] <- args[[1]]
        } else {
          obj[[val_field]] <- NA
        }
      }

      # Add class and type attribute
      class(obj) <- c("soap_object", "list")
      attr(obj, "soap_type") <- type_name
      attr(obj, "soap_attrs") <- names(type_def$attributes)
      if (isTRUE(type_def$simple_content)) {
        attr(obj, "soap_simple_content") <- if (!is.null(type_def$value_field)) type_def$value_field else "value"
      }

      return(obj)
    },

    #' @description
    #' Convert a list to a SOAP object of a given type.
    #' @param data The list to convert.
    #' @param type_name The target type name.
    #' @return The converted soap_object.
    from_list = function(data, type_name) {
      if (is.null(self$types[[type_name]])) {
        stop(paste("Type not found:", type_name))
      }

      type_def <- self$types[[type_name]]

      if (type_def$kind == "simple") {
        # Assuming data is a scalar, just convert
        base <- type_def$base %||% "xsd:string"
        return(self$convert_builtin_type(data, base))
      }

      obj <- data

      # Ensure all defined fields exist (fill with defaults/NA if missing)
      # Or should we trust the input list?
      # A safe "cast" should probably ensure structure matches.

      # 1. Attributes
      for (attr_name in names(type_def$attributes)) {
        if (is.null(obj[[attr_name]])) {
          # Check default
          attr_def <- type_def$attributes[[attr_name]]
          default_val <- attr_def$default
          if (!is.null(default_val)) {
            obj[[attr_name]] <- self$convert_builtin_type(default_val, attr_def$type)
          } else {
            obj[[attr_name]] <- NA
          }
        }
      }

      # 2. Elements
      for (elem_name in names(type_def$elements)) {
        if (is.null(obj[[elem_name]])) {
          elem_def <- type_def$elements[[elem_name]]
          default_val <- elem_def$default

          if (!is.null(default_val)) {
            obj[[elem_name]] <- self$convert_builtin_type(default_val, elem_def$type)
          } else if (!is.na(elem_def$maxOccurs) && elem_def$maxOccurs == "unbounded") {
            obj[[elem_name]] <- list()
          } else {
            obj[[elem_name]] <- NA
          }
        }
      }

      if (isTRUE(type_def$simple_content)) {
        val_field <- if (!is.null(type_def$value_field)) type_def$value_field else "value"
        if (is.null(obj[[val_field]])) {
          obj[[val_field]] <- NA
        }
        attr(obj, "soap_simple_content") <- val_field
      }

      class(obj) <- c("soap_object", "list")
      attr(obj, "soap_type") <- type_name
      attr(obj, "soap_attrs") <- names(type_def$attributes)

      return(obj)
    },

    #' @description
    #' Get a list of available type names.
    #' @return Character vector of type names.
    get_type_names = function() {
      return(names(self$types))
    },

    #' @description
    #' Deserialize an XML node into an R object.
    #' @param node The xml_node to deserialize.
    #' @param type_name The expected type name.
    #' @return The deserialized R object.
    deserialize = function(node, type_name) {
      soap_deserialize(
        node = node,
        type_name = type_name,
        schema = self$types,
        instantiate_complex = function(t_name, obj) {
          if (!is.null(self$types[[t_name]]) && self$types[[t_name]]$kind == "complex") {
            return(do.call(self$create, c(list(type_name = t_name), obj)))
          }
          NULL
        },
        instantiate_leaf = function(t_name, converted) {
          if (!is.null(self$types[[t_name]])) {
            return(do.call(self$create, list(type_name = t_name, converted)))
          }
          NULL
        }
      )
    },

    #' @description
    #' Dynamically deserialize unknown structures
    #' @param node The xml node
    #' @param type_name The requested type name
    deserialize_dynamic = function(node, type_name) {
      soap_deserialize_dynamic(
        node = node,
        type_name = type_name,
        schema = self$types,
        instantiate_complex = function(t_name, obj) {
          if (!is.null(self$types[[t_name]]) && self$types[[t_name]]$kind == "complex") {
            return(do.call(self$create, c(list(type_name = t_name), obj)))
          }
          NULL
        },
        instantiate_leaf = function(t_name, converted) {
          if (!is.null(self$types[[t_name]])) {
            return(do.call(self$create, list(type_name = t_name, converted)))
          }
          NULL
        }
      )
    },

    #' @description
    #' Convert a value to a built-in R type.
    #' @param val The value to convert.
    #' @param type_name The XSD type name.
    #' @return The converted value.
    convert_builtin_type = function(val, type_name) {
      soap_convert_builtin_type(val, type_name)
    }
  )
)
