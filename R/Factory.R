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
        # Check if we have a default value in the definition (rare for simpleType defs themselves, usually on element/attr)
        # But for Enums (restrictions), we might just return NA or the first allowed value?
        # Returning NA is safer to indicate "not set".
        # We can use convert_builtin_type to get the NA of correct type (e.g. integer)
        base <- type_def$base %||% "xsd:string"
        return(self$convert_builtin_type(NA, base))
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

      # Add class and type attribute
      class(obj) <- c("soap_object", "list")
      attr(obj, "soap_type") <- type_name
      attr(obj, "soap_attrs") <- names(type_def$attributes)

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
      if (is.null(self$types[[type_name]])) {
        # Check for built-in XSD types
        val <- if (length(xml2::xml_children(node)) == 0) {
          xml2::xml_text(node)
        } else {
          xml2::as_list(node)
        }

        return(self$convert_builtin_type(val, type_name))
      }

      type_def <- self$types[[type_name]]

      if (type_def$kind == "simple") {
        val <- xml2::xml_text(node)
        # Use the base type for conversion if available
        base_type <- if (!is.null(type_def$base)) {
          type_def$base
        } else {
          "xsd:string"
        }
        return(self$convert_builtin_type(val, base_type))
      }

      # Complex Type
      obj <- list()

      # Attributes
      for (attr_name in names(type_def$attributes)) {
        attr_val <- xml2::xml_attr(node, attr_name)
        if (!is.na(attr_val)) {
          attr_type <- type_def$attributes[[attr_name]]$type
          obj[[attr_name]] <- self$convert_builtin_type(attr_val, attr_type)
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
    },

    #' @description
    #' Convert a value to a built-in R type.
    #' @param val The value to convert.
    #' @param type_name The XSD type name.
    #' @return The converted value.
    convert_builtin_type = function(val, type_name) {
      if (is.list(val)) {
        # If it's a list and we expect a primitive, try to unlist
        local_type <- gsub("^.*:", "", type_name)
        if (local_type %in% c("int", "integer", "long", "short", "byte", "decimal", "float", "double", "boolean", "string")) {
          val <- unlist(val)
        } else {
          # Complex structure or unknown type
          return(val)
        }
      }

      # Strip namespace prefix if present
      local_type <- gsub("^.*:", "", type_name)

      switch(local_type,
        "int" = as.integer(val),
        "integer" = as.integer(val),
        "long" = bit64::as.integer64(val),
        "short" = as.integer(val),
        "byte" = as.integer(val),
        "decimal" = as.numeric(val),
        "float" = as.numeric(val),
        "double" = as.numeric(val),
        "boolean" = {
          if (tolower(val) == "true" || val == "1") TRUE else FALSE
        },
        "string" = as.character(val),
        val # Default: return as is
      )
    }
  )
)
