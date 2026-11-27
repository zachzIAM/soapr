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
        if (length(xml2::xml_children(node)) == 0) {
          val <- xml2::xml_text(node)
          return(self$convert_builtin_type(val, type_name))
        } else {
          return(self$deserialize_dynamic(node, type_name))
        }
      }

      type_def <- self$types[[type_name]]

      if (type_def$kind == "simple") {
        val <- xml2::xml_text(node)
        base_type <- if (!is.null(type_def$base)) {
          type_def$base
        } else {
          "xsd:string"
        }
        return(self$convert_builtin_type(val, base_type))
      }

      if (type_def$kind == "element") {
        if (!is.null(type_def$type) && type_def$type != type_name && type_def$type != "") {
          return(self$deserialize(node, type_def$type))
        }
        return(self$deserialize_dynamic(node, type_name))
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
          obj[[elem_name]] <- child_vals[[1]]
        }
      }

      parsed <- tryCatch({
        do.call(self$create, c(list(type_name = type_name), obj))
      }, error = function(e) NULL)

      if (!is.null(parsed) && inherits(parsed, "soap_object")) {
        return(parsed)
      }

      class(obj) <- c("soap_object", "list")
      attr(obj, "soap_type") <- type_name
      attr(obj, "soap_attrs") <- names(type_def$attributes)

      return(obj)
    },

    #' @description
    #' Dynamically deserialize unknown structures
    #' @param node The xml node
    #' @param type_name The requested type name
    deserialize_dynamic = function(node, type_name) {
      children <- xml2::xml_children(node)

      if (length(children) == 0) {
        val <- xml2::xml_text(node)
        if (!is.null(self$types[[type_name]])) {
          parsed <- tryCatch({
            do.call(self$create, list(type_name = type_name, val))
          }, error = function(e) NULL)
          if (!is.null(parsed)) return(parsed)
        }
        return(self$convert_builtin_type(val, type_name))
      }

      res <- list()
      for (child in children) {
        child_name <- xml2::xml_name(child)
        local_child_name <- gsub("^.*:", "", child_name)

        child_val <- self$deserialize(child, local_child_name)

        if (!local_child_name %in% names(res)) {
          res[[local_child_name]] <- child_val
        } else {
          current <- res[[local_child_name]]
          if (inherits(current, "soap_object") || is.atomic(current)) {
            res[[local_child_name]] <- list(current, child_val)
          } else {
            res[[local_child_name]][[length(current) + 1]] <- child_val
          }
        }
      }

      attrs <- xml2::xml_attrs(node)
      if (length(attrs) > 0) {
        attributes(res) <- c(attributes(res), as.list(attrs))
      }

      if (!is.null(self$types[[type_name]])) {
        parsed <- tryCatch({
          do.call(self$create, c(list(type_name = type_name), res))
        }, error = function(e) NULL)

        if (!is.null(parsed) && inherits(parsed, "soap_object")) {
          return(parsed)
        }
      }

      class(res) <- c("soap_object", "list")
      attr(res, "soap_type") <- type_name
      return(res)
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
        primitive_types <- c(
          "int", "integer", "long", "short", "byte",
          "decimal", "float", "double", "boolean", "string",
          "date", "dateTime"
        )
        if (local_type %in% primitive_types) {
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
          if (is.null(val) || length(val) == 0) {
            NA
          } else if (is.logical(val)) {
            val
          } else {
            val_chr <- tolower(trimws(as.character(val)))
            res <- rep(FALSE, length(val_chr))
            res[is.na(val)] <- NA
            res[!is.na(val) & (val_chr == "true" | val_chr == "1")] <- TRUE
            res
          }
        },
        "string" = as.character(val),
        "date" = private$parse_xsd_date(val),
        "dateTime" = private$parse_xsd_datetime(val),
        val # Default: return as is
      )
    }
  ),
  private = list(
    parse_xsd_date = function(val) {
      if (is.null(val) || length(val) == 0) {
        return(as.Date(NA))
      }
      if (inherits(val, "Date")) {
        return(val)
      }
      vals <- as.character(val)
      parsed <- vapply(vals, function(s) {
        if (is.na(s)) {
          return(NA_real_)
        }
        s <- trimws(s)
        if (!nzchar(s)) {
          return(NA_real_)
        }
        s <- sub("(Z|[+-][0-9]{2}:?[0-9]{2})$", "", s, ignore.case = TRUE)
        d <- tryCatch(as.Date(s, format = "%Y-%m-%d"), error = function(e) as.Date(NA))
        as.numeric(d)
      }, numeric(1), USE.NAMES = FALSE)
      as.Date(parsed, origin = "1970-01-01")
    },

    parse_xsd_datetime = function(val) {
      if (is.null(val) || length(val) == 0) {
        return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"))
      }
      if (inherits(val, "POSIXt")) {
        out <- as.POSIXct(val)
        attr(out, "tzone") <- "UTC"
        return(out)
      }
      vals <- as.character(val)
      parsed <- vapply(vals, function(s) {
        if (is.na(s)) {
          return(NA_real_)
        }
        s <- trimws(s)
        if (!nzchar(s)) {
          return(NA_real_)
        }
        offset_sec <- 0
        if (grepl("[Zz]$", s)) {
          s <- sub("[Zz]$", "", s)
        } else if (grepl("([+-])([0-9]{2}):?([0-9]{2})$", s)) {
          m <- regexec("([+-])([0-9]{2}):?([0-9]{2})$", s)
          parts <- regmatches(s, m)[[1]]
          sign <- if (parts[2] == "-") -1 else 1
          offset_sec <- sign * (as.numeric(parts[3]) * 3600 + as.numeric(parts[4]) * 60)
          s <- sub("([+-])([0-9]{2}):?([0-9]{2})$", "", s)
        }
        dt <- as.POSIXct(s, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
        if (is.na(dt)) {
          dt <- as.POSIXct(s, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
        }
        if (is.na(dt)) {
          return(NA_real_)
        }
        as.numeric(dt) - offset_sec
      }, numeric(1), USE.NAMES = FALSE)
      as.POSIXct(parsed, origin = "1970-01-01", tz = "UTC")
    }
  )
)
