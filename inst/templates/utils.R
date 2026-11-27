get_active_default_ns <- function(node) {
  cur <- node
  while (inherits(cur, "xml_node") && xml2::xml_type(cur) == "element") {
    ns_attr <- xml2::xml_attr(cur, "xmlns")
    if (!is.na(ns_attr)) {
      return(ns_attr)
    }
    cur <- xml2::xml_parent(cur)
  }
  ""
}

add_namespaced_child <- function(parent, name, node_ns, text = NULL) {
  parent_ns <- get_active_default_ns(parent)
  needs_xmlns <- !is.null(node_ns) && length(node_ns) == 1L && !is.na(node_ns) && !identical(node_ns, parent_ns)

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
}

build_xml_node <- function(parent, name, value, ns = NULL) {
  if (
    is.null(value) ||
      (is.atomic(value) && length(value) == 1 && is.na(value))
  ) {
    return()
  }

  # Handle unnamed lists (maxOccurs="unbounded")
  if (
    is.list(value) &&
      !inherits(value, "soap_object") &&
      (is.null(names(value)) || all(names(value) == ""))
  ) {
    for (item in value) {
      build_xml_node(parent, name, item, ns = ns)
    }
    return()
  }

  # Handle namespaces based on elementFormDefault and attributeFormDefault
  element_form_default <- "{{{element_form_default}}}"
  attribute_form_default <- "{{{attribute_form_default}}}"
  target_namespace <- "{{{target_namespace}}}"

  node_ns <- ns
  if (!is.null(node_ns) && length(node_ns) == 1L && is.na(node_ns)) {
    node_ns <- NULL
  } else if (is.null(node_ns)) {
    if (identical(element_form_default, "qualified")) {
      node_ns <- if (!is.na(target_namespace) && nzchar(target_namespace)) target_namespace else NULL
    } else {
      node_ns <- ""
    }
  }

  if (is.list(value)) {
    simple_field <- attr(value, "soap_simple_content")
    if (!is.null(simple_field) && length(simple_field) == 1L && !is.na(simple_field)) {
      text_val <- value[[simple_field]]
      text_str <- if (
        !is.null(text_val) &&
          length(text_val) > 0L &&
          !(is.atomic(text_val) && length(text_val) == 1L && is.na(text_val))
      ) {
        if (is.logical(text_val)) tolower(as.character(text_val)) else as.character(text_val)
      } else {
        NULL
      }
      child <- add_namespaced_child(parent, name, node_ns, text = text_str)
      soap_attrs <- attr(value, "soap_attrs")
      if (!is.null(soap_attrs)) {
        for (attr_name in soap_attrs) {
          val <- value[[attr_name]]
          if (!is.null(val) && length(val) > 0 && !is.null(val[[1]]) && !is.na(val[[1]])) {
            attr_str <- if (is.logical(val)) tolower(as.character(val)) else as.character(val)
            full_attr_name <- if (identical(attribute_form_default, "qualified")) {
              paste0("tns:", attr_name)
            } else {
              attr_name
            }
            xml2::xml_attr(child, full_attr_name) <- attr_str
          }
        }
      }
      return()
    }

    child <- add_namespaced_child(parent, name, node_ns)
    soap_attrs <- attr(value, "soap_attrs")

    if (!is.null(soap_attrs)) {
      for (attr_name in soap_attrs) {
        val <- value[[attr_name]]
        if (!is.null(val) && length(val) > 0 && !is.null(val[[1]]) && !is.na(val[[1]])) {
          attr_str <- if (is.logical(val)) tolower(as.character(val)) else as.character(val)
          full_attr_name <- if (identical(attribute_form_default, "qualified")) {
            paste0("tns:", attr_name)
          } else {
            attr_name
          }
          xml2::xml_attr(child, full_attr_name) <- attr_str
        }
      }
    }

    for (key in names(value)) {
      if (!key %in% soap_attrs) {
        build_xml_node(child, key, value[[key]])
      }
    }
  } else {
    val_str <- if (is.logical(value)) tolower(as.character(value)) else as.character(value)
    add_namespaced_child(parent, name, node_ns, text = val_str)
  }
}

get_header_ci <- function(headers, target_name) {
  if (is.null(headers) || length(headers) == 0) {
    return(NULL)
  }
  hdr_names <- names(headers)
  if (is.null(hdr_names)) {
    return(NULL)
  }
  idx <- which(tolower(hdr_names) == tolower(target_name))
  if (length(idx) == 0) {
    return(NULL)
  }
  headers[[idx[1]]]
}

deserialize_response <- function(response) {
  content_type <- get_header_ci(httr::headers(response), "Content-Type")
  if (
    !is.null(content_type) &&
      length(content_type) > 0 &&
      !is.na(content_type[[1]]) &&
      nzchar(trimws(content_type[[1]])) &&
      !grepl("xml", content_type[[1]], ignore.case = TRUE)
  ) {
    stop("Response is not XML")
  }

  body_text <- httr::content(response, "text", encoding = "UTF-8")
  if (is.null(body_text) || length(body_text) == 0 || is.na(body_text[[1]]) || !nzchar(trimws(body_text[[1]]))) {
    return(NULL)
  }

  xml2::read_xml(body_text[[1]])
}

handle_fault <- function(response) {
  # Try to parse XML
  doc <- tryCatch(
    deserialize_response(response),
    error = function(e) NULL
  )

  if (is.null(doc)) {
    httr::stop_for_status(response)
    return()
  }

  ns <- xml2::xml_ns(doc)

  # Check for SOAP 1.1 Fault
  fault_node <- tryCatch(
    xml2::xml_find_first(doc, ".//soap:Fault", ns),
    warning = function(w) xml2::xml_missing()
  )

  # Check for SOAP 1.2 Fault
  if (is_xml_missing(fault_node)) {
    fault_node <- tryCatch(
      xml2::xml_find_first(doc, ".//env:Fault", ns),
      warning = function(w) xml2::xml_missing()
    )
  }

  if (is_xml_missing(fault_node)) {
    # Fallback for when prefix is different but local name is Fault
    fault_node <- xml2::xml_find_first(doc, ".//*[local-name()='Fault']")
  }

  if (!is_xml_missing(fault_node)) {
    # Extract Fault Code
    fault_code <- xml2::xml_text(xml2::xml_find_first(
      fault_node,
      ".//*[local-name()='faultcode' or local-name()='Code']"
    ))

    # Extract Fault String/Reason
    fault_string <- xml2::xml_text(xml2::xml_find_first(
      fault_node,
      ".//*[local-name()='faultstring' or local-name()=\'Reason\']"
    ))

    # Extract Detail
    fault_detail <- xml2::xml_text(xml2::xml_find_first(
      fault_node,
      ".//*[local-name()='detail' or local-name()='Detail\']"
    ))

    msg <- sprintf("SOAP Fault: [%s] %s", fault_code, fault_string)
    if (!is.na(fault_detail) && nchar(fault_detail) > 0) {
      msg <- paste0(msg, "\nDetail: ", fault_detail)
    }
    stop(msg, call. = FALSE)
  }

  # If no fault found, let httr handle it
  httr::stop_for_status(response)
}

parse_xsd_date <- function(val) {
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
}

parse_xsd_datetime <- function(val) {
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

convert_builtin_type <- function(val, type_name) {
  local_type <- gsub("^.*:", "", type_name)
  if (is.list(val)) {
    primitive_types <- c(
      "int", "integer", "long", "short", "byte",
      "decimal", "float", "double", "boolean", "string",
      "date", "dateTime"
    )
    if (local_type %in% primitive_types) {
      val <- unlist(val)
    } else {
      return(val)
    }
  }

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
      if (is.null(val) || length(val) == 0L) {
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
    "date" = parse_xsd_date(val),
    "dateTime" = parse_xsd_datetime(val),
    val # Default: return as is
  )
}

deserialize_dynamic <- function(node, type_name, schema, factory) {
  children <- xml2::xml_children(node)

  if (length(children) == 0L) {
    val <- xml2::xml_text(node)
    converted <- convert_builtin_type(val, type_name)

    if (!is.null(factory) && type_name %in% names(factory)) {
      parsed <- tryCatch({
        arg_names <- names(formals(factory[[type_name]]))
        if (length(arg_names) > 0L) {
          args <- list()
          args[[arg_names[1]]] <- converted
          do.call(factory[[type_name]], args)
        } else {
          NULL
        }
      }, error = function(e) NULL)
      if (!is.null(parsed)) return(parsed)
    }

    return(converted)
  }

  res <- list()
  for (child in children) {
    child_name <- xml2::xml_name(child)
    local_child_name <- gsub("^.*:", "", child_name)

    # dynamic parse
    child_val <- deserialize(child, local_child_name, schema, factory)

    if (!local_child_name %in% names(res)) {
      res[[local_child_name]] <- child_val
    } else {
      current <- res[[local_child_name]]
      if (inherits(current, "soap_object") || is.atomic(current)) {
        res[[local_child_name]] <- list(current, child_val)
      } else {
        res[[local_child_name]][[length(current) + 1L]] <- child_val
      }
    }
  }

  attrs <- xml2::xml_attrs(node)
  if (length(attrs) > 0L) {
    attributes(res) <- c(attributes(res), as.list(attrs))
  }

  if (!is.null(factory) && type_name %in% names(factory) &&
        !is.null(schema[[type_name]]) && identical(schema[[type_name]]$kind, "complex")) {
    parsed <- tryCatch({
      do.call(factory[[type_name]], res)
    }, error = function(e) NULL)

    if (!is.null(parsed) && inherits(parsed, "soap_object")) {
      return(parsed)
    }
  }

  class(res) <- c("soap_object", "list")
  attr(res, "soap_type") <- type_name
  return(res)
}

deserialize <- function(node, type_name, schema, factory = NULL) {
  if (is.null(schema[[type_name]])) {
    # Check for built-in XSD types
    if (length(xml2::xml_children(node)) == 0) {
      val <- xml2::xml_text(node)
      return(convert_builtin_type(val, type_name))
    } else {
      return(deserialize_dynamic(node, type_name, schema, factory))
    }
  }

  type_def <- schema[[type_name]]

  if (type_def$kind == "simple") {
    val <- xml2::xml_text(node)
    base_type <- if (!is.null(type_def$base)) {
      type_def$base
    } else {
      "xsd:string"
    }
    return(convert_builtin_type(val, base_type))
  }

  if (type_def$kind == "element") {
    if (!is.null(type_def$type) && type_def$type != type_name && type_def$type != "") {
      return(deserialize(node, type_def$type, schema, factory))
    }

    return(deserialize_dynamic(node, type_name, schema, factory))
  }

  # Complex Type
  obj <- list()

  # Attributes
  for (attr_name in names(type_def$attributes)) {
    attr_val <- xml2::xml_attr(node, attr_name)
    if (!is.na(attr_val)) {
      attr_type <- type_def$attributes[[attr_name]]$type
      obj[[attr_name]] <- convert_builtin_type(attr_val, attr_type)
    } else {
      obj[[attr_name]] <- NA
    }
  }

  if (isTRUE(type_def$simple_content)) {
    val_field <- if (!is.null(type_def$value_field)) type_def$value_field else "value"
    raw_text <- xml2::xml_text(node)
    base_type <- if (!is.null(type_def$base)) type_def$base else "xsd:string"
    if (!is.null(schema[[base_type]]) && identical(schema[[base_type]]$kind, "simple")) {
      base_type <- if (!is.null(schema[[base_type]]$base)) schema[[base_type]]$base else "xsd:string"
    }
    if (!nzchar(trimws(raw_text)) && !identical(gsub("^.*:", "", base_type), "string")) {
      raw_text <- NA_character_
    }
    obj[[val_field]] <- convert_builtin_type(raw_text, base_type)
  }

  # Elements
  for (elem_name in names(type_def$elements)) {
    elem_def <- type_def$elements[[elem_name]]

    children <- xml2::xml_children(node)
    matching_children <- children[xml2::xml_name(children) == elem_name]

    if (length(matching_children) == 0) {
      if (
        !is.na(elem_def$maxOccurs) && elem_def$maxOccurs == "unbounded"
      ) {
        obj[[elem_name]] <- list()
      } else {
        obj[[elem_name]] <- NA
      }
      next
    }

    # Deserialize children
    child_vals <- lapply(matching_children, function(child) {
      deserialize(child, elem_def$type, schema, factory)
    })

    if (!is.na(elem_def$maxOccurs) && elem_def$maxOccurs == "unbounded") {
      obj[[elem_name]] <- child_vals
    } else {
      obj[[elem_name]] <- child_vals[[1]]
    }
  }

  if (!is.null(factory) && type_name %in% names(factory)) {
    parsed <- tryCatch({
      do.call(factory[[type_name]], obj)
    }, error = function(e) NULL)

    if (!is.null(parsed) && inherits(parsed, "soap_object")) {
      return(parsed)
    }
  }

  class(obj) <- c("soap_object", "list")
  attr(obj, "soap_type") <- type_name
  attr(obj, "soap_attrs") <- names(type_def$attributes)
  if (isTRUE(type_def$simple_content)) {
    attr(obj, "soap_simple_content") <- if (!is.null(type_def$value_field)) type_def$value_field else "value"
  }

  return(obj)
}

extract_response_object <- function(content, schema, factory) {
  if (!inherits(content, "xml_node")) {
    return(NULL)
  }

  body_node <- xml2::xml_find_first(content, ".//*[local-name()='Body']")
  if (is_xml_missing(body_node)) {
    return(NULL)
  }

  body_children <- xml2::xml_children(body_node)
  if (length(body_children) == 0L) {
    return(NULL)
  }

  response_node <- body_children[[1]]
  response_name <- xml2::xml_name(response_node)
  local_name <- gsub("^.*:", "", response_name)

  if (!is.null(schema[[response_name]])) {
    return(deserialize(response_node, response_name, schema, factory))
  }
  if (!is.null(schema[[local_name]])) {
    return(deserialize(response_node, local_name, schema, factory))
  }

  content
}

is_xml_missing <- function(node) {
  return(inherits(node, "xml_missing"))
}

get_soap_envelope_ns <- function(soap_version = "1.1") {
  if (identical(soap_version, "1.2")) {
    "http://www.w3.org/2003/05/soap-envelope"
  } else {
    "http://schemas.xmlsoap.org/soap/envelope/"
  }
}

build_soap_content_type <- function(action = "", soap_version = "1.1") {
  if (identical(soap_version, "1.2")) {
    if (!is.null(action) && length(action) == 1L && !is.na(action) && nzchar(action)) {
      sprintf('application/soap+xml; charset=utf-8; action="%s"', action)
    } else {
      "application/soap+xml; charset=utf-8"
    }
  } else {
    "text/xml; charset=utf-8"
  }
}

build_transport_config <- function(config, action = "", soap_version = "1.1") {
  ct <- build_soap_content_type(action = action, soap_version = soap_version)
  req_config <- if (is.list(config)) config else list()
  extra_headers <- if (identical(soap_version, "1.2")) {
    c("Content-Type" = ct)
  } else {
    c("Content-Type" = ct, SOAPAction = if (is.null(action)) "" else action)
  }
  req_config$headers <- c(req_config$headers, extra_headers)
  req_config
}

check_soap_type <- function(obj, expected_type, arg_name) {
  if (is.list(obj) && !inherits(obj, "soap_object")) {
    for (i in seq_along(obj)) {
      check_soap_type(obj[[i]], expected_type, sprintf("%s[[%d]]", arg_name, i))
    }
    return(invisible(TRUE))
  }

  if (is.atomic(obj) && length(obj) == 1 && is.na(obj)) {
    return(invisible(TRUE))
  }

  if (!inherits(obj, "soap_object")) {
    stop(
      sprintf(
        "Argument '%s' must be an object of class 'soap_object', but got '%s'.",
        arg_name,
        class(obj)[1]
      ),
      call. = FALSE
    )
  }

  actual_type <- attr(obj, "soap_type")
  if (!identical(actual_type, expected_type)) {
    stop(
      sprintf(
        "Argument '%s' must be of SOAP type '%s', but got '%s'.",
        arg_name,
        expected_type,
        actual_type
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}
