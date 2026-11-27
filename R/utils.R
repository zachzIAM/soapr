is_xml_missing <- function(node) {
  inherits(node, "xml_missing")
}

soap_get_active_default_ns <- function(node) {
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

soap_add_namespaced_child <- function(parent, name, node_ns, text = NULL) {
  parent_ns <- soap_get_active_default_ns(parent)
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

soap_apply_soap_attrs <- function(
  node,
  value,
  attribute_form_default = "unqualified",
  target_namespace_prefix = "tns"
) {
  soap_attrs <- attr(value, "soap_attrs")
  if (is.null(soap_attrs)) {
    return(invisible(NULL))
  }
  for (attr_name in soap_attrs) {
    attr_val <- value[[attr_name]]
    if (!is.null(attr_val) && length(attr_val) > 0L && !is.null(attr_val[[1]]) && !is.na(attr_val[[1]])) {
      attr_str <- if (is.logical(attr_val)) tolower(as.character(attr_val)) else as.character(attr_val)
      full_attr_name <- if (identical(attribute_form_default, "qualified")) {
        paste0(target_namespace_prefix, ":", attr_name)
      } else {
        attr_name
      }
      xml2::xml_attr(node, full_attr_name) <- attr_str
    }
  }
  invisible(NULL)
}

soap_resolve_node_ns <- function(ns = NULL, element_form_default = "unqualified", target_namespace = NULL) {
  if (!is.null(ns)) {
    if (length(ns) == 1L && is.na(ns)) {
      return(NULL)
    }
    return(ns)
  }
  if (identical(element_form_default, "qualified")) {
    if (
      !is.null(target_namespace) &&
        length(target_namespace) == 1L &&
        !is.na(target_namespace) &&
        nzchar(target_namespace)
    ) {
      return(target_namespace)
    }
    return(NULL)
  }
  ""
}

soap_build_xml_node <- function(
  parent,
  name,
  value,
  ns = NULL,
  element_form_default = "unqualified",
  attribute_form_default = "unqualified",
  target_namespace = NULL,
  target_namespace_prefix = "tns"
) {
  if (is.null(value) || (is.atomic(value) && length(value) == 1L && is.na(value))) {
    return(invisible(NULL))
  }

  # Unnamed lists represent repeated elements (maxOccurs="unbounded")
  if (
    is.list(value) &&
      !inherits(value, "soap_object") &&
      (is.null(names(value)) || all(names(value) == ""))
  ) {
    for (item in value) {
      soap_build_xml_node(
        parent,
        name,
        item,
        ns = ns,
        element_form_default = element_form_default,
        attribute_form_default = attribute_form_default,
        target_namespace = target_namespace,
        target_namespace_prefix = target_namespace_prefix
      )
    }
    return(invisible(NULL))
  }

  node_ns <- soap_resolve_node_ns(
    ns = ns,
    element_form_default = element_form_default,
    target_namespace = target_namespace
  )

  if (is.list(value)) {
    child <- soap_add_namespaced_child(parent, name, node_ns)
    soap_apply_soap_attrs(child, value, attribute_form_default, target_namespace_prefix)
    soap_attrs <- attr(value, "soap_attrs")

    for (key in names(value)) {
      if (!key %in% soap_attrs) {
        soap_build_xml_node(
          child,
          key,
          value[[key]],
          ns = NULL,
          element_form_default = element_form_default,
          attribute_form_default = attribute_form_default,
          target_namespace = target_namespace,
          target_namespace_prefix = target_namespace_prefix
        )
      }
    }
  } else {
    val_str <- if (is.logical(value)) tolower(as.character(value)) else as.character(value)
    soap_add_namespaced_child(parent, name, node_ns, text = val_str)
  }
  invisible(NULL)
}

soap_get_header_ci <- function(headers, target_name) {
  if (is.null(headers) || length(headers) == 0L) {
    return(NULL)
  }
  hdr_names <- names(headers)
  if (is.null(hdr_names)) {
    return(NULL)
  }
  idx <- which(tolower(hdr_names) == tolower(target_name))
  if (length(idx) == 0L) {
    return(NULL)
  }
  headers[[idx[1]]]
}

soap_deserialize_response <- function(response) {
  content_type <- soap_get_header_ci(httr::headers(response), "Content-Type")
  if (
    !is.null(content_type) &&
      length(content_type) > 0L &&
      !is.na(content_type[[1]]) &&
      nzchar(trimws(content_type[[1]])) &&
      !grepl("xml", content_type[[1]], ignore.case = TRUE)
  ) {
    stop("Response is not XML")
  }

  body_text <- httr::content(response, "text", encoding = "UTF-8")
  if (is.null(body_text) || length(body_text) == 0L || is.na(body_text[[1]]) || !nzchar(trimws(body_text[[1]]))) {
    return(NULL)
  }

  xml2::read_xml(body_text[[1]])
}

soap_handle_fault <- function(response, deserialize_fn = soap_deserialize_response) {
  doc <- tryCatch(
    deserialize_fn(response),
    error = function(e) NULL
  )

  if (is.null(doc)) {
    httr::stop_for_status(response)
    return(invisible(NULL))
  }

  ns <- xml2::xml_ns(doc)

  fault_node <- tryCatch(
    xml2::xml_find_first(doc, ".//soap:Fault", ns),
    warning = function(w) xml2::xml_missing()
  )

  if (is_xml_missing(fault_node)) {
    fault_node <- tryCatch(
      xml2::xml_find_first(doc, ".//env:Fault", ns),
      warning = function(w) xml2::xml_missing()
    )
  }

  if (is_xml_missing(fault_node)) {
    fault_node <- xml2::xml_find_first(doc, ".//*[local-name()='Fault']")
  }

  if (!is_xml_missing(fault_node)) {
    fault_code <- xml2::xml_text(xml2::xml_find_first(
      fault_node,
      ".//*[local-name()='faultcode' or local-name()='Code']"
    ))
    fault_string <- xml2::xml_text(xml2::xml_find_first(
      fault_node,
      ".//*[local-name()='faultstring' or local-name()='Reason']"
    ))
    fault_detail <- xml2::xml_text(xml2::xml_find_first(
      fault_node,
      ".//*[local-name()='detail' or local-name()='Detail']"
    ))

    msg <- sprintf("SOAP Fault: [%s] %s", fault_code, fault_string)
    if (!is.na(fault_detail) && nchar(fault_detail) > 0L) {
      msg <- paste0(msg, "\nDetail: ", fault_detail)
    }
    stop(msg, call. = FALSE)
  }

  httr::stop_for_status(response)
  invisible(NULL)
}

soap_parse_xsd_date <- function(val) {
  if (is.null(val) || length(val) == 0L) {
    return(as.Date(NA))
  }
  if (inherits(val, "Date")) {
    return(val)
  }
  vals <- as.character(val)
  parsed <- vapply(
    vals,
    function(s) {
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
    },
    numeric(1),
    USE.NAMES = FALSE
  )
  as.Date(parsed, origin = "1970-01-01")
}

soap_parse_xsd_datetime <- function(val) {
  if (is.null(val) || length(val) == 0L) {
    return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"))
  }
  if (inherits(val, "POSIXt")) {
    out <- as.POSIXct(val)
    attr(out, "tzone") <- "UTC"
    return(out)
  }
  vals <- as.character(val)
  parsed <- vapply(
    vals,
    function(s) {
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
    },
    numeric(1),
    USE.NAMES = FALSE
  )
  as.POSIXct(parsed, origin = "1970-01-01", tz = "UTC")
}

soap_convert_builtin_type <- function(val, type_name) {
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

  switch(
    local_type,
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
    "date" = soap_parse_xsd_date(val),
    "dateTime" = soap_parse_xsd_datetime(val),
    val
  )
}

soap_deserialize_dynamic <- function(
  node,
  type_name,
  schema,
  instantiate_complex = NULL,
  instantiate_leaf = NULL
) {
  children <- xml2::xml_children(node)

  if (length(children) == 0L) {
    val <- xml2::xml_text(node)
    converted <- soap_convert_builtin_type(val, type_name)
    if (!is.null(instantiate_leaf)) {
      parsed <- tryCatch(instantiate_leaf(type_name, converted), error = function(e) NULL)
      if (!is.null(parsed)) {
        return(parsed)
      }
    }
    return(converted)
  }

  res <- list()
  for (child in children) {
    child_name <- xml2::xml_name(child)
    local_child_name <- gsub("^.*:", "", child_name)

    child_val <- soap_deserialize(
      child,
      local_child_name,
      schema,
      instantiate_complex = instantiate_complex,
      instantiate_leaf = instantiate_leaf
    )

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

  if (!is.null(instantiate_complex)) {
    parsed <- tryCatch(instantiate_complex(type_name, res), error = function(e) NULL)
    if (!is.null(parsed) && inherits(parsed, "soap_object")) {
      return(parsed)
    }
  }

  class(res) <- c("soap_object", "list")
  attr(res, "soap_type") <- type_name
  res
}

soap_deserialize <- function(
  node,
  type_name,
  schema,
  instantiate_complex = NULL,
  instantiate_leaf = NULL
) {
  if (is.null(schema[[type_name]])) {
    if (length(xml2::xml_children(node)) == 0L) {
      val <- xml2::xml_text(node)
      return(soap_convert_builtin_type(val, type_name))
    }
    return(soap_deserialize_dynamic(
      node,
      type_name,
      schema,
      instantiate_complex = instantiate_complex,
      instantiate_leaf = instantiate_leaf
    ))
  }

  type_def <- schema[[type_name]]

  if (type_def$kind == "simple") {
    val <- xml2::xml_text(node)
    base_type <- if (!is.null(type_def$base)) type_def$base else "xsd:string"
    return(soap_convert_builtin_type(val, base_type))
  }

  if (type_def$kind == "element") {
    if (!is.null(type_def$type) && type_def$type != type_name && type_def$type != "") {
      return(soap_deserialize(
        node,
        type_def$type,
        schema,
        instantiate_complex = instantiate_complex,
        instantiate_leaf = instantiate_leaf
      ))
    }
    return(soap_deserialize_dynamic(
      node,
      type_name,
      schema,
      instantiate_complex = instantiate_complex,
      instantiate_leaf = instantiate_leaf
    ))
  }

  obj <- list()

  for (attr_name in names(type_def$attributes)) {
    attr_val <- xml2::xml_attr(node, attr_name)
    if (!is.na(attr_val)) {
      attr_type <- type_def$attributes[[attr_name]]$type
      obj[[attr_name]] <- soap_convert_builtin_type(attr_val, attr_type)
    } else {
      obj[[attr_name]] <- NA
    }
  }

  children <- xml2::xml_children(node)
  child_names <- xml2::xml_name(children)
  for (elem_name in names(type_def$elements)) {
    elem_def <- type_def$elements[[elem_name]]
    matching_children <- children[child_names == elem_name]

    if (length(matching_children) == 0L) {
      if (!is.na(elem_def$maxOccurs) && elem_def$maxOccurs == "unbounded") {
        obj[[elem_name]] <- list()
      } else {
        obj[[elem_name]] <- NA
      }
      next
    }

    child_vals <- lapply(matching_children, function(child) {
      soap_deserialize(
        child,
        elem_def$type,
        schema,
        instantiate_complex = instantiate_complex,
        instantiate_leaf = instantiate_leaf
      )
    })

    if (!is.na(elem_def$maxOccurs) && elem_def$maxOccurs == "unbounded") {
      obj[[elem_name]] <- child_vals
    } else {
      obj[[elem_name]] <- child_vals[[1]]
    }
  }

  if (!is.null(instantiate_complex)) {
    parsed <- tryCatch(instantiate_complex(type_name, obj), error = function(e) NULL)
    if (!is.null(parsed) && inherits(parsed, "soap_object")) {
      return(parsed)
    }
  }

  class(obj) <- c("soap_object", "list")
  attr(obj, "soap_type") <- type_name
  attr(obj, "soap_attrs") <- names(type_def$attributes)
  obj
}

soap_extract_response_object <- function(content, schema, deserialize_fn) {
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
    return(deserialize_fn(response_node, response_name))
  }
  if (!is.null(schema[[local_name]])) {
    return(deserialize_fn(response_node, local_name))
  }

  content
}
