build_xml_node <- function(parent, name, value) {
  if (
    is.null(value) ||
      (is.atomic(value) && length(value) == 1 && is.na(value))
  ) {
    return()
  }

  # Handle lists (maxOccurs="unbounded")
  if (is.list(value) && is.null(names(value))) {
    for (item in value) {
      build_xml_node(parent, name, item)
    }
    return()
  }

  # Create node
  # Create node
  # Handle namespaces based on elementFormDefault
  element_form_default <- "{{{element_form_default}}}"

  if (element_form_default == "qualified") {
    node_name <- if (grepl(":", name)) name else paste0("tns:", name)
    child <- xml2::xml_add_child(parent, node_name)
  } else {
    # Unqualified
    # If parent has a default namespace, we must explicitly unset it for the child
    # to ensure it is in no namespace.
    node_name <- name
    child <- xml2::xml_add_child(parent, node_name)

    # Check if parent has default namespace
    parent_ns <- xml2::xml_attr(parent, "xmlns")
    if (!is.na(parent_ns) && parent_ns != "") {
      xml2::xml_attr(child, "xmlns") <- ""
    }
  }

  if (is.list(value)) {
    soap_attrs <- attr(value, "soap_attrs")

    for (key in names(value)) {
      val <- value[[key]]
      if (!is.null(soap_attrs) && key %in% soap_attrs) {
        if (!is.null(val) && !is.na(val)) {
          xml2::xml_set_attr(child, key, as.character(val))
        }
      } else {
        build_xml_node(child, key, val)
      }
    }
  } else {
    xml2::xml_set_text(child, as.character(value))
  }
}

handle_fault <- function(response) {
  # Try to parse XML
  doc <- tryCatch(
    {
      content_type <- httr::headers(response)$`Content-Type`
      if (
        !is.null(content_type) &&
          !grepl("xml", content_type, ignore.case = TRUE)
      ) {
        NULL
      } else {
        xml2::read_xml(httr::content(
          response,
          "text",
          encoding = "UTF-8"
        ))
      }
    },
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

convert_builtin_type <- function(val, type_name) {
  if (is.list(val)) {
    return(val)
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

deserialize <- function(node, type_name, schema) {
  if (is.null(schema[[type_name]])) {
    # Check for built-in XSD types
    val <- if (length(xml2::xml_children(node)) == 0) {
      xml2::xml_text(node)
    } else {
      xml2::as_list(node)
    }

    return(convert_builtin_type(val, type_name))
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
      deserialize(child, elem_def$type, schema)
    })

    if (!is.na(elem_def$maxOccurs) && elem_def$maxOccurs == "unbounded") {
      obj[[elem_name]] <- child_vals
    } else {
      obj[[elem_name]] <- child_vals[[1]]
    }
  }

  class(obj) <- c("soap_object", "list")
  attr(obj, "soap_type") <- type_name
  attr(obj, "soap_attrs") <- names(type_def$attributes)

  return(obj)
}

is_xml_missing <- function(node) {
  return(inherits(node, "xml_missing"))
}
