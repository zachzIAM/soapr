# code built using Claude

library(xml2)
library(httr)

# wsdl discovety ------------------------------------------------------------------------------

parse_definitions <- function(wsdl_doc) {
  root <- xml2::xml_root(wsdl_doc)

  # Get all namespace declarations
  namespaces <- xml2::xml_ns(root)

  # Get attributes
  target_namespace <- xml2::xml_attr(root, "targetNamespace")
  name <- xml2::xml_attr(root, "name")

  list(
    name = name,
    target_namespace = target_namespace,
    namespaces = namespaces,
    root = root
  )
}


#' Collect all message definitions
collect_messages <- function(wsdl_doc) {
  root <- xml2::xml_root(wsdl_doc)
  namespaces <- xml2::xml_ns(root)

  # Find all message elements
  message_nodes <- xml2::xml_find_all(root, ".//wsdl:message", namespaces)

  messages <- lapply(message_nodes, function(msg_node) {
    name <- xml2::xml_attr(msg_node, "name")

    # Get documentation if present
    doc_node <- xml2::xml_find_first(msg_node, "./wsdl:documentation", namespaces)
    documentation <- if (!is.na(doc_node)) xml2::xml_text(doc_node) else NULL

    # Collect all parts
    part_nodes <- xml2::xml_find_all(msg_node, "./wsdl:part", namespaces)
    parts <- lapply(part_nodes, function(part_node) {
      list(
        name = xml2::xml_attr(part_node, "name"),
        element = xml2::xml_attr(part_node, "element"),
        type = xml2::xml_attr(part_node, "type")
      )
    })

    list(
      name = name,
      documentation = documentation,
      parts = parts,
      node = msg_node
    )
  })

  # Name the list by message names for easy lookup
  names(messages) <- sapply(messages, function(m) m$name)
  messages
}


#' Collect all portType definitions
collect_port_types <- function(wsdl_doc) {
  root <- xml2::xml_root(wsdl_doc)
  namespaces <- xml2::xml_ns(root)

  # Find all portType elements
  porttype_nodes <- xml2::xml_find_all(root, ".//wsdl:portType", namespaces)

  port_types <- lapply(porttype_nodes, function(pt_node) {
    name <- xml2::xml_attr(pt_node, "name")

    # Get documentation if present
    doc_node <- xml2::xml_find_first(pt_node, "./wsdl:documentation", namespaces)
    documentation <- if (!is.na(doc_node)) xml2::xml_text(doc_node) else NULL

    # Collect all operations
    op_nodes <- xml2::xml_find_all(pt_node, "./wsdl:operation", namespaces)
    operations <- lapply(op_nodes, function(op_node) {
      op_name <- xml2::xml_attr(op_node, "name")
      param_order <- xml2::xml_attr(op_node, "parameterOrder")

      # Get operation documentation
      op_doc_node <- xml2::xml_find_first(op_node, "./wsdl:documentation", namespaces)
      op_documentation <- if (!is.na(op_doc_node)) xml2::xml_text(op_doc_node) else NULL

      # Get input
      input_node <- xml2::xml_find_first(op_node, "./wsdl:input", namespaces)
      input <- if (!is.na(input_node)) {
        list(
          name = xml2::xml_attr(input_node, "name"),
          message = xml2::xml_attr(input_node, "message")
        )
      } else {
        NULL
      }

      # Get output
      output_node <- xml2::xml_find_first(op_node, "./wsdl:output", namespaces)
      output <- if (!is.na(output_node)) {
        list(
          name = xml2::xml_attr(output_node, "name"),
          message = xml2::xml_attr(output_node, "message")
        )
      } else {
        NULL
      }

      # Get all faults
      fault_nodes <- xml2::xml_find_all(op_node, "./wsdl:fault", namespaces)
      faults <- lapply(fault_nodes, function(fault_node) {
        # Get fault documentation
        fault_doc_node <- xml2::xml_find_first(fault_node, "./wsdl:documentation", namespaces)
        fault_documentation <- if (!is.na(fault_doc_node)) xml2::xml_text(fault_doc_node) else NULL

        list(
          name = xml2::xml_attr(fault_node, "name"),
          message = xml2::xml_attr(fault_node, "message"),
          documentation = fault_documentation
        )
      })

      list(
        name = op_name,
        parameter_order = param_order,
        documentation = op_documentation,
        input = input,
        output = output,
        faults = faults,
        node = op_node
      )
    })

    names(operations) <- sapply(operations, function(o) o$name)

    list(
      name = name,
      documentation = documentation,
      operations = operations,
      node = pt_node
    )
  })

  names(port_types) <- sapply(port_types, function(pt) pt$name)
  port_types
}

#' Collect all binding definitions
collect_bindings <- function(wsdl_doc) {
  root <- xml2::xml_root(wsdl_doc)
  namespaces <- xml2::xml_ns(root)

  # Find all binding elements
  binding_nodes <- xml2::xml_find_all(root, ".//wsdl:binding", namespaces)

  bindings <- lapply(binding_nodes, function(bind_node) {
    name <- xml2::xml_attr(bind_node, "name")
    type <- xml2::xml_attr(bind_node, "type")

    # Get documentation
    doc_node <- xml2::xml_find_first(bind_node, "./wsdl:documentation", namespaces)
    documentation <- if (!is.na(doc_node)) xml2::xml_text(doc_node) else NULL

    # Get binding-level protocol elements (e.g., soap:binding)
    # We'll collect all non-wsdl namespace children at binding level
    binding_extensions <- list()
    children <- xml2::xml_children(bind_node)
    for (child in children) {
      child_ns <- xml2::xml_ns(child)
      child_name <- xml2::xml_name(child)
      # Skip wsdl namespace elements and documentation
      if (length(child_ns) > 0 && !grepl("wsdl", child_ns[[1]]) && child_name != "documentation") {
        binding_extensions[[child_name]] <- list(
          namespace = child_ns,
          attributes = xml2::xml_attrs(child),
          node = child
        )
      }
    }

    # Collect all operations
    op_nodes <- xml2::xml_find_all(bind_node, "./wsdl:operation", namespaces)
    operations <- lapply(op_nodes, function(op_node) {
      op_name <- xml2::xml_attr(op_node, "name")

      # Get operation documentation
      op_doc_node <- xml2::xml_find_first(op_node, "./wsdl:documentation", namespaces)
      op_documentation <- if (!is.na(op_doc_node)) xml2::xml_text(op_doc_node) else NULL

      # Get protocol-specific operation elements (e.g., soap:operation)
      op_extensions <- list()
      op_children <- xml2::xml_children(op_node)
      for (child in op_children) {
        child_ns <- xml2::xml_ns(child)
        child_name <- xml2::xml_name(child)
        if (length(child_ns) > 0 && !grepl("wsdl", child_ns[[1]]) &&
            !child_name %in% c("documentation", "input", "output", "fault")) {
          op_extensions[[child_name]] <- list(
            namespace = child_ns,
            attributes = xml2::xml_attrs(child),
            node = child
          )
        }
      }

      # Get input binding
      input_node <- xml2::xml_find_first(op_node, "./wsdl:input", namespaces)
      input <- if (!is.na(input_node)) {
        input_extensions <- list()
        input_children <- xml2::xml_children(input_node)
        for (child in input_children) {
          child_ns <- xml2::xml_ns(child)
          child_name <- xml2::xml_name(child)
          if (length(child_ns) > 0 && !grepl("wsdl", child_ns[[1]]) && child_name != "documentation") {
            input_extensions[[child_name]] <- list(
              namespace = child_ns,
              attributes = xml2::xml_attrs(child),
              node = child
            )
          }
        }
        list(
          name = xml2::xml_attr(input_node, "name"),
          extensions = input_extensions
        )
      } else {
        NULL
      }

      # Get output binding
      output_node <- xml2::xml_find_first(op_node, "./wsdl:output", namespaces)
      output <- if (!is.na(output_node)) {
        output_extensions <- list()
        output_children <- xml2::xml_children(output_node)
        for (child in output_children) {
          child_ns <- xml2::xml_ns(child)
          child_name <- xml2::xml_name(child)
          if (length(child_ns) > 0 && !grepl("wsdl", child_ns[[1]]) && child_name != "documentation") {
            output_extensions[[child_name]] <- list(
              namespace = child_ns,
              attributes = xml2::xml_attrs(child),
              node = child
            )
          }
        }
        list(
          name = xml2::xml_attr(output_node, "name"),
          extensions = output_extensions
        )
      } else {
        NULL
      }

      # Get fault bindings
      fault_nodes <- xml2::xml_find_all(op_node, "./wsdl:fault", namespaces)
      faults <- lapply(fault_nodes, function(fault_node) {
        fault_extensions <- list()
        fault_children <- xml2::xml_children(fault_node)
        for (child in fault_children) {
          child_ns <- xml2::xml_ns(child)
          child_name <- xml2::xml_name(child)
          if (length(child_ns) > 0 && !grepl("wsdl", child_ns[[1]]) && child_name != "documentation") {
            fault_extensions[[child_name]] <- list(
              namespace = child_ns,
              attributes = xml2::xml_attrs(child),
              node = child
            )
          }
        }

        # Get fault documentation
        fault_doc_node <- xml2::xml_find_first(fault_node, "./wsdl:documentation", namespaces)
        fault_documentation <- if (!is.na(fault_doc_node)) xml2::xml_text(fault_doc_node) else NULL

        list(
          name = xml2::xml_attr(fault_node, "name"),
          documentation = fault_documentation,
          extensions = fault_extensions
        )
      })

      list(
        name = op_name,
        documentation = op_documentation,
        extensions = op_extensions,
        input = input,
        output = output,
        faults = faults,
        node = op_node
      )
    })

    names(operations) <- sapply(operations, function(o) o$name)

    list(
      name = name,
      type = type,
      documentation = documentation,
      extensions = binding_extensions,
      operations = operations,
      node = bind_node
    )
  })

  names(bindings) <- sapply(bindings, function(b) b$name)
  bindings
}


#' Collect all service definitions
collect_services <- function(wsdl_doc) {
  root <- xml2::xml_root(wsdl_doc)
  namespaces <- xml2::xml_ns(root)

  # Find all service elements
  service_nodes <- xml2::xml_find_all(root, ".//wsdl:service", namespaces)

  services <- lapply(service_nodes, function(svc_node) {
    name <- xml2::xml_attr(svc_node, "name")

    # Get documentation
    doc_node <- xml2::xml_find_first(svc_node, "./wsdl:documentation", namespaces)
    documentation <- if (!is.na(doc_node)) xml2::xml_text(doc_node) else NULL

    # Collect all ports
    port_nodes <- xml2::xml_find_all(svc_node, "./wsdl:port", namespaces)
    ports <- lapply(port_nodes, function(port_node) {
      port_name <- xml2::xml_attr(port_node, "name")
      binding <- xml2::xml_attr(port_node, "binding")

      # Get port documentation
      port_doc_node <- xml2::xml_find_first(port_node, "./wsdl:documentation", namespaces)
      port_documentation <- if (!is.na(port_doc_node)) xml2::xml_text(port_doc_node) else NULL

      # Get protocol-specific address elements (e.g., soap:address)
      port_extensions <- list()
      port_children <- xml2::xml_children(port_node)
      for (child in port_children) {
        child_ns <- xml2::xml_ns(child)
        child_name <- xml2::xml_name(child)
        if (length(child_ns) > 0 && !grepl("wsdl", child_ns[[1]]) && child_name != "documentation") {
          port_extensions[[child_name]] <- list(
            namespace = child_ns,
            attributes = xml2::xml_attrs(child),
            node = child
          )
        }
      }

      list(
        name = port_name,
        binding = binding,
        documentation = port_documentation,
        extensions = port_extensions,
        node = port_node
      )
    })

    names(ports) <- sapply(ports, function(p) p$name)

    list(
      name = name,
      documentation = documentation,
      ports = ports,
      node = svc_node
    )
  })

  names(services) <- sapply(services, function(s) s$name)
  services
}

#' Collect type definitions (XSD schemas)
collect_types <- function(wsdl_doc) {
  root <- xml2::xml_root(wsdl_doc)
  namespaces <- xml2::xml_ns(root)

  # Find the types element
  types_node <- xml2::xml_find_first(root, ".//wsdl:types", namespaces)

  if (is.na(types_node)) {
    return(list())
  }

  # Get documentation
  doc_node <- xml2::xml_find_first(types_node, "./wsdl:documentation", namespaces)
  documentation <- if (!is.na(doc_node)) xml2::xml_text(doc_node) else NULL

  # Collect all schema elements
  schema_nodes <- xml2::xml_find_all(types_node, "./xsd:schema", namespaces)

  schemas <- lapply(schema_nodes, function(schema_node) {
    list(
      target_namespace = xml2::xml_attr(schema_node, "targetNamespace"),
      element_form_default = xml2::xml_attr(schema_node, "elementFormDefault"),
      attribute_form_default = xml2::xml_attr(schema_node, "attributeFormDefault"),
      node = schema_node
    )
  })

  list(
    documentation = documentation,
    schemas = schemas,
    node = types_node
  )
}

parse_wsdl_discovery <- function(wsdl_doc) {
  # 1. Parse root <definitions> and namespaces
  definitions <- parse_definitions(wsdl_doc)

  # 2. Collect all named elements (don't validate references yet)
  messages <- collect_messages(wsdl_doc)
  port_types <- collect_port_types(wsdl_doc)
  bindings <- collect_bindings(wsdl_doc)
  services <- collect_services(wsdl_doc)
  types <- collect_types(wsdl_doc)

  return(list(
    definitions = definitions,
    messages = messages,
    port_types = port_types,
    bindings = bindings,
    services = services,
    types = types
  ))
}

# wsdl validation -----------------------------------------------------------------------------

# WSDL Validation Pass - Validate references and structure
# Requires: the discovery functions from the previous artifact

#' Validation result structure
create_validation_result <- function(valid = TRUE, errors = list(), warnings = list()) {
  list(
    valid = valid,
    errors = errors,
    warnings = warnings
  )
}

#' Add error to validation result
add_error <- function(result, message, element = NULL, element_type = NULL) {
  error <- list(
    message = message,
    element = element,
    element_type = element_type
  )
  result$errors <- c(result$errors, list(error))
  result$valid <- FALSE
  result
}

#' Add warning to validation result
add_warning <- function(result, message, element = NULL, element_type = NULL) {
  warning <- list(
    message = message,
    element = element,
    element_type = element_type
  )
  result$warnings <- c(result$warnings, list(warning))
  result
}

#' Resolve a qualified name (e.g., "tns:GetQuote") to just the local part
resolve_qname <- function(qname, namespaces) {
  if (is.null(qname) || is.na(qname)) return(NULL)

  parts <- strsplit(qname, ":")[[1]]
  if (length(parts) == 2) {
    return(parts[2])  # Return local part
  }
  return(qname)  # No prefix, return as-is
}

#' Validate types section
validate_types <- function(types) {
  result <- create_validation_result()

  if (length(types) == 0) {
    result <- add_warning(result, "No types section found - WSDL has no schema definitions")
    return(result)
  }

  # Basic validation - just check that schemas exist
  if (length(types$schemas) == 0) {
    result <- add_warning(result, "Types section exists but contains no schemas")
  }

  # Could add more detailed XSD validation here

  result
}

#' Validate messages - check that referenced types/elements exist
validate_messages <- function(messages, types) {
  result <- create_validation_result()

  if (length(messages) == 0) {
    result <- add_error(result, "No messages defined in WSDL")
    return(result)
  }

  for (msg_name in names(messages)) {
    msg <- messages[[msg_name]]

    # Check that message has at least one part
    if (length(msg$parts) == 0) {
      result <- add_warning(result,
                            sprintf("Message '%s' has no parts", msg_name),
                            msg_name, "message")
    }

    # Validate each part
    for (i in seq_along(msg$parts)) {
      part <- msg$parts[[i]]
      part_name <- part$name

      # Check that part has a name
      if (is.null(part_name) || is.na(part_name)) {
        result <- add_error(result,
                            sprintf("Part %d in message '%s' has no name", i, msg_name),
                            msg_name, "message")
        next
      }

      # Check that part has either 'element' or 'type' attribute (not both, not neither)
      has_element <- !is.null(part$element) && !is.na(part$element)
      has_type <- !is.null(part$type) && !is.na(part$type)

      if (!has_element && !has_type) {
        result <- add_error(result,
                            sprintf("Part '%s' in message '%s' must have either 'element' or 'type' attribute",
                                    part_name, msg_name),
                            msg_name, "message")
      }

      if (has_element && has_type) {
        result <- add_error(result,
                            sprintf("Part '%s' in message '%s' cannot have both 'element' and 'type' attributes",
                                    part_name, msg_name),
                            msg_name, "message")
      }

      # Note: We're not validating that the type/element actually exists in the schema
      # That would require full XSD parsing which is complex
      # You could add that validation here if needed
    }
  }

  result
}

#' Validate port types - check that referenced messages exist
validate_port_types <- function(port_types, messages) {
  result <- create_validation_result()

  if (length(port_types) == 0) {
    result <- add_error(result, "No portTypes defined in WSDL")
    return(result)
  }

  for (pt_name in names(port_types)) {
    pt <- port_types[[pt_name]]

    # Check that portType has at least one operation
    if (length(pt$operations) == 0) {
      result <- add_warning(result,
                            sprintf("PortType '%s' has no operations", pt_name),
                            pt_name, "portType")
      next
    }

    # Validate each operation
    for (op_name in names(pt$operations)) {
      op <- pt$operations[[op_name]]

      # Check that operation has at least input or output
      has_input <- !is.null(op$input)
      has_output <- !is.null(op$output)

      if (!has_input && !has_output) {
        result <- add_error(result,
                            sprintf("Operation '%s' in portType '%s' must have at least one input or output",
                                    op_name, pt_name),
                            sprintf("%s.%s", pt_name, op_name), "operation")
        next
      }

      # Validate input message reference
      if (has_input) {
        msg_ref <- resolve_qname(op$input$message, NULL)
        if (!msg_ref %in% names(messages)) {
          result <- add_error(result,
                              sprintf("Operation '%s' in portType '%s' references non-existent input message '%s'",
                                      op_name, pt_name, msg_ref),
                              sprintf("%s.%s", pt_name, op_name), "operation")
        }
      }

      # Validate output message reference
      if (has_output) {
        msg_ref <- resolve_qname(op$output$message, NULL)
        if (!msg_ref %in% names(messages)) {
          result <- add_error(result,
                              sprintf("Operation '%s' in portType '%s' references non-existent output message '%s'",
                                      op_name, pt_name, msg_ref),
                              sprintf("%s.%s", pt_name, op_name), "operation")
        }
      }

      # Validate fault message references
      for (fault in op$faults) {
        if (is.null(fault$name) || is.na(fault$name)) {
          result <- add_error(result,
                              sprintf("Fault in operation '%s' of portType '%s' has no name",
                                      op_name, pt_name),
                              sprintf("%s.%s", pt_name, op_name), "operation")
          next
        }

        if (is.null(fault$message) || is.na(fault$message)) {
          result <- add_error(result,
                              sprintf("Fault '%s' in operation '%s' of portType '%s' has no message attribute",
                                      fault$name, op_name, pt_name),
                              sprintf("%s.%s", pt_name, op_name), "operation")
          next
        }

        msg_ref <- resolve_qname(fault$message, NULL)
        if (!msg_ref %in% names(messages)) {
          result <- add_error(result,
                              sprintf("Fault '%s' in operation '%s' of portType '%s' references non-existent message '%s'",
                                      fault$name, op_name, pt_name, msg_ref),
                              sprintf("%s.%s", pt_name, op_name), "operation")
        }
      }
    }
  }

  result
}

#' Validate bindings - check that referenced portTypes exist and operations match
validate_bindings <- function(bindings, port_types) {
  result <- create_validation_result()

  if (length(bindings) == 0) {
    result <- add_warning(result, "No bindings defined in WSDL")
    return(result)
  }

  for (bind_name in names(bindings)) {
    bind <- bindings[[bind_name]]

    # Check that binding references a portType
    if (is.null(bind$type) || is.na(bind$type)) {
      result <- add_error(result,
                          sprintf("Binding '%s' has no type attribute", bind_name),
                          bind_name, "binding")
      next
    }

    # Resolve and check portType reference
    pt_ref <- resolve_qname(bind$type, NULL)
    if (!pt_ref %in% names(port_types)) {
      result <- add_error(result,
                          sprintf("Binding '%s' references non-existent portType '%s'",
                                  bind_name, pt_ref),
                          bind_name, "binding")
      next
    }

    pt <- port_types[[pt_ref]]

    # Check that binding has protocol-specific extensions
    if (length(bind$extensions) == 0) {
      result <- add_warning(result,
                            sprintf("Binding '%s' has no protocol-specific binding elements (e.g., soap:binding)",
                                    bind_name),
                            bind_name, "binding")
    }

    # Validate operations in binding match portType operations
    pt_op_names <- names(pt$operations)
    bind_op_names <- names(bind$operations)

    # Check for operations in binding not in portType
    extra_ops <- setdiff(bind_op_names, pt_op_names)
    if (length(extra_ops) > 0) {
      for (op in extra_ops) {
        result <- add_error(result,
                            sprintf("Binding '%s' has operation '%s' not defined in portType '%s'",
                                    bind_name, op, pt_ref),
                            bind_name, "binding")
      }
    }

    # Check for operations in portType not in binding
    missing_ops <- setdiff(pt_op_names, bind_op_names)
    if (length(missing_ops) > 0) {
      for (op in missing_ops) {
        result <- add_warning(result,
                              sprintf("Binding '%s' missing operation '%s' defined in portType '%s'",
                                      bind_name, op, pt_ref),
                              bind_name, "binding")
      }
    }

    # Validate each operation in binding
    for (op_name in bind_op_names) {
      if (!op_name %in% pt_op_names) next  # Already reported as error

      bind_op <- bind$operations[[op_name]]
      pt_op <- pt$operations[[op_name]]

      # Check input exists if portType has input
      if (!is.null(pt_op$input) && is.null(bind_op$input)) {
        result <- add_error(result,
                            sprintf("Binding operation '%s.%s' missing input (required by portType)",
                                    bind_name, op_name),
                            bind_name, "binding")
      }

      # Check output exists if portType has output
      if (!is.null(pt_op$output) && is.null(bind_op$output)) {
        result <- add_error(result,
                            sprintf("Binding operation '%s.%s' missing output (required by portType)",
                                    bind_name, op_name),
                            bind_name, "binding")
      }

      # Check that binding input/output have protocol extensions
      if (!is.null(bind_op$input) && length(bind_op$input$extensions) == 0) {
        result <- add_warning(result,
                              sprintf("Binding operation '%s.%s' input has no protocol-specific elements",
                                      bind_name, op_name),
                              bind_name, "binding")
      }

      if (!is.null(bind_op$output) && length(bind_op$output$extensions) == 0) {
        result <- add_warning(result,
                              sprintf("Binding operation '%s.%s' output has no protocol-specific elements",
                                      bind_name, op_name),
                              bind_name, "binding")
      }

      # Validate faults match
      pt_fault_names <- sapply(pt_op$faults, function(f) f$name)
      bind_fault_names <- sapply(bind_op$faults, function(f) f$name)

      extra_faults <- setdiff(bind_fault_names, pt_fault_names)
      if (length(extra_faults) > 0) {
        for (fault in extra_faults) {
          result <- add_error(result,
                              sprintf("Binding operation '%s.%s' has fault '%s' not defined in portType",
                                      bind_name, op_name, fault),
                              bind_name, "binding")
        }
      }
    }
  }

  result
}

#' Validate services - check that referenced bindings exist
validate_services <- function(services, bindings) {
  result <- create_validation_result()

  if (length(services) == 0) {
    result <- add_warning(result, "No services defined in WSDL")
    return(result)
  }

  for (svc_name in names(services)) {
    svc <- services[[svc_name]]

    # Check that service has at least one port
    if (length(svc$ports) == 0) {
      result <- add_warning(result,
                            sprintf("Service '%s' has no ports", svc_name),
                            svc_name, "service")
      next
    }

    # Validate each port
    for (port_name in names(svc$ports)) {
      port <- svc$ports[[port_name]]

      # Check that port references a binding
      if (is.null(port$binding) || is.na(port$binding)) {
        result <- add_error(result,
                            sprintf("Port '%s' in service '%s' has no binding attribute",
                                    port_name, svc_name),
                            sprintf("%s.%s", svc_name, port_name), "port")
        next
      }

      # Resolve and check binding reference
      bind_ref <- resolve_qname(port$binding, NULL)
      if (!bind_ref %in% names(bindings)) {
        result <- add_error(result,
                            sprintf("Port '%s' in service '%s' references non-existent binding '%s'",
                                    port_name, svc_name, bind_ref),
                            sprintf("%s.%s", svc_name, port_name), "port")
      }

      # Check that port has address/endpoint information
      if (length(port$extensions) == 0) {
        result <- add_warning(result,
                              sprintf("Port '%s' in service '%s' has no address elements (e.g., soap:address)",
                                      port_name, svc_name),
                              sprintf("%s.%s", svc_name, port_name), "port")
      }
    }
  }

  result
}

#' Merge validation results
merge_validation_results <- function(...) {
  results <- list(...)

  merged <- create_validation_result()

  for (result in results) {
    merged$errors <- c(merged$errors, result$errors)
    merged$warnings <- c(merged$warnings, result$warnings)
    if (!result$valid) {
      merged$valid <- FALSE
    }
  }

  merged
}

#' Main validation function - validates all references
validate_references <- function(discovered_elements) {
  # Validate in dependency order (bottom-up)

  # 1. Validate types (most independent)
  types_result <- validate_types(discovered_elements$types)

  # 2. Validate messages (depend on types)
  messages_result <- validate_messages(discovered_elements$messages,
                                       discovered_elements$types)

  # 3. Validate port types (depend on messages)
  port_types_result <- validate_port_types(discovered_elements$port_types,
                                           discovered_elements$messages)

  # 4. Validate bindings (depend on port types)
  bindings_result <- validate_bindings(discovered_elements$bindings,
                                       discovered_elements$port_types)

  # 5. Validate services (depend on bindings)
  services_result <- validate_services(discovered_elements$services,
                                       discovered_elements$bindings)

  # Merge all results
  overall_result <- merge_validation_results(
    types_result,
    messages_result,
    port_types_result,
    bindings_result,
    services_result
  )

  overall_result
}

#' Print validation results in a readable format
print_validation_results <- function(result) {
  cat("\n=== WSDL Validation Results ===\n\n")

  if (result$valid) {
    cat("✓ WSDL is valid\n")
  } else {
    cat("✗ WSDL has errors\n")
  }

  if (length(result$errors) > 0) {
    cat("\nErrors:\n")
    for (i in seq_along(result$errors)) {
      err <- result$errors[[i]]
      cat(sprintf("  %d. [%s] %s", i,
                  ifelse(is.null(err$element_type), "general", err$element_type),
                  err$message))
      if (!is.null(err$element)) {
        cat(sprintf(" (element: %s)", err$element))
      }
      cat("\n")
    }
  }

  if (length(result$warnings) > 0) {
    cat("\nWarnings:\n")
    for (i in seq_along(result$warnings)) {
      warn <- result$warnings[[i]]
      cat(sprintf("  %d. [%s] %s", i,
                  ifelse(is.null(warn$element_type), "general", warn$element_type),
                  warn$message))
      if (!is.null(warn$element)) {
        cat(sprintf(" (element: %s)", warn$element))
      }
      cat("\n")
    }
  }

  if (length(result$errors) == 0 && length(result$warnings) == 0) {
    cat("\nNo issues found.\n")
  }

  cat("\nSummary:\n")
  cat(sprintf("  Errors: %d\n", length(result$errors)))
  cat(sprintf("  Warnings: %d\n", length(result$warnings)))
  cat("\n")
}


# build model ---------------------------------------------------------------------------------

# WSDL Model Builder - Combines discovery and validation into a complete model
# Requires: discovery and validation functions from previous artifacts

#' Build a complete WSDL model with resolved references
#'
#' This function takes the discovered elements and builds a complete object model
#' where references are resolved into actual objects rather than just names.
#'
#' @param discovered_elements List from parse_wsdl_discovery()
#' @param validation_result Optional validation result to include
#' @return Complete WSDL model with resolved references
build_wsdl_model <- function(discovered_elements, validation_result = NULL) {

  # Extract components
  definitions <- discovered_elements$definitions
  messages <- discovered_elements$messages
  port_types <- discovered_elements$port_types
  bindings <- discovered_elements$bindings
  services <- discovered_elements$services
  types <- discovered_elements$types

  # Build enhanced messages with metadata
  enhanced_messages <- lapply(names(messages), function(msg_name) {
    msg <- messages[[msg_name]]

    list(
      name = msg_name,
      documentation = msg$documentation,
      parts = msg$parts,
      part_count = length(msg$parts),
      node = msg$node
    )
  })
  names(enhanced_messages) <- names(messages)

  # Build enhanced port types with resolved message references
  enhanced_port_types <- lapply(names(port_types), function(pt_name) {
    pt <- port_types[[pt_name]]

    # Process operations with resolved references
    enhanced_operations <- lapply(names(pt$operations), function(op_name) {
      op <- pt$operations[[op_name]]

      # Resolve input message
      input_info <- NULL
      if (!is.null(op$input)) {
        msg_ref <- resolve_qname(op$input$message, NULL)
        input_info <- list(
          name = op$input$name,
          message_name = msg_ref,
          message = messages[[msg_ref]]  # Resolved reference
        )
      }

      # Resolve output message
      output_info <- NULL
      if (!is.null(op$output)) {
        msg_ref <- resolve_qname(op$output$message, NULL)
        output_info <- list(
          name = op$output$name,
          message_name = msg_ref,
          message = messages[[msg_ref]]  # Resolved reference
        )
      }

      # Resolve fault messages
      fault_info <- lapply(op$faults, function(fault) {
        msg_ref <- resolve_qname(fault$message, NULL)
        list(
          name = fault$name,
          message_name = msg_ref,
          message = messages[[msg_ref]],  # Resolved reference
          documentation = fault$documentation
        )
      })
      names(fault_info) <- sapply(fault_info, function(f) f$name)

      # Determine operation pattern
      pattern <- if (!is.null(input_info) && !is.null(output_info)) {
        "request-response"
      } else if (!is.null(input_info)) {
        "one-way"
      } else if (!is.null(output_info)) {
        "notification"
      } else {
        "invalid"
      }

      list(
        name = op_name,
        pattern = pattern,
        documentation = op$documentation,
        parameter_order = op$parameter_order,
        input = input_info,
        output = output_info,
        faults = fault_info,
        has_input = !is.null(input_info),
        has_output = !is.null(output_info),
        fault_count = length(fault_info),
        node = op$node
      )
    })
    names(enhanced_operations) <- names(pt$operations)

    list(
      name = pt_name,
      documentation = pt$documentation,
      operations = enhanced_operations,
      operation_count = length(enhanced_operations),
      node = pt$node
    )
  })
  names(enhanced_port_types) <- names(port_types)

  # Build enhanced bindings with resolved portType references
  enhanced_bindings <- lapply(names(bindings), function(bind_name) {
    bind <- bindings[[bind_name]]

    # Resolve portType reference
    pt_ref <- resolve_qname(bind$type, NULL)
    port_type <- port_types[[pt_ref]]

    # Extract protocol information from extensions
    protocol <- "unknown"
    protocol_version <- NULL
    transport <- NULL
    style <- NULL

    if (length(bind$extensions) > 0) {
      # Check for SOAP binding
      if ("binding" %in% names(bind$extensions)) {
        soap_binding <- bind$extensions$binding
        if (!is.null(soap_binding$attributes)) {
          attrs <- soap_binding$attributes
          transport <- attrs["transport"]
          style <- attrs["style"]
          protocol <- "SOAP"
        }
      }
    }

    # Process operations with resolved references
    enhanced_operations <- lapply(names(bind$operations), function(op_name) {
      bind_op <- bind$operations[[op_name]]
      pt_op <- if (!is.null(port_type)) port_type$operations[[op_name]] else NULL

      # Extract SOAP action if present
      soap_action <- NULL
      operation_style <- NULL
      if (length(bind_op$extensions) > 0 && "operation" %in% names(bind_op$extensions)) {
        soap_op <- bind_op$extensions$operation
        if (!is.null(soap_op$attributes)) {
          soap_action <- soap_op$attributes["soapAction"]
          operation_style <- soap_op$attributes["style"]
        }
      }

      list(
        name = op_name,
        documentation = bind_op$documentation,
        soap_action = soap_action,
        style = operation_style,
        port_type_operation = pt_op,  # Reference to abstract operation
        input = bind_op$input,
        output = bind_op$output,
        faults = bind_op$faults,
        extensions = bind_op$extensions,
        node = bind_op$node
      )
    })
    names(enhanced_operations) <- names(bind$operations)

    list(
      name = bind_name,
      documentation = bind$documentation,
      port_type_name = pt_ref,
      port_type = port_type,  # Resolved reference
      protocol = protocol,
      transport = transport,
      style = style,
      operations = enhanced_operations,
      operation_count = length(enhanced_operations),
      extensions = bind$extensions,
      node = bind$node
    )
  })
  names(enhanced_bindings) <- names(bindings)

  # Build enhanced services with resolved binding references
  enhanced_services <- lapply(names(services), function(svc_name) {
    svc <- services[[svc_name]]

    # Process ports with resolved references
    enhanced_ports <- lapply(names(svc$ports), function(port_name) {
      port <- svc$ports[[port_name]]

      # Resolve binding reference
      bind_ref <- resolve_qname(port$binding, NULL)
      binding <- bindings[[bind_ref]]

      # Extract endpoint address
      address <- NULL
      if (length(port$extensions) > 0 && "address" %in% names(port$extensions)) {
        addr_ext <- port$extensions$address
        if (!is.null(addr_ext$attributes)) {
          address <- addr_ext$attributes["location"]
        }
      }

      list(
        name = port_name,
        documentation = port$documentation,
        binding_name = bind_ref,
        binding = binding,  # Resolved reference
        address = address,
        extensions = port$extensions,
        node = port$node
      )
    })
    names(enhanced_ports) <- names(svc$ports)

    list(
      name = svc_name,
      documentation = svc$documentation,
      ports = enhanced_ports,
      port_count = length(enhanced_ports),
      node = svc$node
    )
  })
  names(enhanced_services) <- names(services)

  # Build summary statistics
  summary_stats <- list(
    message_count = length(enhanced_messages),
    port_type_count = length(enhanced_port_types),
    binding_count = length(enhanced_bindings),
    service_count = length(enhanced_services),
    total_operations = sum(sapply(enhanced_port_types, function(pt) pt$operation_count)),
    has_types = length(types) > 0 && length(types$schemas) > 0,
    target_namespace = definitions$target_namespace,
    wsdl_name = definitions$name
  )

  # Create the complete model
  model <- list(
    # Metadata
    name = definitions$name,
    target_namespace = definitions$target_namespace,
    namespaces = definitions$namespaces,

    # Core components
    types = types,
    messages = enhanced_messages,
    port_types = enhanced_port_types,
    bindings = enhanced_bindings,
    services = enhanced_services,

    # Additional info
    summary = summary_stats,
    validation = validation_result,

    # Original discovered elements (for advanced use)
    raw = discovered_elements
  )

  # Add S3 class for custom printing
  class(model) <- c("wsdl_model", "list")

  model
}

#' Print method for WSDL model
#' @export
print.wsdl_model <- function(x, ...) {
  cat("\n=== WSDL Model ===\n\n")

  # Basic info
  cat(sprintf("Name: %s\n", ifelse(is.null(x$name) || is.na(x$name), "(unnamed)", x$name)))
  cat(sprintf("Target Namespace: %s\n", x$target_namespace))

  # Validation status
  if (!is.null(x$validation)) {
    cat(sprintf("\nValidation: %s\n",
                ifelse(x$validation$valid, "✓ VALID", "✗ INVALID")))
    if (length(x$validation$errors) > 0) {
      cat(sprintf("  Errors: %d\n", length(x$validation$errors)))
    }
    if (length(x$validation$warnings) > 0) {
      cat(sprintf("  Warnings: %d\n", length(x$validation$warnings)))
    }
  }

  # Summary
  cat("\n--- Summary ---\n")
  cat(sprintf("Messages: %d\n", x$summary$message_count))
  cat(sprintf("Port Types: %d\n", x$summary$port_type_count))
  cat(sprintf("Bindings: %d\n", x$summary$binding_count))
  cat(sprintf("Services: %d\n", x$summary$service_count))
  cat(sprintf("Total Operations: %d\n", x$summary$total_operations))
  cat(sprintf("Has Type Definitions: %s\n", ifelse(x$summary$has_types, "Yes", "No")))

  # Port Types detail
  if (length(x$port_types) > 0) {
    cat("\n--- Port Types ---\n")
    for (pt_name in names(x$port_types)) {
      pt <- x$port_types[[pt_name]]
      cat(sprintf("  %s (%d operations)\n", pt_name, pt$operation_count))

      for (op_name in names(pt$operations)) {
        op <- pt$operations[[op_name]]
        cat(sprintf("    - %s [%s]\n", op_name, op$pattern))
      }
    }
  }

  # Services detail
  if (length(x$services) > 0) {
    cat("\n--- Services ---\n")
    for (svc_name in names(x$services)) {
      svc <- x$services[[svc_name]]
      cat(sprintf("  %s (%d ports)\n", svc_name, svc$port_count))

      for (port_name in names(svc$ports)) {
        port <- svc$ports[[port_name]]
        cat(sprintf("    - %s\n", port_name))
        cat(sprintf("      Binding: %s\n", port$binding_name))
        if (!is.null(port$address) && !is.na(port$address)) {
          cat(sprintf("      Address: %s\n", port$address))
        }
      }
    }
  }

  cat("\n")
  invisible(x)
}

#' Summary method for WSDL model
#' @export
summary.wsdl_model <- function(object, ...) {
  cat("\n=== WSDL Model Summary ===\n\n")

  cat(sprintf("WSDL Name: %s\n",
              ifelse(is.null(object$name) || is.na(object$name), "(unnamed)", object$name)))
  cat(sprintf("Target Namespace: %s\n\n", object$target_namespace))

  # Component counts
  cat("Component Counts:\n")
  cat(sprintf("  Messages: %d\n", object$summary$message_count))
  cat(sprintf("  Port Types: %d\n", object$summary$port_type_count))
  cat(sprintf("  Bindings: %d\n", object$summary$binding_count))
  cat(sprintf("  Services: %d\n", object$summary$service_count))
  cat(sprintf("  Operations: %d\n", object$summary$total_operations))

  # Operation patterns
  if (length(object$port_types) > 0) {
    all_ops <- unlist(lapply(object$port_types, function(pt) pt$operations), recursive = FALSE)
    patterns <- table(sapply(all_ops, function(op) op$pattern))

    cat("\nOperation Patterns:\n")
    for (pattern in names(patterns)) {
      cat(sprintf("  %s: %d\n", pattern, patterns[pattern]))
    }
  }

  # Protocols
  if (length(object$bindings) > 0) {
    protocols <- table(sapply(object$bindings, function(b) b$protocol))

    cat("\nProtocols:\n")
    for (protocol in names(protocols)) {
      cat(sprintf("  %s: %d bindings\n", protocol, protocols[protocol]))
    }
  }

  # Validation status
  if (!is.null(object$validation)) {
    cat(sprintf("\nValidation Status: %s\n",
                ifelse(object$validation$valid, "VALID", "INVALID")))
    cat(sprintf("  Errors: %d\n", length(object$validation$errors)))
    cat(sprintf("  Warnings: %d\n", length(object$validation$warnings)))
  }

  cat("\n")
  invisible(object)
}

#' Get all operations from a WSDL model
#' @export
get_operations <- function(wsdl_model, port_type = NULL) {
  if (is.null(port_type)) {
    # Return all operations from all port types
    all_ops <- list()
    for (pt_name in names(wsdl_model$port_types)) {
      pt <- wsdl_model$port_types[[pt_name]]
      for (op_name in names(pt$operations)) {
        op <- pt$operations[[op_name]]
        all_ops[[paste(pt_name, op_name, sep = ".")]] <- op
      }
    }
    return(all_ops)
  } else {
    # Return operations from specific port type
    if (port_type %in% names(wsdl_model$port_types)) {
      return(wsdl_model$port_types[[port_type]]$operations)
    } else {
      warning(sprintf("Port type '%s' not found", port_type))
      return(list())
    }
  }
}

#' Get service endpoints from a WSDL model
#' @export
get_endpoints <- function(wsdl_model, service = NULL) {
  endpoints <- list()

  services_to_process <- if (is.null(service)) {
    names(wsdl_model$services)
  } else {
    service
  }

  for (svc_name in services_to_process) {
    if (!svc_name %in% names(wsdl_model$services)) {
      warning(sprintf("Service '%s' not found", svc_name))
      next
    }

    svc <- wsdl_model$services[[svc_name]]
    for (port_name in names(svc$ports)) {
      port <- svc$ports[[port_name]]
      endpoints[[paste(svc_name, port_name, sep = ".")]] <- list(
        service = svc_name,
        port = port_name,
        address = port$address,
        binding = port$binding_name
      )
    }
  }

  endpoints
}

#' Main WSDL parser function - combines all steps
#' @export
parse_wsdl <- function(wsdl_file) {
  cat(sprintf("Parsing WSDL: %s\n", wsdl_file))

  # Step 1: Discovery
  cat("  [1/3] Discovering elements...\n")
  discovered <- parse_wsdl_discovery(wsdl_file)

  # Step 2: Validation
  cat("  [2/3] Validating references...\n")
  validation <- validate_references(discovered)

  # Step 3: Build model
  cat("  [3/3] Building model...\n")
  model <- build_wsdl_model(discovered, validation)

  cat("Done.\n\n")

  # Print validation summary
  if (!validation$valid) {
    cat("WARNING: WSDL has validation errors!\n")
    print_validation_results(validation)
  } else if (length(validation$warnings) > 0) {
    cat("Note: WSDL has warnings.\n")
    print_validation_results(validation)
  }

  model
}


# client --------------------------------------------------------------------------------------

# SOAP Client - Phase 1: Basic SOAP Request/Response
# Requires: xml2, httr packages and the WSDL parser functions



#' Get SOAP namespace for version
get_soap_namespace <- function(version = "1.1") {
  if (version == "1.1") {
    "http://schemas.xmlsoap.org/soap/envelope/"
  } else if (version == "1.2") {
    "http://www.w3.org/2003/05/soap-envelope"
  } else {
    stop("Unsupported SOAP version. Use '1.1' or '1.2'")
  }
}

#' Get encoding namespace
get_encoding_namespace <- function() {
  "http://schemas.xmlsoap.org/soap/encoding/"
}

#' Detect SOAP version from binding
detect_soap_version <- function(binding) {
  # Check binding extensions for SOAP namespace
  if (length(binding$extensions) > 0) {
    for (ext in binding$extensions) {
      if (!is.null(ext$namespace)) {
        ns <- ext$namespace[[1]]
        if (grepl("schemas.xmlsoap.org/wsdl/soap12", ns)) {
          return("1.2")
        } else if (grepl("schemas.xmlsoap.org/wsdl/soap", ns)) {
          return("1.1")
        }
      }
    }
  }
  # Default to 1.1
  return("1.1")
}

#' Serialize a simple value to XML text
serialize_value <- function(value, param_name = NULL) {
  if (is.null(value)) {
    return("")
  } else if (is.logical(value)) {
    return(tolower(as.character(value)))
  } else if (is.numeric(value)) {
    return(as.character(value))
  } else {
    return(as.character(value))
  }
}

#' Create XML element for a parameter
create_param_element <- function(doc, parent, param_name, param_value, namespace = NULL) {
  if (is.list(param_value) && !is.null(names(param_value))) {
    # Complex type - nested structure
    if (!is.null(namespace)) {
      elem <- xml_add_child(parent, param_name, namespace = namespace)
    } else {
      elem <- xml_add_child(parent, param_name)
    }

    for (field_name in names(param_value)) {
      create_param_element(doc, elem, field_name, param_value[[field_name]], namespace)
    }
  } else if (is.list(param_value)) {
    # Array/list of values
    for (item in param_value) {
      create_param_element(doc, parent, param_name, item, namespace)
    }
  } else {
    # Simple value
    value_text <- serialize_value(param_value)
    if (!is.null(namespace)) {
      xml_add_child(parent, param_name, value_text, namespace = namespace)
    } else {
      xml_add_child(parent, param_name, value_text)
    }
  }

  invisible(parent)
}

#' Create a SOAP request message
#'
#' @param wsdl_model WSDL model from parse_wsdl()
#' @param operation_name Name of the operation to call
#' @param parameters Named list of parameters
#' @param service_name Optional service name (uses first if not specified)
#' @param port_name Optional port name (uses first if not specified)
#' @return XML document as string
#' @export
create_soap_request <- function(wsdl_model, operation_name, parameters = list(),
                                service_name = NULL, port_name = NULL) {

  # Find the service and port
  if (is.null(service_name)) {
    service_name <- names(wsdl_model$services)[1]
    if (is.null(service_name)) {
      stop("No services defined in WSDL")
    }
  }

  service <- wsdl_model$services[[service_name]]
  if (is.null(service)) {
    stop(sprintf("Service '%s' not found", service_name))
  }

  if (is.null(port_name)) {
    port_name <- names(service$ports)[1]
    if (is.null(port_name)) {
      stop(sprintf("No ports defined in service '%s'", service_name))
    }
  }

  port <- service$ports[[port_name]]
  if (is.null(port)) {
    stop(sprintf("Port '%s' not found in service '%s'", port_name, service_name))
  }

  # Get the binding
  binding <- port$binding
  if (is.null(binding)) {
    stop(sprintf("Binding '%s' not found", port$binding_name))
  }

  # Get the operation from binding
  binding_op <- binding$operations[[operation_name]]
  if (is.null(binding_op)) {
    stop(sprintf("Operation '%s' not found in binding", operation_name))
  }

  # Get the abstract operation from portType
  port_type_op <- binding_op$port_type_operation
  if (is.null(port_type_op)) {
    stop(sprintf("Abstract operation '%s' not found", operation_name))
  }

  # Get input message
  if (is.null(port_type_op$input)) {
    stop(sprintf("Operation '%s' has no input", operation_name))
  }

  input_message <- port_type_op$input$message
  if (is.null(input_message)) {
    stop(sprintf("Input message for operation '%s' not found", operation_name))
  }

  # Detect SOAP version
  soap_version <- detect_soap_version(binding)
  soap_ns <- get_soap_namespace(soap_version)

  # Get target namespace
  target_ns <- wsdl_model$target_namespace

  # Create SOAP envelope
  envelope <- xml_new_root(
    "soap:Envelope",
    "xmlns:soap" = soap_ns,
    "xmlns:tns" = target_ns
  )

  # Add SOAP header (empty for now)
  header <- xml_add_child(envelope, "soap:Header")

  # Add SOAP body
  body <- xml_add_child(envelope, "soap:Body")

  # Determine operation style from binding
  style <- "document"  # default
  if (!is.null(binding$style)) {
    style <- binding$style
  }
  if (!is.null(binding_op$style)) {
    style <- binding_op$style
  }

  # Create operation element in body
  # For document/literal, use operation name from portType
  operation_element <- xml_add_child(body, operation_name, namespace = "tns")

  # Add parameters based on message parts
  if (length(input_message$parts) > 0) {
    for (part in input_message$parts) {
      part_name <- part$name

      if (part_name %in% names(parameters)) {
        param_value <- parameters[[part_name]]
        create_param_element(envelope, operation_element, part_name, param_value, "tns")
      } else {
        # Part not provided - could add warning or leave empty
        warning(sprintf("Parameter '%s' not provided for operation '%s'",
                        part_name, operation_name))
      }
    }
  }

  # Convert to string
  soap_request <- as.character(envelope)

  # Return as list with metadata
  list(
    xml = soap_request,
    endpoint = port$address,
    soap_action = binding_op$soap_action,
    soap_version = soap_version,
    operation = operation_name,
    service = service_name,
    port = port_name
  )
}

#' Send a SOAP request to the web service
#'
#' @param soap_request Request object from create_soap_request()
#' @param timeout Timeout in seconds (default: 60)
#' @param verbose Print request/response details (default: FALSE)
#' @return httr response object
#' @export
send_soap_request <- function(soap_request, timeout = 60, verbose = FALSE) {

  endpoint <- soap_request$endpoint
  if (is.null(endpoint) || is.na(endpoint)) {
    stop("No endpoint address specified")
  }

  # Prepare headers
  content_type <- if (soap_request$soap_version == "1.2") {
    sprintf("application/soap+xml; charset=utf-8; action=\"%s\"",
            ifelse(is.null(soap_request$soap_action), "", soap_request$soap_action))
  } else {
    "text/xml; charset=utf-8"
  }

  headers <- c(
    "Content-Type" = content_type
  )

  # Add SOAPAction header for SOAP 1.1
  if (soap_request$soap_version == "1.1") {
    soap_action <- if (!is.null(soap_request$soap_action) && !is.na(soap_request$soap_action)) {
      sprintf('"%s"', soap_request$soap_action)
    } else {
      '""'
    }
    headers <- c(headers, "SOAPAction" = soap_action)
  }

  if (verbose) {
    cat("\n=== SOAP Request ===\n")
    cat(sprintf("Endpoint: %s\n", endpoint))
    cat(sprintf("SOAPAction: %s\n", soap_request$soap_action))
    cat(sprintf("SOAP Version: %s\n", soap_request$soap_version))
    cat("\nRequest Body:\n")
    cat(soap_request$xml)
    cat("\n\n")
  }

  # Send POST request
  response <- tryCatch({
    POST(
      url = endpoint,
      body = soap_request$xml,
      add_headers(.headers = headers),
      timeout(timeout),
      encode = "raw"
    )
  }, error = function(e) {
    stop(sprintf("Failed to send SOAP request: %s", e$message))
  })

  if (verbose) {
    cat("=== SOAP Response ===\n")
    cat(sprintf("Status: %d\n", status_code(response)))
    cat("\nResponse Body:\n")
    cat(content(response, "text", encoding = "UTF-8"))
    cat("\n\n")
  }

  response
}

#' Parse SOAP response
#'
#' @param response httr response object from send_soap_request()
#' @param simplify Attempt to simplify to R list (default: TRUE)
#' @return Parsed response as list or xml_document
#' @export
parse_soap_response <- function(response, simplify = TRUE) {

  # Check HTTP status
  if (status_code(response) >= 400) {
    warning(sprintf("HTTP error: %d", status_code(response)))
  }

  # Get response content
  response_text <- content(response, "text", encoding = "UTF-8")

  if (nchar(response_text) == 0) {
    stop("Empty response received")
  }

  # Parse XML
  response_xml <- tryCatch({
    read_xml(response_text)
  }, error = function(e) {
    stop(sprintf("Failed to parse response XML: %s", e$message))
  })

  # Find SOAP namespace
  namespaces <- xml_ns(response_xml)
  soap_ns_name <- NULL
  for (ns_name in names(namespaces)) {
    if (grepl("soap/envelope", namespaces[[ns_name]])) {
      soap_ns_name <- ns_name
      break
    }
  }

  if (is.null(soap_ns_name)) {
    # Try without namespace prefix
    soap_ns_name <- "soap"
    namespaces <- c(namespaces, "soap" = "http://schemas.xmlsoap.org/soap/envelope/")
  }

  # Check for SOAP fault
  fault_node <- xml_find_first(response_xml,
                               sprintf(".//%s:Body/%s:Fault", soap_ns_name, soap_ns_name),
                               namespaces)

  if (!is.na(fault_node)) {
    # Parse SOAP fault
    fault_code <- xml_text(xml_find_first(fault_node, ".//faultcode"))
    fault_string <- xml_text(xml_find_first(fault_node, ".//faultstring"))
    fault_detail <- xml_find_first(fault_node, ".//detail")

    fault_info <- list(
      is_fault = TRUE,
      fault_code = fault_code,
      fault_string = fault_string,
      fault_detail = if (!is.na(fault_detail)) as.character(fault_detail) else NULL,
      raw_xml = response_xml
    )

    class(fault_info) <- c("soap_fault", "list")
    return(fault_info)
  }

  # Get SOAP body
  body_node <- xml_find_first(response_xml,
                              sprintf(".//%s:Body", soap_ns_name),
                              namespaces)

  if (is.na(body_node)) {
    stop("Could not find SOAP Body in response")
  }

  if (!simplify) {
    return(body_node)
  }

  # Simplify to R list
  result <- xml_to_list(body_node)

  list(
    is_fault = FALSE,
    result = result,
    raw_xml = response_xml
  )
}

#' Convert XML node to R list (recursive)
xml_to_list <- function(node) {
  # Get children
  children <- xml_children(node)

  if (length(children) == 0) {
    # Leaf node - return text
    text <- xml_text(node)
    return(if (nchar(text) > 0) text else NULL)
  }

  # Has children - process recursively
  result <- list()
  child_names <- xml_name(children)

  for (i in seq_along(children)) {
    child <- children[[i]]
    child_name <- child_names[i]
    child_value <- xml_to_list(child)

    # Handle multiple elements with same name
    if (child_name %in% names(result)) {
      # Convert to list if not already
      if (!is.list(result[[child_name]]) || !is.null(names(result[[child_name]]))) {
        result[[child_name]] <- list(result[[child_name]])
      }
      result[[child_name]] <- c(result[[child_name]], list(child_value))
    } else {
      result[[child_name]] <- child_value
    }
  }

  result
}

#' Print method for SOAP fault
#' @export
print.soap_fault <- function(x, ...) {
  cat("\n=== SOAP Fault ===\n\n")
  cat(sprintf("Fault Code: %s\n", x$fault_code))
  cat(sprintf("Fault String: %s\n", x$fault_string))
  if (!is.null(x$fault_detail)) {
    cat("\nFault Detail:\n")
    cat(x$fault_detail)
    cat("\n")
  }
  cat("\n")
  invisible(x)
}

#' High-level function to call a SOAP operation
#'
#' @param wsdl_model WSDL model from parse_wsdl()
#' @param operation Operation name
#' @param parameters Named list of parameters
#' @param service Optional service name
#' @param port Optional port name
#' @param timeout Timeout in seconds
#' @param verbose Print debug info
#' @return Parsed response
#' @export
call_soap_operation <- function(wsdl_model, operation, parameters = list(),
                                service = NULL, port = NULL,
                                timeout = 60, verbose = FALSE) {

  # Create request
  request <- create_soap_request(wsdl_model, operation, parameters, service, port)

  # Send request
  response <- send_soap_request(request, timeout, verbose)

  # Parse response
  result <- parse_soap_response(response)

  # Handle fault
  if (result$is_fault) {
    warning("SOAP operation returned a fault")
    print(result)
    return(result)
  }

  result
}

# schema support ------------------------------------------------------------------------------

# SOAP Client - Phase 2: XSD Schema Support
# Requires: xml2 package and Phase 1 SOAP client functions

library(xml2)

#' Parse XSD types from WSDL
#'
#' @param types Types section from WSDL model
#' @return Parsed type definitions
#' @export
parse_xsd_types <- function(types) {
  if (length(types) == 0 || length(types$schemas) == 0) {
    return(list(
      simple_types = list(),
      complex_types = list(),
      elements = list()
    ))
  }

  all_simple_types <- list()
  all_complex_types <- list()
  all_elements <- list()

  # Process each schema
  for (schema in types$schemas) {
    schema_node <- schema$node
    target_ns <- schema$target_namespace
    namespaces <- xml_ns(schema_node)

    # Find all simpleType definitions
    simple_type_nodes <- xml_find_all(schema_node, ".//xsd:simpleType", namespaces)
    for (st_node in simple_type_nodes) {
      type_def <- parse_simple_type(st_node, namespaces, target_ns)
      if (!is.null(type_def$name)) {
        all_simple_types[[type_def$name]] <- type_def
      }
    }

    # Find all complexType definitions
    complex_type_nodes <- xml_find_all(schema_node, ".//xsd:complexType", namespaces)
    for (ct_node in complex_type_nodes) {
      type_def <- parse_complex_type(ct_node, namespaces, target_ns)
      if (!is.null(type_def$name)) {
        all_complex_types[[type_def$name]] <- type_def
      }
    }

    # Find all element definitions
    element_nodes <- xml_find_all(schema_node, ".//xsd:element", namespaces)
    for (elem_node in element_nodes) {
      # Only top-level elements (direct children of schema)
      if (xml_name(xml_parent(elem_node)) == "schema") {
        elem_def <- parse_element(elem_node, namespaces, target_ns)
        if (!is.null(elem_def$name)) {
          all_elements[[elem_def$name]] <- elem_def
        }
      }
    }
  }

  list(
    simple_types = all_simple_types,
    complex_types = all_complex_types,
    elements = all_elements
  )
}

#' Parse a simpleType definition
parse_simple_type <- function(node, namespaces, target_ns) {
  name <- xml_attr(node, "name")

  # Check for restriction
  restriction_node <- xml_find_first(node, "./xsd:restriction", namespaces)
  if (!is.na(restriction_node)) {
    base_type <- xml_attr(restriction_node, "base")

    # Parse restrictions
    restrictions <- list()

    # Enumeration values
    enum_nodes <- xml_find_all(restriction_node, "./xsd:enumeration", namespaces)
    if (length(enum_nodes) > 0) {
      restrictions$enumeration <- sapply(enum_nodes, function(n) xml_attr(n, "value"))
    }

    # Pattern
    pattern_node <- xml_find_first(restriction_node, "./xsd:pattern", namespaces)
    if (!is.na(pattern_node)) {
      restrictions$pattern <- xml_attr(pattern_node, "value")
    }

    # Min/Max length
    min_length_node <- xml_find_first(restriction_node, "./xsd:minLength", namespaces)
    if (!is.na(min_length_node)) {
      restrictions$min_length <- as.integer(xml_attr(min_length_node, "value"))
    }

    max_length_node <- xml_find_first(restriction_node, "./xsd:maxLength", namespaces)
    if (!is.na(max_length_node)) {
      restrictions$max_length <- as.integer(xml_attr(max_length_node, "value"))
    }

    # Min/Max inclusive/exclusive
    min_incl_node <- xml_find_first(restriction_node, "./xsd:minInclusive", namespaces)
    if (!is.na(min_incl_node)) {
      restrictions$min_inclusive <- as.numeric(xml_attr(min_incl_node, "value"))
    }

    max_incl_node <- xml_find_first(restriction_node, "./xsd:maxInclusive", namespaces)
    if (!is.na(max_incl_node)) {
      restrictions$max_inclusive <- as.numeric(xml_attr(max_incl_node, "value"))
    }

    return(list(
      name = name,
      type = "simple",
      base_type = base_type,
      restrictions = restrictions,
      namespace = target_ns
    ))
  }

  # Check for list
  list_node <- xml_find_first(node, "./xsd:list", namespaces)
  if (!is.na(list_node)) {
    item_type <- xml_attr(list_node, "itemType")
    return(list(
      name = name,
      type = "list",
      item_type = item_type,
      namespace = target_ns
    ))
  }

  # Check for union
  union_node <- xml_find_first(node, "./xsd:union", namespaces)
  if (!is.na(union_node)) {
    member_types <- xml_attr(union_node, "memberTypes")
    return(list(
      name = name,
      type = "union",
      member_types = strsplit(member_types, "\\s+")[[1]],
      namespace = target_ns
    ))
  }

  NULL
}

#' Parse a complexType definition
parse_complex_type <- function(node, namespaces, target_ns) {
  name <- xml_attr(node, "name")

  # Check for sequence
  sequence_node <- xml_find_first(node, "./xsd:sequence", namespaces)
  if (!is.na(sequence_node)) {
    elements <- parse_sequence_elements(sequence_node, namespaces)
    return(list(
      name = name,
      type = "complex",
      content_type = "sequence",
      elements = elements,
      namespace = target_ns
    ))
  }

  # Check for all
  all_node <- xml_find_first(node, "./xsd:all", namespaces)
  if (!is.na(all_node)) {
    elements <- parse_sequence_elements(all_node, namespaces)
    return(list(
      name = name,
      type = "complex",
      content_type = "all",
      elements = elements,
      namespace = target_ns
    ))
  }

  # Check for choice
  choice_node <- xml_find_first(node, "./xsd:choice", namespaces)
  if (!is.na(choice_node)) {
    elements <- parse_sequence_elements(choice_node, namespaces)
    return(list(
      name = name,
      type = "complex",
      content_type = "choice",
      elements = elements,
      namespace = target_ns
    ))
  }

  # Check for simpleContent
  simple_content_node <- xml_find_first(node, "./xsd:simpleContent", namespaces)
  if (!is.na(simple_content_node)) {
    extension_node <- xml_find_first(simple_content_node, "./xsd:extension", namespaces)
    base_type <- if (!is.na(extension_node)) xml_attr(extension_node, "base") else NULL

    return(list(
      name = name,
      type = "complex",
      content_type = "simpleContent",
      base_type = base_type,
      namespace = target_ns
    ))
  }

  # Check for complexContent
  complex_content_node <- xml_find_first(node, "./xsd:complexContent", namespaces)
  if (!is.na(complex_content_node)) {
    extension_node <- xml_find_first(complex_content_node, "./xsd:extension", namespaces)
    if (!is.na(extension_node)) {
      base_type <- xml_attr(extension_node, "base")
      sequence_node <- xml_find_first(extension_node, "./xsd:sequence", namespaces)
      elements <- if (!is.na(sequence_node)) parse_sequence_elements(sequence_node, namespaces) else list()

      return(list(
        name = name,
        type = "complex",
        content_type = "extension",
        base_type = base_type,
        elements = elements,
        namespace = target_ns
      ))
    }
  }

  NULL
}

#' Parse elements in a sequence/all/choice
parse_sequence_elements <- function(node, namespaces) {
  element_nodes <- xml_find_all(node, "./xsd:element", namespaces)

  elements <- lapply(element_nodes, function(elem_node) {
    parse_element(elem_node, namespaces, NULL)
  })

  names(elements) <- sapply(elements, function(e) e$name)
  elements
}

#' Parse an element definition
parse_element <- function(node, namespaces, target_ns) {
  name <- xml_attr(node, "name")
  type <- xml_attr(node, "type")
  ref <- xml_attr(node, "ref")
  min_occurs <- xml_attr(node, "minOccurs")
  max_occurs <- xml_attr(node, "maxOccurs")

  list(
    name = name,
    type = type,
    ref = ref,
    min_occurs = if (!is.na(min_occurs)) min_occurs else "1",
    max_occurs = if (!is.na(max_occurs)) max_occurs else "1",
    required = is.na(min_occurs) || min_occurs != "0",
    is_array = !is.na(max_occurs) && (max_occurs == "unbounded" || as.integer(max_occurs) > 1),
    namespace = target_ns
  )
}

#' Get XSD base type category
get_xsd_type_category <- function(xsd_type) {
  if (is.null(xsd_type) || is.na(xsd_type)) return("unknown")

  # Remove namespace prefix
  type_name <- sub("^.*:", "", xsd_type)

  # Numeric types
  if (type_name %in% c("int", "integer", "long", "short", "byte",
                       "decimal", "float", "double",
                       "positiveInteger", "negativeInteger",
                       "nonPositiveInteger", "nonNegativeInteger",
                       "unsignedInt", "unsignedLong", "unsignedShort", "unsignedByte")) {
    return("numeric")
  }

  # String types
  if (type_name %in% c("string", "normalizedString", "token", "Name", "NCName",
                       "language", "NMTOKEN", "NMTOKENS", "ID", "IDREF", "IDREFS")) {
    return("string")
  }

  # Boolean
  if (type_name == "boolean") {
    return("boolean")
  }

  # Date/Time types
  if (type_name %in% c("date", "time", "dateTime", "duration",
                       "gYear", "gYearMonth", "gMonth", "gMonthDay", "gDay")) {
    return("datetime")
  }

  # Binary types
  if (type_name %in% c("base64Binary", "hexBinary")) {
    return("binary")
  }

  # URI
  if (type_name == "anyURI") {
    return("uri")
  }

  "unknown"
}

#' Validate parameters against operation schema
#'
#' @param operation Operation definition from WSDL model
#' @param parameters Named list of parameters
#' @param type_system Parsed type system from parse_xsd_types()
#' @return Validation result with errors and warnings
#' @export
validate_parameters <- function(operation, parameters, type_system) {
  result <- list(
    valid = TRUE,
    errors = list(),
    warnings = list()
  )

  if (is.null(operation$input)) {
    return(result)
  }

  input_message <- operation$input$message
  if (is.null(input_message)) {
    return(result)
  }

  # Check each message part
  for (part in input_message$parts) {
    part_name <- part$name

    # Check if parameter provided
    if (!part_name %in% names(parameters)) {
      result$errors <- c(result$errors,
                         sprintf("Required parameter '%s' not provided", part_name))
      result$valid <- FALSE
      next
    }

    param_value <- parameters[[part_name]]

    # Validate based on type
    if (!is.null(part$type)) {
      type_validation <- validate_value_against_type(
        param_value,
        part$type,
        type_system
      )

      if (!type_validation$valid) {
        result$errors <- c(result$errors, type_validation$errors)
        result$warnings <- c(result$warnings, type_validation$warnings)
        result$valid <- FALSE
      }
    }
  }

  result
}

#' Validate a value against an XSD type
validate_value_against_type <- function(value, type_name, type_system) {
  result <- list(valid = TRUE, errors = list(), warnings = list())

  # Remove namespace prefix
  local_type <- sub("^.*:", "", type_name)

  # Check if it's a built-in XSD type
  type_category <- get_xsd_type_category(type_name)

  if (type_category == "numeric") {
    if (!is.numeric(value)) {
      result$errors <- c(result$errors,
                         sprintf("Value must be numeric for type '%s'", local_type))
      result$valid <- FALSE
    }
  } else if (type_category == "boolean") {
    if (!is.logical(value)) {
      result$errors <- c(result$errors,
                         sprintf("Value must be logical for type 'boolean'"))
      result$valid <- FALSE
    }
  } else if (type_category == "string") {
    if (!is.character(value) && !is.numeric(value)) {
      result$warnings <- c(result$warnings,
                           sprintf("Value should be character for type '%s'", local_type))
    }
  }

  # Check if it's a defined simple type
  if (local_type %in% names(type_system$simple_types)) {
    simple_type <- type_system$simple_types[[local_type]]

    # Validate restrictions
    if (!is.null(simple_type$restrictions)) {
      restr <- simple_type$restrictions

      # Enumeration
      if (!is.null(restr$enumeration)) {
        if (!value %in% restr$enumeration) {
          result$errors <- c(result$errors,
                             sprintf("Value '%s' not in allowed enumeration: %s",
                                     value, paste(restr$enumeration, collapse = ", ")))
          result$valid <- FALSE
        }
      }

      # Pattern
      if (!is.null(restr$pattern)) {
        if (!grepl(restr$pattern, value)) {
          result$errors <- c(result$errors,
                             sprintf("Value '%s' does not match pattern: %s",
                                     value, restr$pattern))
          result$valid <- FALSE
        }
      }

      # String length
      if (is.character(value)) {
        str_len <- nchar(value)

        if (!is.null(restr$min_length) && str_len < restr$min_length) {
          result$errors <- c(result$errors,
                             sprintf("String length %d is less than minimum %d",
                                     str_len, restr$min_length))
          result$valid <- FALSE
        }

        if (!is.null(restr$max_length) && str_len > restr$max_length) {
          result$errors <- c(result$errors,
                             sprintf("String length %d exceeds maximum %d",
                                     str_len, restr$max_length))
          result$valid <- FALSE
        }
      }

      # Numeric range
      if (is.numeric(value)) {
        if (!is.null(restr$min_inclusive) && value < restr$min_inclusive) {
          result$errors <- c(result$errors,
                             sprintf("Value %s is less than minimum %s",
                                     value, restr$min_inclusive))
          result$valid <- FALSE
        }

        if (!is.null(restr$max_inclusive) && value > restr$max_inclusive) {
          result$errors <- c(result$errors,
                             sprintf("Value %s exceeds maximum %s",
                                     value, restr$max_inclusive))
          result$valid <- FALSE
        }
      }
    }
  }

  # Check if it's a complex type
  if (local_type %in% names(type_system$complex_types)) {
    complex_type <- type_system$complex_types[[local_type]]

    if (!is.list(value)) {
      result$errors <- c(result$errors,
                         sprintf("Value must be a list for complex type '%s'", local_type))
      result$valid <- FALSE
    } else {
      # Validate elements
      if (complex_type$content_type %in% c("sequence", "all")) {
        for (elem_name in names(complex_type$elements)) {
          elem_def <- complex_type$elements[[elem_name]]

          if (elem_def$required && !elem_name %in% names(value)) {
            result$errors <- c(result$errors,
                               sprintf("Required element '%s' missing in complex type '%s'",
                                       elem_name, local_type))
            result$valid <- FALSE
          }
        }
      }
    }
  }

  result
}

#' Serialize complex type to XML with schema awareness
#'
#' @param data R data structure
#' @param type_name XSD type name
#' @param type_system Parsed type system
#' @return XML element
#' @export
serialize_complex_type <- function(doc, parent, element_name, data, type_name,
                                   type_system, namespace = NULL) {

  # Remove namespace prefix from type
  local_type <- sub("^.*:", "", type_name)

  # Check if it's a complex type we know about
  if (!local_type %in% names(type_system$complex_types)) {
    # Fall back to basic serialization
    create_param_element(doc, parent, element_name, data, namespace)
    return(invisible(parent))
  }

  complex_type <- type_system$complex_types[[local_type]]

  # Create element
  if (!is.null(namespace)) {
    elem <- xml_add_child(parent, element_name, namespace = namespace)
  } else {
    elem <- xml_add_child(parent, element_name)
  }

  # Handle different content types
  if (complex_type$content_type %in% c("sequence", "all")) {
    # Add elements in order
    for (elem_name in names(complex_type$elements)) {
      elem_def <- complex_type$elements[[elem_name]]

      if (elem_name %in% names(data)) {
        value <- data[[elem_name]]

        if (elem_def$is_array && is.list(value)) {
          # Multiple occurrences
          for (item in value) {
            create_param_element(doc, elem, elem_name, item, namespace)
          }
        } else {
          # Single occurrence
          create_param_element(doc, elem, elem_name, value, namespace)
        }
      } else if (elem_def$required) {
        warning(sprintf("Required element '%s' not provided for type '%s'",
                        elem_name, local_type))
      }
    }
  } else if (complex_type$content_type == "simpleContent") {
    # Value goes as text content
    if (is.list(data) && "value" %in% names(data)) {
      xml_text(elem) <- serialize_value(data$value)
    } else {
      xml_text(elem) <- serialize_value(data)
    }
  }

  invisible(parent)
}

#' Enhanced SOAP request creation with schema validation
#'
#' @param wsdl_model WSDL model
#' @param operation_name Operation name
#' @param parameters Parameters
#' @param validate Validate parameters against schema (default: TRUE)
#' @param service_name Service name
#' @param port_name Port name
#' @return SOAP request with validation info
#' @export
create_validated_soap_request <- function(wsdl_model, operation_name, parameters = list(),
                                          validate = TRUE, service_name = NULL,
                                          port_name = NULL) {

  # Parse type system if not already done
  if (is.null(wsdl_model$type_system)) {
    wsdl_model$type_system <- parse_xsd_types(wsdl_model$types)
  }

  # Get operation
  operation <- NULL
  for (pt_name in names(wsdl_model$port_types)) {
    pt <- wsdl_model$port_types[[pt_name]]
    if (operation_name %in% names(pt$operations)) {
      operation <- pt$operations[[operation_name]]
      break
    }
  }

  if (is.null(operation)) {
    stop(sprintf("Operation '%s' not found", operation_name))
  }

  # Validate if requested
  validation_result <- NULL
  if (validate) {
    validation_result <- validate_parameters(operation, parameters, wsdl_model$type_system)

    if (!validation_result$valid) {
      warning("Parameter validation failed:")
      for (error in validation_result$errors) {
        warning(sprintf("  - %s", error))
      }
    }

    if (length(validation_result$warnings) > 0) {
      for (warn in validation_result$warnings) {
        warning(sprintf("  - %s", warn))
      }
    }
  }

  # Create request using Phase 1 function
  request <- create_soap_request(wsdl_model, operation_name, parameters,
                                 service_name, port_name)

  # Add validation info
  request$validation <- validation_result

  request
}

# advanced features ---------------------------------------------------------------------------
# SOAP Client - Phase 3: Advanced Features
# Requires: xml2, httr, base64enc packages and Phases 1 & 2

library(xml2)
library(httr)
library(base64enc)

#' Enhanced SOAP fault handler with detailed parsing
#'
#' @param response httr response object
#' @return Enhanced fault information
#' @export
handle_soap_fault <- function(response) {
  response_text <- content(response, "text", encoding = "UTF-8")

  if (nchar(response_text) == 0) {
    return(list(
      is_fault = TRUE,
      fault_type = "empty_response",
      message = "Empty response received from server",
      http_status = status_code(response)
    ))
  }

  # Try to parse as XML
  response_xml <- tryCatch({
    read_xml(response_text)
  }, error = function(e) {
    return(list(
      is_fault = TRUE,
      fault_type = "parse_error",
      message = sprintf("Failed to parse response: %s", e$message),
      http_status = status_code(response),
      raw_response = response_text
    ))
  })

  if (!inherits(response_xml, "xml_document")) {
    return(response_xml)  # Return the error list
  }

  # Find SOAP namespace
  namespaces <- xml_ns(response_xml)
  soap_ns_name <- NULL
  for (ns_name in names(namespaces)) {
    if (grepl("soap/envelope", namespaces[[ns_name]])) {
      soap_ns_name <- ns_name
      break
    }
  }

  if (is.null(soap_ns_name)) {
    soap_ns_name <- "soap"
    namespaces <- c(namespaces, "soap" = "http://schemas.xmlsoap.org/soap/envelope/")
  }

  # Find fault
  fault_node <- xml_find_first(response_xml,
                               sprintf(".//%s:Body/%s:Fault", soap_ns_name, soap_ns_name),
                               namespaces)

  if (is.na(fault_node)) {
    return(list(is_fault = FALSE))
  }

  # Parse SOAP 1.1 fault
  fault_code <- xml_text(xml_find_first(fault_node, ".//faultcode"))
  fault_string <- xml_text(xml_find_first(fault_node, ".//faultstring"))
  fault_actor <- xml_text(xml_find_first(fault_node, ".//faultactor"))
  fault_detail_node <- xml_find_first(fault_node, ".//detail")

  # Try SOAP 1.2 format if 1.1 fields are empty
  if (is.na(fault_code) || nchar(fault_code) == 0) {
    code_node <- xml_find_first(fault_node, ".//Code/Value")
    fault_code <- if (!is.na(code_node)) xml_text(code_node) else "Unknown"

    reason_node <- xml_find_first(fault_node, ".//Reason/Text")
    fault_string <- if (!is.na(reason_node)) xml_text(reason_node) else "Unknown error"
  }

  # Parse detail if present
  fault_detail <- NULL
  if (!is.na(fault_detail_node)) {
    detail_children <- xml_children(fault_detail_node)
    if (length(detail_children) > 0) {
      fault_detail <- lapply(detail_children, function(child) {
        list(
          name = xml_name(child),
          text = xml_text(child),
          xml = as.character(child)
        )
      })
    }
  }

  # Classify fault type
  fault_type <- classify_fault_code(fault_code)

  list(
    is_fault = TRUE,
    fault_type = fault_type,
    fault_code = fault_code,
    fault_string = fault_string,
    fault_actor = if (!is.na(fault_actor) && nchar(fault_actor) > 0) fault_actor else NULL,
    fault_detail = fault_detail,
    http_status = status_code(response),
    raw_xml = response_xml,
    retriable = is_retriable_fault(fault_type, status_code(response))
  )
}

#' Classify fault code into categories
classify_fault_code <- function(fault_code) {
  if (is.na(fault_code)) return("unknown")

  code_lower <- tolower(fault_code)

  if (grepl("versionmismatch", code_lower)) {
    return("version_mismatch")
  } else if (grepl("mustunderstand", code_lower)) {
    return("must_understand")
  } else if (grepl("client|sender", code_lower)) {
    return("client_error")
  } else if (grepl("server|receiver", code_lower)) {
    return("server_error")
  } else if (grepl("dataencodingunknown", code_lower)) {
    return("encoding_error")
  }

  "unknown"
}

#' Check if fault is retriable
is_retriable_fault <- function(fault_type, http_status) {
  # Server errors and certain HTTP statuses are retriable
  if (fault_type == "server_error") return(TRUE)
  if (http_status %in% c(500, 502, 503, 504)) return(TRUE)

  FALSE
}

#' Add WS-Security UsernameToken header
#'
#' @param envelope XML envelope
#' @param username Username
#' @param password Password
#' @param password_type Type of password ("PasswordText" or "PasswordDigest")
#' @return Modified envelope
#' @export
add_wsse_username_token <- function(envelope, username, password,
                                    password_type = "PasswordText") {

  # Define namespaces
  wsse_ns <- "http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd"
  wsu_ns <- "http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd"

  # Add namespace declarations to envelope
  xml_set_attr(envelope, "xmlns:wsse", wsse_ns)
  xml_set_attr(envelope, "xmlns:wsu", wsu_ns)

  # Find or create SOAP Header
  namespaces <- xml_ns(envelope)
  soap_ns_name <- names(namespaces)[grepl("soap/envelope", namespaces)][1]

  header <- xml_find_first(envelope, sprintf(".//%s:Header", soap_ns_name), namespaces)

  if (is.na(header)) {
    # Create header if it doesn't exist
    body <- xml_find_first(envelope, sprintf(".//%s:Body", soap_ns_name), namespaces)
    header <- xml_add_sibling(body, "soap:Header", .where = "before")
  }

  # Create Security element
  security <- xml_add_child(header, "wsse:Security")
  xml_set_attr(security, "soap:mustUnderstand", "1")

  # Create UsernameToken
  username_token <- xml_add_child(security, "wsse:UsernameToken")
  xml_set_attr(username_token, "wsu:Id", "UsernameToken-1")

  # Add Username
  xml_add_child(username_token, "wsse:Username", username)

  # Add Password
  if (password_type == "PasswordDigest") {
    # Generate nonce and timestamp for digest
    nonce <- as.raw(sample(0:255, 16, replace = TRUE))
    nonce_b64 <- base64encode(nonce)
    created <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

    # Create digest: Base64(SHA1(nonce + created + password))
    digest_input <- c(nonce, charToRaw(created), charToRaw(password))
    digest <- digest::digest(digest_input, algo = "sha1", serialize = FALSE, raw = TRUE)
    password_digest <- base64encode(digest)

    password_elem <- xml_add_child(username_token, "wsse:Password", password_digest)
    xml_set_attr(password_elem, "Type",
                 paste0(wsse_ns, "#PasswordDigest"))

    xml_add_child(username_token, "wsse:Nonce", nonce_b64)
    xml_set_attr(xml_children(username_token)[3], "EncodingType",
                 paste0(wsse_ns, "#Base64Binary"))

    xml_add_child(username_token, "wsu:Created", created)
  } else {
    # Plain text password
    password_elem <- xml_add_child(username_token, "wsse:Password", password)
    xml_set_attr(password_elem, "Type",
                 paste0(wsse_ns, "#PasswordText"))
  }

  envelope
}

#' Add basic HTTP authentication
#'
#' @param username Username
#' @param password Password
#' @return httr authenticate object
#' @export
create_basic_auth <- function(username, password) {
  authenticate(username, password, type = "basic")
}

#' Add custom SOAP header elements
#'
#' @param envelope XML envelope
#' @param headers Named list of header elements
#' @return Modified envelope
#' @export
add_soap_headers <- function(envelope, headers) {
  if (length(headers) == 0) return(envelope)

  # Find SOAP Header
  namespaces <- xml_ns(envelope)
  soap_ns_name <- names(namespaces)[grepl("soap/envelope", namespaces)][1]

  header <- xml_find_first(envelope, sprintf(".//%s:Header", soap_ns_name), namespaces)

  if (is.na(header)) {
    # Create header if it doesn't exist
    body <- xml_find_first(envelope, sprintf(".//%s:Body", soap_ns_name), namespaces)
    header <- xml_add_sibling(body, "soap:Header", .where = "before")
  }

  # Add each header element
  for (header_name in names(headers)) {
    header_value <- headers[[header_name]]

    if (is.list(header_value)) {
      # Complex header
      header_elem <- xml_add_child(header, header_name)
      for (field_name in names(header_value)) {
        xml_add_child(header_elem, field_name, as.character(header_value[[field_name]]))
      }
    } else {
      # Simple header
      xml_add_child(header, header_name, as.character(header_value))
    }
  }

  envelope
}

#' Handle MTOM attachments (basic support)
#'
#' @param soap_body SOAP body content
#' @param attachments List of attachments with name, content, content_type
#' @return MIME multipart message
#' @export
create_mtom_message <- function(soap_body, attachments) {
  boundary <- sprintf("----=_Part_%s",
                      paste(sample(c(0:9, letters, LETTERS), 20, replace = TRUE),
                            collapse = ""))

  # Create SOAP part
  parts <- list()

  # Add SOAP envelope part
  soap_part <- list(
    headers = c(
      "Content-Type" = "application/xop+xml; charset=UTF-8; type=\"text/xml\"",
      "Content-Transfer-Encoding" = "8bit",
      "Content-ID" = "<rootpart@soapui.org>"
    ),
    body = soap_body
  )
  parts <- c(parts, list(soap_part))

  # Add attachment parts
  for (i in seq_along(attachments)) {
    att <- attachments[[i]]
    content_id <- sprintf("<%s>", att$name)

    att_part <- list(
      headers = c(
        "Content-Type" = att$content_type,
        "Content-Transfer-Encoding" = "binary",
        "Content-ID" = content_id
      ),
      body = att$content
    )
    parts <- c(parts, list(att_part))
  }

  # Build multipart message
  message_parts <- character()
  for (part in parts) {
    message_parts <- c(message_parts, paste0("--", boundary))
    for (header_name in names(part$headers)) {
      message_parts <- c(message_parts,
                         sprintf("%s: %s", header_name, part$headers[[header_name]]))
    }
    message_parts <- c(message_parts, "", part$body, "")
  }
  message_parts <- c(message_parts, paste0("--", boundary, "--"))

  list(
    content_type = sprintf('multipart/related; type="application/xop+xml"; boundary="%s"',
                           boundary),
    body = paste(message_parts, collapse = "\r\n")
  )
}

#' Send SOAP request with retry logic
#'
#' @param soap_request Request from create_soap_request()
#' @param max_retries Maximum number of retries
#' @param retry_delay Initial delay in seconds
#' @param backoff_factor Exponential backoff factor
#' @param auth Optional authentication (basic auth or custom)
#' @param custom_headers Additional HTTP headers
#' @param timeout Timeout in seconds
#' @param verbose Print debug info
#' @return Response object
#' @export
send_soap_request_with_retry <- function(soap_request, max_retries = 3,
                                         retry_delay = 1, backoff_factor = 2,
                                         auth = NULL, custom_headers = NULL,
                                         timeout = 60, verbose = FALSE) {

  endpoint <- soap_request$endpoint
  if (is.null(endpoint) || is.na(endpoint)) {
    stop("No endpoint address specified")
  }

  # Prepare headers
  content_type <- if (soap_request$soap_version == "1.2") {
    sprintf("application/soap+xml; charset=utf-8; action=\"%s\"",
            ifelse(is.null(soap_request$soap_action), "", soap_request$soap_action))
  } else {
    "text/xml; charset=utf-8"
  }

  headers <- c("Content-Type" = content_type)

  # Add SOAPAction for SOAP 1.1
  if (soap_request$soap_version == "1.1") {
    soap_action <- if (!is.null(soap_request$soap_action) && !is.na(soap_request$soap_action)) {
      sprintf('"%s"', soap_request$soap_action)
    } else {
      '""'
    }
    headers <- c(headers, "SOAPAction" = soap_action)
  }

  # Add custom headers
  if (!is.null(custom_headers)) {
    headers <- c(headers, custom_headers)
  }

  # Retry loop
  attempt <- 1
  current_delay <- retry_delay
  last_error <- NULL

  while (attempt <= max_retries + 1) {
    if (verbose && attempt > 1) {
      cat(sprintf("\n=== Retry Attempt %d/%d (delay: %.1fs) ===\n",
                  attempt - 1, max_retries, current_delay))
    }

    # Send request
    response <- tryCatch({
      req <- POST(
        url = endpoint,
        body = soap_request$xml,
        add_headers(.headers = headers),
        timeout(timeout),
        encode = "raw"
      )

      # Add auth if provided
      if (!is.null(auth)) {
        req <- POST(
          url = endpoint,
          body = soap_request$xml,
          add_headers(.headers = headers),
          auth,
          timeout(timeout),
          encode = "raw"
        )
      }

      req
    }, error = function(e) {
      last_error <<- e
      NULL
    })

    # Check if request succeeded
    if (!is.null(response)) {
      # Check for retriable errors
      fault_info <- handle_soap_fault(response)

      if (fault_info$is_fault && fault_info$retriable && attempt <= max_retries) {
        if (verbose) {
          cat(sprintf("Retriable fault encountered: %s\n", fault_info$fault_string))
        }

        # Wait before retry
        Sys.sleep(current_delay)
        current_delay <- current_delay * backoff_factor
        attempt <- attempt + 1
        next
      }

      # Success or non-retriable error
      return(response)
    }

    # Network error
    if (attempt <= max_retries) {
      if (verbose) {
        cat(sprintf("Request failed: %s\n", last_error$message))
      }
      Sys.sleep(current_delay)
      current_delay <- current_delay * backoff_factor
      attempt <- attempt + 1
    } else {
      stop(sprintf("Request failed after %d attempts: %s",
                   max_retries + 1, last_error$message))
    }
  }

  stop("Maximum retries exceeded")
}

#' Advanced SOAP operation call with all features
#'
#' @param wsdl_model WSDL model
#' @param operation Operation name
#' @param parameters Parameters
#' @param service Service name
#' @param port Port name
#' @param auth Authentication (list with type and credentials)
#' @param soap_headers Custom SOAP headers
#' @param http_headers Custom HTTP headers
#' @param validate Validate parameters
#' @param max_retries Maximum retries
#' @param timeout Timeout in seconds
#' @param verbose Verbose output
#' @return Parsed response
#' @export
call_soap_operation_advanced <- function(wsdl_model, operation, parameters = list(),
                                         service = NULL, port = NULL,
                                         auth = NULL, soap_headers = NULL,
                                         http_headers = NULL, validate = TRUE,
                                         max_retries = 3, timeout = 60,
                                         verbose = FALSE) {

  # Create request with validation
  if (validate && !is.null(wsdl_model$type_system)) {
    request <- create_validated_soap_request(
      wsdl_model, operation, parameters, validate, service, port
    )
  } else {
    request <- create_soap_request(
      wsdl_model, operation, parameters, service, port
    )
  }

  # Parse envelope for modifications
  envelope <- read_xml(request$xml)

  # Add authentication to SOAP header
  if (!is.null(auth)) {
    if (auth$type == "wsse") {
      envelope <- add_wsse_username_token(
        envelope,
        auth$username,
        auth$password,
        ifelse(is.null(auth$password_type), "PasswordText", auth$password_type)
      )
      request$xml <- as.character(envelope)
    } else if (auth$type == "basic") {
      auth <- create_basic_auth(auth$username, auth$password)
    }
  }

  # Add custom SOAP headers
  if (!is.null(soap_headers)) {
    envelope <- add_soap_headers(envelope, soap_headers)
    request$xml <- as.character(envelope)
  }

  if (verbose) {
    cat("\n=== Advanced SOAP Request ===\n")
    cat(sprintf("Operation: %s\n", operation))
    cat(sprintf("Endpoint: %s\n", request$endpoint))
    cat(sprintf("Authentication: %s\n",
                ifelse(is.null(auth), "None", auth$type)))
    cat(sprintf("Validation: %s\n", ifelse(validate, "Enabled", "Disabled")))
    cat(sprintf("Max Retries: %d\n", max_retries))
    cat("\n")
  }

  # Send request with retry
  response <- send_soap_request_with_retry(
    request,
    max_retries = max_retries,
    auth = auth,
    custom_headers = http_headers,
    timeout = timeout,
    verbose = verbose
  )

  # Handle fault
  fault_info <- handle_soap_fault(response)
  if (fault_info$is_fault) {
    warning(sprintf("SOAP fault: %s", fault_info$fault_string))
    return(fault_info)
  }

  # Parse response
  result <- parse_soap_response(response)

  result
}

# example -------------------------------------------------------------------------------------
wsdl_doc <- xml2::read_xml("./tests/testthat/testdata/valid_wsdls/BDTInteractive.xml")
wsdl <- parse_wsdl(wsdl_doc)

call_soap_operation(wsdl, operation = "GetCurrentVersion")
