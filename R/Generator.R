#' Generator Class
#'
#' Generates a static R package from a WSDL.
#'
#' @importFrom R6 R6Class
#' @export
Generator <- R6::R6Class(
    "Generator",
    public = list(
        wsdl = NULL,

        initialize = function(wsdl) {
            self$wsdl <- wsdl
        },

        generate_package = function(path, package_name) {
            # Create directories
            pkg_path <- file.path(path, package_name)
            if (!dir.exists(pkg_path)) {
                dir.create(pkg_path, recursive = TRUE)
            }
            if (!dir.exists(file.path(pkg_path, "R"))) {
                dir.create(file.path(pkg_path, "R"), recursive = TRUE)
            }

            # Create DESCRIPTION
            self$write_description(pkg_path, package_name)

            # Create R/utils.R (helper functions)
            self$write_utils(pkg_path)

            # Create R/types.R (constructors)
            self$write_types(pkg_path)

            # Create R/client.R (operations)
            self$write_client(pkg_path)

            # Document package
            message("Running roxygen2::roxygenize...")
            tryCatch(
                {
                    if (requireNamespace("roxygen2", quietly = TRUE)) {
                        roxygen2::roxygenize(
                            pkg_path,
                            roclets = c("rd", "namespace", "collate")
                        )
                        message("Documentation generated.")
                    } else {
                        message(
                            "roxygen2 not installed. Skipping documentation."
                        )
                    }
                },
                error = function(e) {
                    message("Could not run roxygenize: ", e$message)
                    message(
                        "Please run roxygen2::roxygenize() manually on the generated package."
                    )
                }
            )

            message(paste("Package generated at:", pkg_path))
        },

        write_description = function(path, name) {
            desc <- paste0(
                "Package: ",
                name,
                "\n",
                "Title: SOAP Client for ",
                name,
                "\n",
                "Version: 0.1.0\n",
                "Description: Auto-generated SOAP client.\n",
                "Imports: httr, xml2, R6\n",
                "License: MIT\n",
                "Encoding: UTF-8\n",
                "Roxygen: list(markdown = TRUE)\n",
                "RoxygenNote: 7.2.3\n"
            )
            writeLines(desc, file.path(path, "DESCRIPTION"))
        },

        write_utils = function(path) {
            code <- '
build_xml_node <- function(parent, name, value) {
  if (is.null(value) || (is.atomic(value) && length(value) == 1 && is.na(value))) return()
  
  # Handle lists (maxOccurs="unbounded")
  if (is.list(value) && is.null(names(value))) {
    for (item in value) {
      build_xml_node(parent, name, item)
    }
    return()
  }
  
  # Create node
  # Check if name is namespaced
  node_name <- if (grepl(":", name)) name else paste0("tns:", name)
  child <- xml2::xml_add_child(parent, node_name)
  
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
  doc <- tryCatch({
    content_type <- httr::headers(response)$`Content-Type`
    if (!is.null(content_type) && !grepl("xml", content_type, ignore.case = TRUE)) {
       NULL
    } else {
       xml2::read_xml(httr::content(response, "text", encoding = "UTF-8"))
    }
  }, error = function(e) NULL)
  
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
  if (inherits(fault_node, "xml_missing")) {
    fault_node <- tryCatch(
      xml2::xml_find_first(doc, ".//env:Fault", ns),
      warning = function(w) xml2::xml_missing()
    )
  }
  
  if (inherits(fault_node, "xml_missing")) {
    # Fallback for when prefix is different but local name is Fault
    fault_node <- xml2::xml_find_first(doc, ".//*[local-name()=\'Fault\']")
  }
  
  if (!inherits(fault_node, "xml_missing")) {
    # Extract Fault Code
    fault_code <- xml2::xml_text(xml2::xml_find_first(
      fault_node,
      ".//*[local-name()=\'faultcode\' or local-name()=\'Code\']"
    ))
    
    # Extract Fault String/Reason
    fault_string <- xml2::xml_text(xml2::xml_find_first(
      fault_node,
      ".//*[local-name()=\'faultstring\' or local-name()=\'Reason\']"
    ))
    
    # Extract Detail
    fault_detail <- xml2::xml_text(xml2::xml_find_first(
      fault_node,
      ".//*[local-name()=\'detail\' or local-name()=\'Detail\']"
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
'
            writeLines(code, file.path(path, "R", "utils.R"))
        },

        write_types = function(path) {
            types <- self$wsdl$get_type_definitions()
            lines <- c()

            for (name in names(types)) {
                def <- types[[name]]
                if (def$kind == "complex") {
                    # Generate constructor
                    args <- c()
                    body <- c("  obj <- list(")
                    param_docs <- c()

                    # Attributes
                    attr_names <- names(def$attributes)
                    for (attr_name in attr_names) {
                        args <- c(args, paste0(attr_name, " = NA"))
                        body <- c(
                            body,
                            paste0("    ", attr_name, " = ", attr_name, ",")
                        )

                        attr_doc <- def$attributes[[attr_name]]$documentation
                        doc_text <- if (!is.null(attr_doc)) {
                            attr_doc
                        } else {
                            paste("Attribute", attr_name)
                        }
                        param_docs <- c(
                            param_docs,
                            paste0("#' @param ", attr_name, " ", doc_text)
                        )
                    }

                    # Elements
                    for (elem_name in names(def$elements)) {
                        elem <- def$elements[[elem_name]]
                        is_unbounded <- !is.null(elem$maxOccurs) &&
                            !is.na(elem$maxOccurs) &&
                            elem$maxOccurs == "unbounded"
                        default_val <- if (is_unbounded) "list()" else "NA"
                        args <- c(args, paste0(elem_name, " = ", default_val))
                        body <- c(
                            body,
                            paste0("    ", elem_name, " = ", elem_name, ",")
                        )

                        elem_doc <- elem$documentation
                        doc_text <- if (!is.null(elem_doc)) {
                            elem_doc
                        } else {
                            paste("Element", elem_name)
                        }
                        param_docs <- c(
                            param_docs,
                            paste0("#' @param ", elem_name, " ", doc_text)
                        )
                    }

                    # Remove trailing comma
                    if (length(body) > 1) {
                        body[length(body)] <- sub(",$", "", body[length(body)])
                    }
                    body <- c(body, "  )")

                    # Add metadata
                    body <- c(
                        body,
                        paste0('  class(obj) <- c("soap_object", "list")')
                    )
                    body <- c(
                        body,
                        paste0('  attr(obj, "soap_type") <- "', name, '"')
                    )

                    if (length(attr_names) > 0) {
                        attrs_str <- paste(
                            paste0('"', attr_names, '"'),
                            collapse = ", "
                        )
                        body <- c(
                            body,
                            paste0(
                                '  attr(obj, "soap_attrs") <- c(',
                                attrs_str,
                                ')'
                            )
                        )
                    }

                    body <- c(body, "  return(obj)")

                    doc_str <- if (!is.null(def$documentation)) {
                        def$documentation
                    } else {
                        paste("Constructor for", name)
                    }

                    func_def <- paste0(
                        "#' ",
                        doc_str,
                        "\n",
                        paste(param_docs, collapse = "\n"),
                        "\n",
                        "#' @export\n",
                        "new_",
                        name,
                        " <- function(",
                        paste(args, collapse = ", "),
                        ") {\n",
                        paste(body, collapse = "\n"),
                        "\n",
                        "}\n"
                    )
                    lines <- c(lines, func_def)
                }
            }
            writeLines(lines, file.path(path, "R", "types.R"))
        },

        write_client = function(path) {
            # We need to find a service and port
            services <- self$wsdl$get_services()
            if (length(services) == 0) {
                return()
            }
            service <- services[[1]]
            if (length(service$ports) == 0) {
                return()
            }
            port <- service$ports[[1]]

            ops <- self$wsdl$get_operations(port$binding)

            lines <- c()

            # Helper to create the client object that holds the endpoint
            lines <- c(
                lines,
                "#' Create Client",
                "#' @param url The service endpoint URL. Defaults to the WSDL location.",
                "#' @param config A list of httr configuration options (e.g., authentication).",
                "#' @param transport A custom transport function for mocking. Defaults to NULL.",
                "#' @return A client object containing the URL, httr handle, headers, config, and transport.",
                "#' @export",
                "new_client <- function(url = NULL, config = list(), transport = NULL) {",
                paste0('  if (is.null(url)) url <- "', port$location, '"'),
                "  handle <- httr::handle(url)",
                "  list(url = url, handle = handle, headers = list(), config = config, transport = transport)",
                "}",
                "",
                "#' Add SOAP Header",
                "#' @param client The client object.",
                "#' @param header An xml_node representing the header.",
                "#' @return The modified client object.",
                "#' @export",
                "add_header <- function(client, header) {",
                "  client$headers <- c(client$headers, list(header))",
                "  client",
                "}"
            )

            tns <- self$wsdl$get_target_namespace()

            for (op_name in names(ops)) {
                op <- ops[[op_name]]

                type_def <- self$wsdl$get_type_definitions()[[op_name]]

                args <- c("client")
                arg_assignments <- c()
                param_docs <- c(
                    "#' @param client The client object created by new_client()"
                )

                if (!is.null(type_def)) {
                    for (elem_name in names(type_def$elements)) {
                        args <- c(args, paste0(elem_name, " = NA"))
                        arg_assignments <- c(
                            arg_assignments,
                            paste0("  args[['", elem_name, "']] <- ", elem_name)
                        )

                        elem <- type_def$elements[[elem_name]]
                        elem_doc <- elem$documentation
                        doc_text <- if (!is.null(elem_doc)) {
                            elem_doc
                        } else {
                            paste("Parameter", elem_name)
                        }
                        param_docs <- c(
                            param_docs,
                            paste0("#' @param ", elem_name, " ", doc_text)
                        )
                    }
                }

                func_body <- c(
                    paste0("  op_name <- \"", op_name, "\""),
                    paste0("  action <- \"", op$action, "\""),
                    "  args <- list()",
                    arg_assignments,
                    "",
                    "  # Construct Envelope",
                    "  envelope <- xml2::xml_new_root(\"soap:Envelope\",",
                    "    \"xmlns:soap\" = \"http://schemas.xmlsoap.org/soap/envelope/\",",
                    "    \"xmlns:xsi\" = \"http://www.w3.org/2001/XMLSchema-instance\",",
                    "    \"xmlns:xsd\" = \"http://www.w3.org/2001/XMLSchema\"",
                    "  )",
                    "",
                    "  # Add Headers",
                    "  if (length(client$headers) > 0) {",
                    "    header_node <- xml2::xml_add_child(envelope, \"soap:Header\")",
                    "    for (header in client$headers) {",
                    "      xml2::xml_add_child(header_node, header)",
                    "    }",
                    "  }",
                    "",
                    "  body <- xml2::xml_add_child(envelope, \"soap:Body\")",
                    "  op_node <- xml2::xml_add_child(body, op_name)",
                    paste0(
                        "  xml2::xml_attr(op_node, \"xmlns\") <- \"",
                        tns,
                        "\""
                    ),
                    "",
                    "  for (arg_name in names(args)) {",
                    "    build_xml_node(op_node, arg_name, args[[arg_name]])",
                    "  }",
                    "",
                    "  if (!is.null(client$transport)) {",
                    "    response <- client$transport(client$url, action, as.character(envelope), client$config, client$handle)",
                    "  } else {",
                    "    response <- httr::POST(",
                    "      url = client$url,",
                    "      handle = client$handle,",
                    "      httr::add_headers(SOAPAction = action),",
                    "      httr::content_type(\"text/xml; charset=utf-8\"),",
                    "      body = as.character(envelope),",
                    "      config = client$config",
                    "    )",
                    "  }",
                    "  handle_fault(response)",
                    "  httr::content(response, as = \"parsed\", encoding = \"UTF-8\")"
                )

                doc_str <- if (!is.null(op$documentation)) {
                    op$documentation
                } else {
                    op_name
                }

                func_def <- paste0(
                    "#' ",
                    doc_str,
                    "\n",
                    paste(param_docs, collapse = "\n"),
                    "\n",
                    "#' @export\n",
                    paste0(
                        op_name,
                        " <- function(",
                        paste(args, collapse = ", "),
                        ") {\n"
                    ),
                    paste(func_body, collapse = "\n"),
                    "\n",
                    "}\n"
                )
                lines <- c(lines, func_def)
            }
            writeLines(lines, file.path(path, "R", "client.R"))
        }
    )
)
