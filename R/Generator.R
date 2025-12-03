#' Generator Class
#'
#' Generates a static R package from a WSDL.
#'
#' @importFrom R6 R6Class
#' @export
Generator <- R6::R6Class(
  "Generator",
  public = list(
    #' @field wsdl The WSDLParser instance.
    wsdl = NULL,

    #' @description
    #' Initialize the Generator.
    #' @param wsdl A WSDLParser instance.
    initialize = function(wsdl) {
      self$wsdl <- wsdl
    },

    #' @description
    #' Generate the R package.
    #' @param path The output directory path.
    #' @param package_name The name of the package to generate.
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

      # Create README.md
      self$write_readme(pkg_path, package_name)

      # Create R/utils.R (helper functions)
      self$write_utils(pkg_path)

      # Create R/types.R (constructors)
      self$write_types(pkg_path)

      # Create R/schema.R (type definitions for deserialization)
      self$write_schema(pkg_path)

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

    #' @description
    #' Write the DESCRIPTION file.
    #' @param path The package directory path.
    #' @param name The package name.
    write_description = function(path, name) {
      d <- desc::description$new("!new")
      d$set("Package", name)
      d$set("Title", paste("SOAP Client for", name))
      d$set("Version", "0.1.0")
      d$set("Description", "Auto-generated SOAP client.")
      d$set("Imports", "httr, xml2, R6")
      d$set("License", "MIT")
      d$set("Encoding", "UTF-8")
      d$set("Roxygen", "list(markdown = TRUE)")
      d$set("RoxygenNote", "7.2.3")
      d$write(file.path(path, "DESCRIPTION"))
    },

    #' @description
    #' Write the README.md file.
    #' @param path The package directory path.
    #' @param name The package name.
    write_readme = function(path, name) {
      # Get operations list
      ops <- self$wsdl$get_operations(
        self$wsdl$get_services()[[1]]$ports[[1]]$binding
      )
      op_list <- paste0("- `", names(ops), "`", collapse = "\n")

      # Generate Example for First Operation
      example_code <- ""
      if (length(ops) > 0) {
        op_name <- names(ops)[1]
        op <- ops[[1]]

        # Get arguments
        type_def <- self$wsdl$get_type_definitions()[[op_name]]
        args_str <- ""

        if (!is.null(type_def)) {
          arg_parts <- c()
          for (elem_name in names(type_def$elements)) {
            elem <- type_def$elements[[elem_name]]

            # Determine dummy value based on type
            dummy_val <- "NULL"
            type_info <- self$wsdl$get_type_definitions()[[elem$type]]

            if (
              !is.null(type_info) &&
                type_info$kind == "simple" &&
                !is.null(type_info$values)
            ) {
              # Enum
              dummy_val <- paste0('"', type_info$values[1], '"')
            } else if (
              grepl(
                "int|long|short|byte|decimal|float|double",
                elem$type,
                ignore.case = TRUE
              )
            ) {
              dummy_val <- "0"
            } else if (grepl("boolean", elem$type, ignore.case = TRUE)) {
              dummy_val <- "TRUE"
            } else if (
              grepl("dateTime|date|time", elem$type, ignore.case = TRUE)
            ) {
              dummy_val <- paste0('"', Sys.Date(), '"')
            } else {
              dummy_val <- '"string"'
            }

            arg_parts <- c(arg_parts, paste0(elem_name, " = ", dummy_val))
          }
          args_str <- paste(arg_parts, collapse = ", ")
        }

        example_code <- paste0(
          "### \U1F4A1 Example: ",
          op_name,
          "\n\n",
          "```r\n",
          "# Call the ",
          op_name,
          " operation\n",
          "result <- client$",
          op_name,
          "(",
          args_str,
          ")\n",
          "print(result)\n",
          "```\n\n"
        )
      }

      readme <- paste0(
        "# ",
        name,
        "\n\n",
        "<!-- badges: start -->\n",
        "[![Generated by soapr](https://img.shields.io/badge/generated_by-soapr-blue.svg)](https://github.com/zachzIAM/soapr)\n",
        "<!-- badges: end -->\n\n",
        "Auto-generated SOAP client for **",
        name,
        "**.\n\n",
        "## \U2139\UFE0F Service Information\n\n",
        "- **WSDL Location**: `",
        self$wsdl$get_services()[[1]]$ports[[1]]$location,
        "`\n",
        "- **Target Namespace**: `",
        self$wsdl$get_target_namespace(),
        "`\n\n",
        "## \U1F4E6 Installation\n\n",
        "```r\n",
        "# Install from local directory\n",
        "devtools::install(\".\", build_vignettes = TRUE)\n",
        "```\n\n",
        "## \U1F680 Usage\n\n",
        "### Initialization\n\n",
        "Create a new client instance:\n\n",
        "```r\n",
        "library(",
        name,
        ")\n",
        "client <- new_client()\n",
        "```\n\n",
        example_code,
        "### \U1F4CB Available Operations\n\n",
        op_list,
        "\n\n",
        "### \U2699\UFE0F Configuration\n\n",
        "You can pass `httr` configuration options to `new_client` via the `config` argument.\n\n",
        "```r\n",
        "# Basic Authentication\n",
        "client <- new_client(\n",
        "  config = httr::authenticate(\"user\", \"password\")\n",
        ")\n",
        "```\n\n",
        "### \U1F512 WS-Security\n\n",
        "Add WS-Security headers using `add_header`.\n\n",
        "```r\n",
        "header <- xml2::read_xml('<wsse:Security>...</wsse:Security>')\n",
        "client$add_header(header)\n",
        "```\n\n",
        "### \U1F9EA Mocking\n\n",
        "Inject a custom transport function for testing.\n\n",
        "```r\n",
        "mock_transport <- function(url, action, body, config, handle) {\n",
        "  # Return a mock response\n",
        "  structure(\n",
        "    list(\n",
        "      status_code = 200,\n",
        "      content = charToRaw(\"<response>...</response>\"),\n",
        "      headers = list(\"Content-Type\" = \"text/xml\")\n",
        "    ),\n",
        "    class = \"response\"\n",
        "  )\n",
        "}\n",
        "client <- new_client(transport = mock_transport)\n",
        "```\n"
      )
      writeLines(readme, file.path(path, "README.md"))
    },

    #' @description
    #' Write the utils.R file.
    #' @param path The package directory path.
    write_utils = function(path) {
      template_path <- system.file("templates/utils.R", package = "soapr")
      if (template_path == "") {
        # Fallback for dev mode
        template_path <- file.path(getwd(), "inst/templates/utils.R")
      }
      file.copy(
        template_path,
        file.path(path, "R", "utils.R"),
        overwrite = TRUE
      )
    },

    #' @description
    #' Write the types.R file.
    #' @param path The package directory path.
    write_types = function(path) {
      types <- self$wsdl$get_type_definitions()

      lines <- c()
      lines <- c(lines, "#' Type Factory")
      lines <- c(
        lines,
        "#' @description Factory for creating SOAP complex types."
      )
      lines <- c(lines, "#' @importFrom R6 R6Class")
      lines <- c(lines, "#' @export")
      lines <- c(lines, "TypeFactory <- R6::R6Class(")
      lines <- c(lines, '  "TypeFactory",')
      lines <- c(lines, "  public = list(")

      methods <- c()

      for (name in names(types)) {
        def <- types[[name]]
        if (def$kind == "complex") {
          # Generate constructor method
          args <- c()
          body <- c("      obj <- list(")
          param_docs <- c()

          # Attributes
          attr_names <- names(def$attributes)
          for (attr_name in attr_names) {
            args <- c(args, paste0(attr_name, " = NA"))
            body <- c(
              body,
              paste0("        ", attr_name, " = ", attr_name, ",")
            )

            attr_doc <- def$attributes[[attr_name]]$documentation
            doc_text <- if (!is.null(attr_doc)) {
              attr_doc
            } else {
              paste("Attribute", attr_name)
            }
            param_docs <- c(
              param_docs,
              paste0("    #' @param ", attr_name, " ", doc_text)
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
              paste0("        ", elem_name, " = ", elem_name, ",")
            )

            elem_doc <- elem$documentation
            doc_text <- if (!is.null(elem_doc)) {
              elem_doc
            } else {
              paste("Element", elem_name)
            }
            param_docs <- c(
              param_docs,
              paste0("    #' @param ", elem_name, " ", doc_text)
            )
          }

          # Remove trailing comma
          if (length(body) > 1) {
            body[length(body)] <- sub(",$", "", body[length(body)])
          }
          body <- c(body, "      )")

          # Add metadata
          body <- c(
            body,
            paste0('      class(obj) <- c("soap_object", "list")')
          )
          body <- c(
            body,
            paste0('      attr(obj, "soap_type") <- "', name, '"')
          )

          if (length(attr_names) > 0) {
            attrs_str <- paste(
              paste0('"', attr_names, '"'),
              collapse = ", "
            )
            body <- c(
              body,
              paste0(
                '      attr(obj, "soap_attrs") <- c(',
                attrs_str,
                ")"
              )
            )
          }

          body <- c(body, "      return(obj)")

          doc_str <- if (!is.null(def$documentation)) {
            def$documentation
          } else {
            paste("Constructor for", name)
          }

          method_def <- paste0(
            "    #' @description ",
            doc_str,
            "\n",
            paste(param_docs, collapse = "\n"),
            "\n",
            "    ",
            name,
            " = function(",
            paste(args, collapse = ", "),
            ") {\n",
            paste(body, collapse = "\n"),
            "\n    }"
          )
          methods <- c(methods, method_def)
        }
      }

      lines <- c(lines, paste(methods, collapse = ",\n"))
      lines <- c(lines, "  )")
      lines <- c(lines, ")")

      writeLines(lines, file.path(path, "R", "types.R"))
    },

    #' @description
    #' Write the schema.R file.
    #' @param path The package directory path.
    write_schema = function(path) {
      # We need to serialize the type definitions to a file so they can be loaded at runtime
      # Since they are just lists, we can use dput or similar, but let's make it cleaner
      # We'll just dump the structure into a function that returns it

      types <- self$wsdl$get_type_definitions()

      # We can't easily dput the whole environment/R6 object, but types is a list of lists
      # so dput should work reasonably well.

      lines <- c(
        "#' Get Schema",
        "#' @return A list of type definitions.",
        "#' @export",
        "get_schema <- function() {"
      )

      # Use capture.output to get the dput output
      types_code <- capture.output(dput(types))
      lines <- c(lines, paste0("  ", types_code))
      lines <- c(lines, "}")

      writeLines(lines, file.path(path, "R", "schema.R"))
    },

    #' @description
    #' Write the client.R file.
    #' @param path The package directory path.
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

      detached_docs <- c()
      public_members <- c()

      # --- Fields ---
      public_members <- c(
        public_members,
        "    #' @field url The service endpoint URL.",
        "    url = NULL",
        "    #' @field handle The httr handle.",
        "    handle = NULL",
        "    #' @field headers List of SOAP headers.",
        "    headers = list()",
        "    #' @field config List of httr configuration options.",
        "    config = list()",
        "    #' @field transport Custom transport function.",
        "    transport = NULL",
        "    #' @field types Factory for creating complex types.",
        "    types = NULL"
      )

      # --- Initialize ---
      init_body <- c(
        paste0('      if (is.null(url)) url <- "', port$location, '"'),
        "      self$url <- url",
        "      self$handle <- httr::handle(url)",
        "      self$config <- config",
        "      self$transport <- transport",
        "      self$types <- TypeFactory$new()"
      )

      init_def <- paste0(
        "    #' @description\n",
        "    #' Initialize the client.\n",
        "    #' @param url The service endpoint URL.\n",
        "    #' @param config A list of httr configuration options.\n",
        "    #' @param transport A custom transport function.\n",
        "    initialize = function(url = NULL, config = list(), transport = NULL) {\n",
        paste(init_body, collapse = "\n"),
        "\n    }"
      )
      public_members <- c(public_members, init_def)

      # --- Add Header ---
      add_header_def <- paste0(
        "    #' @description\n",
        "    #' Add a SOAP header.\n",
        "    #' @param header An xml_node representing the header.\n",
        "    add_header = function(header) {\n",
        "      self$headers <- c(self$headers, list(header))\n",
        "      invisible(self)\n",
        "    }"
      )
      public_members <- c(public_members, add_header_def)

      tns <- self$wsdl$get_target_namespace()

      for (op_name in names(ops)) {
        op <- ops[[op_name]]
        type_def <- self$wsdl$get_type_definitions()[[op_name]]

        # --- Detached Documentation ---
        doc_str <- if (!is.null(op$documentation)) {
          op$documentation
        } else {
          paste("Operation", op_name)
        }

        args_doc <- c()
        if (!is.null(type_def)) {
          for (elem_name in names(type_def$elements)) {
            elem <- type_def$elements[[elem_name]]
            elem_doc <- elem$documentation
            doc_text <- if (!is.null(elem_doc)) {
              elem_doc
            } else {
              paste("Parameter", elem_name)
            }
            args_doc <- c(
              args_doc,
              paste0("#' @param ", elem_name, " ", doc_text)
            )
          }
        }

        detached_doc <- c(
          paste0("#' ", doc_str),
          "#' @description",
          paste0("#' Call the ", op_name, " operation."),
          args_doc,
          paste0("#' @usage client$", op_name, "(...)"),
          paste0("#' @name ", op_name),
          paste0("#' @aliases ", op_name),
          "NULL",
          ""
        )
        detached_docs <- c(detached_docs, detached_doc)

        # --- Method Definition ---
        args <- c()
        arg_assignments <- c()
        arg_validations <- c()

        if (!is.null(type_def)) {
          for (elem_name in names(type_def$elements)) {
            elem <- type_def$elements[[elem_name]]

            # Check if type is an enum
            type_info <- self$wsdl$get_type_definitions()[[elem$type]]
            if (
              !is.null(type_info) &&
                type_info$kind == "simple" &&
                !is.null(type_info$values)
            ) {
              # It's an enum!
              default_val <- paste0(
                "c(",
                paste(paste0('"', type_info$values, '"'), collapse = ", "),
                ")"
              )
              args <- c(args, paste0(elem_name, " = ", default_val))
              arg_validations <- c(
                arg_validations,
                paste0("      ", elem_name, " <- match.arg(", elem_name, ")")
              )
            } else {
              args <- c(args, paste0(elem_name, " = NA"))
            }

            arg_assignments <- c(
              arg_assignments,
              paste0("      args[['", elem_name, "']] <- ", elem_name)
            )
          }
        }

        func_body <- c(
          paste0("      op_name <- \"", op_name, "\""),
          paste0("      action <- \"", op$action, "\""),
          arg_validations,
          "      args <- list()",
          arg_assignments,
          "",
          "      # Construct Envelope",
          "      envelope <- xml2::xml_new_root(\"soap:Envelope\",",
          "        \"xmlns:soap\" = \"http://schemas.xmlsoap.org/soap/envelope/\",",
          "        \"xmlns:xsi\" = \"http://www.w3.org/2001/XMLSchema-instance\",",
          "        \"xmlns:xsd\" = \"http://www.w3.org/2001/XMLSchema\",",
          paste0("        \"xmlns:tns\" = \"", tns, "\""),
          "      )",
          "",
          "      # Add Headers",
          "      if (length(self$headers) > 0) {",
          "        header_node <- xml2::xml_add_child(envelope, \"soap:Header\")",
          "        for (header in self$headers) {",
          "          xml2::xml_add_child(header_node, header)",
          "        }",
          "      }",
          "",
          "      body <- xml2::xml_add_child(envelope, \"soap:Body\")",
          "      op_node <- xml2::xml_add_child(body, op_name)",
          paste0(
            "      xml2::xml_attr(op_node, \"xmlns\") <- \"",
            tns,
            "\""
          ),
          "",
          "      for (arg_name in names(args)) {",
          "        build_xml_node(op_node, arg_name, args[[arg_name]])",
          "      }",
          "",
          "      if (!is.null(self$transport)) {",
          "        response <- self$transport(self$url, action, as.character(envelope), self$config, self$handle)",
          "      } else {",
          "        response <- httr::POST(",
          "          url = self$url,",
          "          handle = self$handle,",
          "          httr::add_headers(SOAPAction = action),",
          "          httr::content_type(\"text/xml; charset=utf-8\"),",
          "          body = as.character(envelope),",
          "          config = self$config",
          "        )",
          "      }",
          "      handle_fault(response)",
          "      content <- httr::content(response, as = \"parsed\", encoding = \"UTF-8\")",
          "      body_node <- xml2::xml_find_first(content, \".//*[local-name()='Body']\")",
          "      response_node <- xml2::xml_children(body_node)[[1]]",
          "      response_name <- xml2::xml_name(response_node)",
          "      local_name <- gsub(\"^.*:\", \"\", response_name)",
          "      schema <- get_schema()",
          "      if (!is.null(schema[[local_name]])) {",
          "        deserialize(response_node, local_name, schema)",
          "      } else {",
          "        content",
          "      }"
        )

        method_doc <- c(
          paste0("    #' @description Call the ", op_name, " operation."),
          if (length(args_doc) > 0) paste0("    ", args_doc) else NULL
        )

        method_def <- paste0(
          paste(method_doc, collapse = "\n"),
          "\n",
          "    ",
          op_name,
          " = function(",
          paste(args, collapse = ", "),
          ") {\n",
          paste(func_body, collapse = "\n"),
          "\n    }"
        )
        public_members <- c(public_members, method_def)
      }

      # Assemble File
      lines <- c(
        detached_docs,
        "#' SOAP Client Class",
        "#' @description",
        "#' R6 class for interacting with the SOAP service.",
        "#' @importFrom R6 R6Class",
        "#' @export",
        "Client <- R6::R6Class(",
        "  \"Client\",",
        "  public = list(",
        paste(public_members, collapse = ",\n"),
        "  )",
        ")",
        "",
        "#' Create Client",
        "#' @param url The service endpoint URL. Defaults to the WSDL location.",
        "#' @param config A list of httr configuration options (e.g., authentication).",
        "#' @param transport A custom transport function for mocking. Defaults to NULL.",
        "#' @return A Client object.",
        "#' @export",
        "new_client <- function(url = NULL, config = list(), transport = NULL) {",
        "  Client$new(url = url, config = config, transport = transport)",
        "}"
      )

      writeLines(lines, file.path(path, "R", "client.R"))
    }
  )
)
