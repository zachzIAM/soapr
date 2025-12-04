#' Generator Class
#'
#' Generates a static R package from a WSDL.
#'
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
      self$write_documentation(pkg_path)

      # Inform user
      message(paste("Package generated at:", pkg_path))
    },

    #' @description
    #' Generate package documentation.
    #' @param path The package directory path.
    write_documentation = function(path) {
      roxygen2::roxygenize(path, roclets = c("rd", "namespace", "collate"))
    },

    #' @description
    #' Write the DESCRIPTION file.
    #' @param path The package directory path.
    #' @param name The package name.
    write_description = function(path, name) {
      view <- list(package_name = name)
      template <- private$read_template("DESCRIPTION.mustache")
      content <- whisker::whisker.render(template, view)
      writeLines(content, file.path(path, "DESCRIPTION"))
    },

    #' @description
    #' Write the README.md file.
    #' @param path The package directory path.
    #' @param name The package name.
    write_readme = function(path, name) {
      view <- private$get_readme_view_model(name)
      template <- private$read_template("README.mustache")
      readme <- whisker::whisker.render(template, view)
      writeLines(readme, file.path(path, "README.md"))
    },

    #' @description
    #' Write the utils.R file.
    #' @param path The package directory path.
    write_utils = function(path) {
      template_path <- private$get_template_path("utils.R")
      dest_path <- file.path(path, "R", "utils.R")
      file.copy(template_path, dest_path, overwrite = TRUE)
    },

    #' @description
    #' Write the types.R file.
    #' @param path The package directory path.
    write_types = function(path) {
      view <- private$get_types_view_model()
      template <- private$read_template("types.mustache")
      content <- whisker::whisker.render(template, view)
      writeLines(content, file.path(path, "R", "types.R"))
    },

    #' @description
    #' Write the schema.R file.
    #' @param path The package directory path.
    write_schema = function(path) {
      types <- self$wsdl$get_type_definitions()
      types_code <- capture.output(dput(types))
      view <- list(types_code = paste(types_code, collapse = "\n"))
      template <- private$read_template("schema.mustache")
      content <- whisker::whisker.render(template, view)
      writeLines(content, file.path(path, "R", "schema.R"))
    },

    #' @description
    #' Write the client.R file.
    #' @param path The package directory path.
    write_client = function(path) {
      view <- private$get_client_view_model()
      if (is.null(view)) {
        return()
      }

      template <- private$read_template("client.mustache")
      content <- whisker::whisker.render(template, view)
      writeLines(content, file.path(path, "R", "client.R"))
    }
  ),

  private = list(
    get_template_path = function(name) {
      template_path <- system.file(
        file.path("templates", name),
        package = "soapr"
      )
      if (template_path == "") {
        template_path <- file.path(getwd(), "inst", "templates", name)
      }
      return(template_path)
    },

    read_template = function(name) {
      template_path <- private$get_template_path(name)
      content <- readLines(template_path, warn = FALSE)
      return(paste(content, collapse = "\n"))
    },

    get_types_view_model = function() {
      types <- self$wsdl$get_type_definitions()
      type_list <- list()

      type_names <- names(types)
      for (i in seq_along(type_names)) {
        name <- type_names[i]
        def <- types[[name]]

        if (def$kind == "complex") {
          # Generate constructor method
          args <- c()
          body <- c()
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

          # Remove trailing comma from last body item
          if (length(body) > 0) {
            body[length(body)] <- sub(",$", "", body[length(body)])
          }

          doc_str <- if (!is.null(def$documentation)) {
            def$documentation
          } else {
            paste("Constructor for", name)
          }

          type_list[[length(type_list) + 1]] <- list(
            name = name,
            doc = doc_str,
            param_docs = param_docs,
            args = paste(args, collapse = ", "),
            body = body,
            has_attrs = length(attr_names) > 0,
            attr_names = if (length(attr_names) > 0) {
              paste(paste0('"', attr_names, '"'), collapse = ", ")
            } else {
              NULL
            },
            last = FALSE
          )
        }
      }

      if (length(type_list) > 0) {
        type_list[[length(type_list)]]$last <- TRUE
      }

      list(types = type_list)
    },

    get_client_view_model = function() {
      # We need to find a service and port
      services <- self$wsdl$get_services()
      service <- services[[1]]                    # TODO handle cases with multiple services
      port <- service$ports[[1]]                  # TODO handle cases with multiple ports, e.g. CountryInfoService

      ops <- self$wsdl$get_operations(port$binding)
      tns <- self$wsdl$get_target_namespace()

      detached_docs <- private$create_client_view_model_detatched_docs(ops)
      operations <- private$create_client_view_model_operations(ops)

      list(
        service_url = port$location,
        tns = tns,
        detached_docs = detached_docs,
        operations = operations
      )
    },

    create_client_view_model_detatched_docs = function(port_binding_ops) {
      purrr::map_chr(purrr::set_names(port_binding_ops, NULL), ~{
        type_def <- self$wsdl$get_type_definitions()[[.x$name]]

        doc_str <- .x$documentation %||% paste("Operation", .x$name)

        args_doc <- purrr::map_chr(type_def$elements, ~{
          doc_text <- .x$documentation %||% paste(.x$type, .x$name)
          paste("#'", "@param", .x$name, doc_text)
        })

        usage_args <- paste(names(type_def$elements), collapse = ", ")
        usage_doc <- paste("#'", "@usage", paste0("client$", .x$name, "(", usage_args, ")"))

        detached_doc <- c(
          paste("#'", doc_str),
          paste("#'", "@description"),
          paste("#'", "Call the", .x$name, "operation."),
          args_doc,
          usage_doc,
          paste("#'", "@name", .x$name),
          paste("#'", "@aliases", .x$name),
          "NULL",
          ""
        )
        paste(detached_doc, collapse = "\n")
      })
    },

    create_client_view_model_operations = function(port_binding_ops) {

      # tns <- self$wsdl$get_target_namespace()
      # operations <- purrr::map(port_binding_ops, ~{
      #   type_def <- self$wsdl$get_type_definitions()[[.x$name]]
      #
      #   args_handling <- purrr::map(type_def$elements, ~{
      #
      #     doc_text <- .x$documentation %||% paste(.x$type, .x$name)
      #     arg_doc <- paste("#'", "@param", .x$name, doc_text)
      #
      #     type_info <- self$wsdl$get_type_definitions()[[.x$type]]
      #
      #     if (!is.null(type_info) && type_info$kind == "simple" && !is.null(type_info$values)) {
      #       default_vals_str <- paste(capture.output(dput(type_info$values)), collapse = "")
      #       arg_signature <- paste(.x$name, "=", default_vals_str)
      #       arg_validation <- paste0("      ", .x$name, " <- match.arg(", .x$name, ")")
      #     } else {
      #       arg_signature <- paste(.x$name, "=", NA)
      #       arg_validation <- NULL
      #     }
      #
      #     arg_assignment <- paste0("      args[['", .x$name, "']] <- ", .x$name)
      #
      #     list(
      #       doc = arg_doc,
      #       signature = arg_signature,
      #       validation = arg_validation,
      #       assignment = arg_assignment
      #     )
      #   })
      #
      #   args_doc <- purrr::map_chr(args_handling, ~.x$doc)
      #   args_signature <- paste(purrr::map_chr(args_handling, ~.x$signature), collapse = ", ")
      #   arg_validations <- purrr::map_chr(args_handling, ~.x$validation %||% NA)
      #   arg_validations <- arg_validations[!is.na(arg_validations)]
      #   arg_assignments <- purrr::map_chr(args_handling, ~.x$assignment)
      #
      #   list(
      #     name = .x$name,
      #     action = .x$action,
      #     args_doc = purrr::set_names(args_doc, NULL),
      #     args = purrr::set_names(args_signature, NULL),
      #     arg_validations = purrr::set_names(arg_validations, NULL),
      #     arg_assignments = purrr::set_names(arg_assignments, NULL),
      #     tns = tns,
      #     last = FALSE
      #   )
      #
      # })
      #
      # if (length(operations) > 0) {
      #   operations[[length(operations)]]$last <- TRUE
      # }
      #
      # list(
      #   service_url = port$location,
      #   tns = tns,
      #   detached_docs = detached_docs,
      #   operations = operations
      # )

      tns <- self$wsdl$get_target_namespace()
      operations <- list()
      op_names <- names(port_binding_ops)
      for (i in seq_along(op_names)) {
        op_name <- op_names[i]
        op <- port_binding_ops[[op_name]]
        type_def <- self$wsdl$get_type_definitions()[[op_name]]

        args_doc <- purrr::map_chr(type_def$elements, ~{
          doc_text <- .x$documentation %||% paste(.x$type, .x$name)
          paste("#'", "@param", .x$name, doc_text)
        })

        args <- c()
        arg_assignments <- c()
        arg_validations <- c()

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

        operations[[length(operations) + 1]] <- list(
          name = op_name,
          action = op$action,
          args_doc = args_doc,
          args = paste(args, collapse = ", "),
          arg_validations = arg_validations,
          arg_assignments = arg_assignments,
          tns = tns,
          last = FALSE
        )
      }

      if (length(operations) > 0) {
        operations[[length(operations)]]$last <- TRUE
      }

      return(operations)
    },

    get_readme_view_model = function(name) {
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

      list(
        package_name = name,
        wsdl_location = self$wsdl$get_services()[[1]]$ports[[1]]$location,
        target_namespace = self$wsdl$get_target_namespace(),
        example_code = example_code,
        op_list = op_list
      )
    }
  )
)
