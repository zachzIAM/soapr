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

      # Create R/detached_docs.R (operations)
      self$write_detached_documentation(pkg_path)

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
    },

    #' @description
    #' Write the detached_docs.R file.
    #' @param path The package directory path.
    write_detached_documentation = function(path) {
      view <- private$get_detached_docs_view_model()
      template <- private$read_template("detached_docs.mustache")
      content <- whisker::whisker.render(template, view)
      writeLines(content, file.path(path, "R", "detached_docs.R"))
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

      process_attribute <- function(type_attribute) {
        attr_name <- type_attribute$name
        attr_doc <- type_attribute$documentation
        attr_type <- type_attribute$type
        attr_use <- sprintf("(%s)", type_attribute$use)
        doc_text <- attr_doc %||% paste(attr_type, attr_use)
        param_doc <- paste(attr_name, doc_text)

        list(
          arg = paste(attr_name, "=", NA),
          body = as.name(attr_name),
          param_doc = param_doc
        )
      }

      process_element <- function(type_element) {
        el_name <- type_element$name
        el_doc <- type_element$documentation
        el_type <- type_element$type

        is_unbounded <- (
          !is.null(type_element$maxOccurs) &&
            !is.na(type_element$maxOccurs) &&
            type_element$maxOccurs == "unbounded"
        )
        default_val <- if (is_unbounded) "list()" else "NA"
        doc_text <- el_doc %||% paste(el_type)
        param_doc <- paste(el_name, doc_text)

        list(
          arg = paste(el_name, "=", default_val),
          body = as.name(el_name),
          param_doc = param_doc
        )
      }

      type_list <- purrr::imap(
        types,
        ~ {
          if (.x$kind == "complex") {
            attributes_handling <- purrr::map(.x$attributes, process_attribute)
            elements_handling <- purrr::map(.x$elements, process_element)

            # build return elements
            args <- paste(
              c(
                purrr::map_chr(attributes_handling, ~ .x$arg),
                purrr::map_chr(elements_handling, ~ .x$arg)
              ),
              collapse = ", "
            )

            body <- c(
              purrr::map(attributes_handling, ~ .x$body),
              purrr::map(elements_handling, ~ .x$body)
            )
            body <- if (length(body) > 0) {
              paste(trimws(capture.output(dput(body))), collapse = " ")
            }

            param_docs <- c(
              purrr::map_chr(attributes_handling, ~ .x$param_doc),
              purrr::map_chr(elements_handling, ~ .x$param_doc)
            )
            param_docs <- purrr::set_names(param_docs, NULL)
            param_docs <- if (length(param_docs) > 0) param_docs

            attr_names <- if (length(.x$attributes) > 0) {
              paste(trimws(capture.output(dput(.x$attributes))), collapse = " ")
            }

            doc_str <- .x$documentation %||% paste("Constructor for", .y)

            list(
              name = .y,
              doc = doc_str,
              param_docs = param_docs,
              args = args,
              body = body,
              has_attrs = !is.null(attr_names),
              attr_names = attr_names,
              last = FALSE
            )
          }
        }
      )

      # drop nulls and names
      type_list <- purrr::set_names(
        type_list[!purrr::map_lgl(type_list, is.null)],
        NULL
      )

      # set last element flag for whisker
      if (length(type_list) > 0) {
        type_list[[length(type_list)]]$last <- TRUE
      }

      return(list(types = type_list))
    },

    get_detached_docs_view_model = function() {
      # We need to find a service and port
      services <- self$wsdl$get_services()
      service <- services[[1]] # TODO handle cases with multiple services
      port <- service$ports[[1]] # TODO handle cases with multiple ports, e.g. CountryInfoService

      detached_docs <- private$create_client_view_model_detatched_docs(port)

      list(detached_docs = detached_docs)
    },

    get_client_view_model = function() {
      services <- self$wsdl$get_services()
      tns <- self$wsdl$get_target_namespace()

      # Build services metadata for runtime selection
      services_meta <- list()
      all_ops_map <- list()

      for (svc in services) {
        ports_meta <- list()
        for (port in svc$ports) {
          # Get operations for this port
          ops <- self$wsdl$get_operations(port$binding)

          # Convert ops to simple list for metadata
          ops_meta <- list()
          for (op_name in names(ops)) {
            ops_meta[[op_name]] <- list(
              action = ops[[op_name]]$action
            )

            # Store operation definition for method generation (deduplicated)
            if (is.null(all_ops_map[[op_name]])) {
              all_ops_map[[op_name]] <- ops[[op_name]]
            }
          }

          ports_meta[[port$name]] <- list(
            location = port$location,
            binding = port$binding,
            operations = ops_meta
          )
        }
        services_meta[[svc$name]] <- list(
          ports = ports_meta
        )
      }

      # Format services_meta as string for injection
      services_meta_str <- paste(
        capture.output(dput(services_meta)),
        collapse = "\n"
      )

      # Generate methods for all unique operations
      # We use the first port's definition for arguments/doc, assuming consistency across ports for same-named ops
      operations <- purrr::map(
        all_ops_map,
        ~ {
          type_def <- self$wsdl$get_type_definitions()[[.x$name]]

          args_handling <- purrr::map(
            purrr::set_names(type_def$elements, NULL),
            ~ {
              min_occurs <- if (is.null(.x$minOccurs) || is.na(.x$minOccurs)) {
                1L
              } else {
                as.integer(.x$minOccurs)
              }
              is_required <- min_occurs > 0
              use <- if (is_required) "required" else "optional"
              doc_text <- .x$documentation %||%
                sprintf("%s %s (%s)", .x$type, .x$name, use)
              arg_doc <- paste(.x$name, doc_text)

              type_info <- self$wsdl$get_type_definitions()[[.x$type]]

              if (
                !is.null(type_info) &&
                  type_info$kind == "simple" &&
                  !is.null(type_info$values)
              ) {
                default_vals_str <- paste(
                  capture.output(dput(type_info$values)),
                  collapse = ""
                )
                arg_signature <- sprintf("%s = %s", .x$name, default_vals_str)
                arg_validation <- sprintf(
                  "%s <- match.arg(%s)",
                  .x$name,
                  .x$name
                )
              } else {
                arg_signature <- sprintf("%s = NA", .x$name)
                arg_validation <- NULL
              }

              arg_assignment <- sprintf("args[['%s']] <- %s", .x$name, .x$name)

              list(
                doc = arg_doc,
                signature = arg_signature,
                validation = arg_validation,
                assignment = arg_assignment
              )
            }
          )

          args_doc <- purrr::map(args_handling, ~ .x$doc)
          args_signature <- paste(
            purrr::map_chr(args_handling, ~ .x$signature),
            collapse = ", "
          )
          arg_validations <- purrr::map(args_handling, ~ .x$validation)
          arg_validations <- arg_validations[
            !purrr::map_lgl(arg_validations, is.null)
          ]
          arg_assignments <- purrr::map(args_handling, ~ .x$assignment)

          list(
            name = .x$name,
            args_doc = args_doc,
            args = args_signature,
            arg_validations = arg_validations,
            arg_assignments = arg_assignments,
            tns = tns,
            last = FALSE
          )
        }
      )

      # Convert named list to unnamed list for mustache
      operations <- unname(operations)

      if (length(operations) > 0) {
        operations[[length(operations)]]$last <- TRUE
      }

      list(
        services_meta = services_meta_str,
        tns = tns,
        operations = operations
      )
    },

    create_client_view_model_detatched_docs = function(port) {
      # This needs to be updated to support all operations too, but for simplicity
      # let's just use the first port of first service to generate docs for now,
      # or duplicate logic.
      # Ideally we iterate same way as above.

      # Reuse the same logic?
      # For now, let's keep it simple and just use the first available port for docs example,
      # as detached docs are mostly helper.
      # Or better: Iterate all unique operations.

      services <- self$wsdl$get_services()
      all_ops_map <- list()

      for (svc in services) {
        for (port in svc$ports) {
          ops <- self$wsdl$get_operations(port$binding)
          for (op_name in names(ops)) {
            if (is.null(all_ops_map[[op_name]])) {
              all_ops_map[[op_name]] <- ops[[op_name]]
            }
          }
        }
      }

      detached_docs <- purrr::map(
        all_ops_map,
        ~ {
          type_def <- self$wsdl$get_type_definitions()[[.x$name]]

          doc_str <- .x$documentation %||% paste("Operation", .x$name)

          args_doc <- purrr::map(
            purrr::set_names(type_def$elements, NULL),
            ~ {
              min_occurs <- if (is.null(.x$minOccurs) || is.na(.x$minOccurs)) {
                1L
              } else {
                as.integer(.x$minOccurs)
              }
              is_required <- min_occurs > 0
              use <- if (is_required) "required" else "optional"
              doc_text <- .x$documentation %||%
                sprintf("%s %s (%s)", .x$type, .x$name, use)
              paste(.x$name, doc_text)
            }
          )

          usage_args <- paste(names(type_def$elements), collapse = ", ")
          usage_doc <- sprintf("client$%s(%s)", .x$name, usage_args)

          list(
            title = doc_str,
            args_doc = args_doc,
            usage_doc = usage_doc,
            name = .x$name,
            alias = .x$name
          )
        }
      )

      unname(detached_docs)
    },

    create_client_view_model_operations = function(port) {
      # Deprecated / Unused now as logic moved to get_client_view_model
      NULL
    },

    get_readme_view_model = function(name) {
      # We need to find a service and port
      services <- self$wsdl$get_services()
      service <- services[[1]]
      port <- service$ports[[1]]

      # Get operations list
      ops <- self$wsdl$get_operations(port$binding)
      operations <- sort(names(ops))

      # Generate Example for First Operation

      if (length(ops) > 0) {
        op <- ops[[1]]
        op_name <- op$name

        # Get arguments
        type_def <- self$wsdl$get_type_definitions()[[op_name]]

        if (!is.null(type_def)) {
          arg_parts <- purrr::map_chr(
            type_def$elements,
            ~ {
              type_info <- self$wsdl$get_type_definitions()[[.x$type]]

              # determine type
              is_enum <- (
                !is.null(type_info) &&
                  !is.null(type_info$values) &&
                  type_info$kind == "simple"
              )
              is_numeric <- grepl(
                "int|long|short|byte|decimal|float|double",
                .x$type,
                ignore.case = TRUE
              )
              is_bool <- grepl("boolean", .x$type, ignore.case = TRUE)
              is_date <- grepl(
                "dateTime|date|time",
                .x$type,
                ignore.case = TRUE
              )

              # set default value
              dummy_val <- if (is_enum) {
                sprintf('"%s"', type_info$values[1])
              } else if (is_numeric) {
                0
              } else if (is_bool) {
                TRUE
              } else if (is_date) {
                sprintf('"%s"', Sys.Date())
              } else {
                '"string"'
              }

              paste(.x$name, "=", dummy_val)
            }
          )

          args_str <- paste(arg_parts, collapse = ", ")
        } else {
          args_str <- ""
        }

        example <- list(
          op_name = op_name,
          op_code = sprintf(
            "result <- client$%s(%s)\nprint(result)\n",
            op_name,
            args_str
          )
        )
      } else {
        example = NULL
      }

      list(
        package_name = name,
        wsdl_location = port$location,
        target_namespace = self$wsdl$get_target_namespace(),
        example = example,
        operations = operations
      )
    }
  )
)
