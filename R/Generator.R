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
      view <- list(element_form_default = self$wsdl$element_form_default)
      template <- private$read_template("utils.R")
      content <- whisker::whisker.render(template, view)
      writeLines(content, file.path(path, "R", "utils.R"))
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
      view <- list(
        types_code = paste(
          trimws(capture.output(dput(self$wsdl$get_type_definitions()))),
          collapse = "\n"
        )
      )
      template <- private$read_template("schema.mustache")
      content <- whisker::whisker.render(template, view)
      writeLines(content, file.path(path, "R", "schema.R"))
    },

    #' @description
    #' Write the client.R file.
    #' @param path The package directory path.
    write_client = function(path) {
      view <- private$get_client_view_model()
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
    read_template = function(name) {
      readLines(system.file("templates", name, package = "soapr"), warn = FALSE)
    },
    get_readme_view_model = function(package_name) {
      # Basic view model for README
      list(
        package_name = package_name,
        package_title = paste("SOAP Client for", package_name)
      )
    },
    get_types_view_model = function() {
      types <- self$wsdl$get_type_definitions()

      # Helper to check if type is simple/enum
      is_simple <- function(t) t$kind == "simple"

      # Helper function to generate argument string and body
      # ... (logic from before) ...

      process_attribute <- function(attr) {
        default_val <- if (attr$use == "optional") "NA" else "NA" # For now, strict types handle required
        doc_text <- attr$documentation %||% paste("Attribute", attr$name)
        param_doc <- paste(attr$name, doc_text)

        list(
          arg = paste(attr$name, "=", default_val),
          body = as.name(attr$name),
          param_doc = param_doc,
          name = attr$name
        )
      }

      process_element <- function(el) {
        is_unbounded <- !is.na(el$maxOccurs) && el$maxOccurs == "unbounded"
        el_type <- el$type
        el_name <- el$name
        el_doc <- el$documentation

        default_val <- if (is_unbounded) "list()" else "NA"
        doc_text <- el_doc %||% paste(el_type)
        param_doc <- paste(el_name, doc_text)

        list(
          arg = paste(el_name, "=", default_val),
          body = as.name(el_name),
          param_doc = param_doc,
          name = el_name
        )
      }

      type_list <- purrr::imap(
        types,
        function(.x, .y) {
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

            # Fix attribute names: we want the names of the attributes, not the list of definitions
            attr_names <- if (length(.x$attributes) > 0) {
              paste(
                trimws(capture.output(dput(names(.x$attributes)))),
                collapse = " "
              )
            }

            doc_str <- .x$documentation %||% paste("Constructor for", .y)

            # Generate validation code (match.arg)
            validations <- c()

            # Helper to check for enum and update arg/validation
            process_enum <- function(item, item_def) {
              if (!is.null(item_def)) {
                type_name <- item_def$type
                type_def <- types[[type_name]]
                if (
                  !is.null(type_def) &&
                    type_def$kind == "simple" &&
                    !is.null(type_def$values)
                ) {
                  # It is an Enum
                  vals_str <- paste(
                    sprintf('"%s"', type_def$values),
                    collapse = ", "
                  )
                  choices_vec <- sprintf("c(%s)", vals_str)

                  # Check if optional
                  is_optional <- TRUE
                  if (
                    !is.null(item_def$use) &&
                      !is.na(item_def$use) &&
                      item_def$use == "required"
                  ) {
                    is_optional <- FALSE
                  }
                  if (
                    !is.null(item_def$minOccurs) &&
                      !is.na(item_def$minOccurs) &&
                      as.integer(item_def$minOccurs) > 0
                  ) {
                    is_optional <- FALSE
                  }

                  if (is_optional) {
                    # Optional: default to NA, validate only if not NA
                    item$arg <- sprintf("%s = NA", item$name)
                    validations <<- c(
                      validations,
                      sprintf(
                        "if (!is.na(%s)) %s <- match.arg(%s, %s)",
                        item$name,
                        item$name,
                        item$name,
                        choices_vec
                      )
                    )
                  } else {
                    # Required: STRICT - No default, explicit choices in match.arg
                    item$arg <- item$name
                    validations <<- c(
                      validations,
                      sprintf(
                        "%s <- match.arg(%s, %s)",
                        item$name,
                        item$name,
                        choices_vec
                      )
                    )
                  }
                }
              }
              return(item)
            }

            # Process attributes for Enums
            attributes_handling <- purrr::map(
              attributes_handling,
              function(attr) {
                attr_def <- .x$attributes[[attr$name]]
                process_enum(attr, attr_def)
              }
            )

            # Process elements for Enums
            elements_handling <- purrr::map(elements_handling, function(elem) {
              elem_def <- .x$elements[[elem$name]]
              process_enum(elem, elem_def)
            })

            # Re-build args string with updated signatures
            args <- paste(
              c(
                purrr::map_chr(attributes_handling, ~ .x$arg),
                purrr::map_chr(elements_handling, ~ .x$arg)
              ),
              collapse = ", "
            )

            validation_code <- if (length(validations) > 0) {
              paste(validations, collapse = "\n      ")
            } else {
              NULL
            }

            list(
              name = .y,
              doc = doc_str,
              param_docs = param_docs,
              args = args,
              attr_names = attr_names,
              body = body,
              validation_code = validation_code
            )
          } else {
            NULL
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
      port <- service$ports[[1]] # TODO handle cases with multiple ports

      ops <- self$wsdl$get_operations(port$binding)
      types <- self$wsdl$get_type_definitions() # Added to fix missing variable error

      # We want to generate Roxygen documentation for each operation
      # But independent of the client class (so users can read help easily)

      op_list <- purrr::imap(ops, function(op, name) {
        doc <- op$documentation %||% paste("Calling operation", name)

        args_info <- private$process_operation_args(name, op, types)

        args_doc <- args_info$param_docs
        usage_doc <- paste0(name, "(", args_info$args, ")")

        list(
          name = name,
          title = doc,
          usage_doc = usage_doc,
          args_doc = args_doc,
          alias = name
        )
      })

      op_list <- purrr::set_names(op_list, NULL)

      list(detached_docs = op_list)
    },
    get_client_view_model = function() {
      services <- self$wsdl$get_services()
      service <- services[[1]] # TODO handle multiple services
      port <- service$ports[[1]] # TODO handle multiple ports

      ops <- self$wsdl$get_operations(port$binding)
      types <- self$wsdl$get_type_definitions()

      services_meta <- self$wsdl$get_services()
      services_meta_dump <- paste(
        trimws(capture.output(dput(services_meta))),
        collapse = " "
      )

      op_list <- purrr::imap(ops, function(op, name) {
        doc <- op$documentation %||% paste("Call operation", name)

        args_info <- private$process_operation_args(name, op, types)

        list(
          name = name,
          doc = doc,
          args = args_info$args,
          args_doc = args_info$param_docs, # RENAMED from param_docs to match template
          validation_code = args_info$validation_code,
          arg_assignments = args_info$arg_assignments,
          arg_validations = args_info$arg_validations
        )
      })

      op_list <- purrr::set_names(op_list, NULL)
      if (length(op_list) > 0) {
        op_list[[length(op_list)]]$last <- TRUE
      }

      list(
        operations = op_list,
        services_meta = services_meta_dump
      )
    },

    process_operation_args = function(op_name, op_def, types) {
      # Arguments
      # We need to look up input message -> part -> element -> complexType
      # For Document/Literal Wrapped, the input element name is usually the operation name.

      op_type <- types[[op_name]]

      if (!is.null(op_type) && op_type$kind == "complex") {
        # Elements of the wrapper are the arguments
        args_vec <- c()
        validations <- c()

        process_arg <- function(elem_name, elem_def) {
          type_name <- elem_def$type
          type_def <- types[[type_name]]

          is_enum <- FALSE
          choices_vec <- ""

          if (
            !is.null(type_def) &&
              type_def$kind == "simple" &&
              !is.null(type_def$values)
          ) {
            is_enum <- TRUE
            vals_str <- paste(
              sprintf('"%s"', type_def$values),
              collapse = ", "
            )
            choices_vec <- sprintf("c(%s)", vals_str)
          }

          # Check optionality
          is_optional <- TRUE
          if (
            !is.null(elem_def$use) &&
              !is.na(elem_def$use) &&
              elem_def$use == "required"
          ) {
            is_optional <- FALSE
          }
          if (
            !is.null(elem_def$minOccurs) &&
              !is.na(elem_def$minOccurs) &&
              as.integer(elem_def$minOccurs) > 0
          ) {
            is_optional <- FALSE
          }

          default_val <- if (is_optional) "NA" else ""

          arg_str <- elem_name
          if (default_val != "") {
            arg_str <- paste0(elem_name, " = ", default_val)
          }

          # Validation
          if (is_enum) {
            if (is_optional) {
              validations <<- c(
                validations,
                sprintf(
                  "if (!is.na(%s)) %s <- match.arg(%s, %s)",
                  elem_name,
                  elem_name,
                  elem_name,
                  choices_vec
                )
              )
            } else {
              validations <<- c(
                validations,
                sprintf(
                  "%s <- match.arg(%s, %s)",
                  elem_name,
                  elem_name,
                  choices_vec
                )
              )
            }
          }

          p_doc <- elem_def$documentation %||% paste(elem_name)

          list(
            str = arg_str,
            name = elem_name,
            doc = paste0(elem_name, " ", p_doc),
            assignment = sprintf("args[['%s']] <- %s", elem_name, elem_name)
          )
        }

        arg_objs <- purrr::imap(op_type$elements, function(e, n) {
          process_arg(n, e)
        })

        args_str <- paste(purrr::map_chr(arg_objs, "str"), collapse = ", ")
        param_docs <- unname(purrr::map_chr(arg_objs, "doc"))
        arg_assignments <- unname(purrr::map_chr(arg_objs, "assignment"))

        list(
          args = args_str,
          param_docs = param_docs,
          validation_code = if (length(validations) > 0) {
            paste(validations, collapse = "\n      ")
          } else {
            NULL
          },
          arg_assignments = arg_assignments,
          arg_validations = unname(validations)
        )
      } else {
        # Fallback
        list(
          args = "...",
          param_docs = NULL,
          validation_code = NULL,
          arg_assignments = NULL,
          arg_validations = NULL
        )
      }
    }
  )
)
