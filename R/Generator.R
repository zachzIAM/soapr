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
    #' @param author The package author name for DESCRIPTION.
    #' @param email The package maintainer email for DESCRIPTION.
    #' @param license_holder The copyright holder name for LICENSE.
    generate_package = function(
      path,
      package_name,
      author = "Package Author",
      email = "author@example.com",
      license_holder = paste(package_name, "authors")
    ) {
      # Create directories
      pkg_path <- file.path(path, package_name)
      if (!dir.exists(pkg_path)) {
        dir.create(pkg_path, recursive = TRUE)
      }
      if (!dir.exists(file.path(pkg_path, "R"))) {
        dir.create(file.path(pkg_path, "R"), recursive = TRUE)
      }

      # Create DESCRIPTION
      self$write_description(pkg_path, package_name, author = author, email = email)

      # Create README.md
      self$write_readme(pkg_path, package_name)

      # Create License
      private$write_license(pkg_path, package_name, license_holder = license_holder)

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
    #' @param author The package author name.
    #' @param email The package maintainer email.
    write_description = function(path, name, author = "Package Author", email = "author@example.com") {
      safe_author <- private$normalize_metadata_value(author, "Package Author", escape_quotes = TRUE)
      safe_email <- private$normalize_metadata_value(email, "author@example.com", escape_quotes = TRUE)
      view <- list(
        package_name = name,
        author = safe_author,
        email = safe_email
      )
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
      view <- list(
        element_form_default = self$wsdl$element_form_default,
        attribute_form_default = self$wsdl$attribute_form_default,
        target_namespace = self$wsdl$get_target_namespace()
      )
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
    normalize_metadata_value = function(val, default, escape_quotes = FALSE) {
      normalized <- if (
        is.null(val) ||
          length(val) != 1L ||
          is.na(val) ||
          !nzchar(trimws(as.character(val)))
      ) {
        default
      } else {
        as.character(val)
      }
      if (escape_quotes) {
        normalized <- gsub("\\\\", "\\\\\\\\", normalized)
        normalized <- gsub('"', '\\\\"', normalized)
      }
      normalized
    },
    write_license = function(path, package_name, license_holder = paste(package_name, "authors")) {
      year <- format(Sys.Date(), "%Y")
      holder <- private$normalize_metadata_value(license_holder, paste(package_name, "authors"))
      writeLines(
        c(
          paste("YEAR:", year),
          paste("COPYRIGHT HOLDER:", holder)
        ),
        file.path(path, "LICENSE")
      )
    },
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
        # Strict types handle required checks; parameters default to NA when omitted.
        default_val <- "NA"
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
            } else {
              "list()"
            }

            param_docs <- c(
              purrr::map_chr(attributes_handling, ~ .x$param_doc),
              purrr::map_chr(elements_handling, ~ .x$param_doc)
            )
            param_docs <- purrr::set_names(param_docs, NULL)
            param_docs_str <- if (length(param_docs) > 0) {
              paste(sprintf("#' @param %s", param_docs), collapse = "\n    ")
            } else {
              ""
            }

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
              param_docs_str = param_docs_str,
              args = args,
              attr_names = attr_names,
              has_attrs = !is.null(attr_names),
              body = body,
              validation_code = validation_code,
              is_complex = TRUE
            )
          } else if (.x$kind == "element") {
            # Simple Element Type
            # Generate a constructor-like function
            # Argument: The element name

            arg_name <- .x$name
            doc_str <- .x$documentation %||% paste("Create", .y)

            # Since it's a simple type (element wrapper), we just pass it through
            # But we can add validation if we want, or just documentation.
            # For consistency with complex types, we might wrap it or just return it?
            # Existing Factory logic for simple types might expect just the value.

            # Let's generate a method that takes the value as a single argument defaulting to NA.

            list(
              name = .y,
              doc = doc_str,
              param_docs_str = sprintf("#' @param %s The value.", arg_name),
              args = paste0(arg_name, " = NA"),
              attr_names = NULL, # No complex attributes
              has_attrs = FALSE,
              body = arg_name,
              validation_code = NULL,
              is_complex = FALSE
            )
          } else if (.x$kind == "simple") {
            # Enum / SimpleType with restriction
            if (!is.null(.x$values) && length(.x$values) > 0) {
              arg_name <- .y # Use type name as arg name? or 'value'
              # But we usually name arg same as Type if no other context.
              # Let's use 'value'.
              arg_name <- "value"

              vals_str <- paste(
                sprintf('"%s"', .x$values),
                collapse = ", "
              )
              choices_vec <- sprintf("c(%s)", vals_str)

              # Strict validation for factory
              validation_code <- sprintf(
                "%s <- match.arg(%s, %s)",
                arg_name,
                arg_name,
                choices_vec
              )

              doc_str <- paste("Create Enum", .y)

              list(
                name = .y,
                doc = doc_str,
                param_docs_str = sprintf("#' @param %s One of: %s", arg_name, vals_str),
                args = paste0(arg_name, " = NA"),
                attr_names = NULL,
                has_attrs = FALSE,
                body = arg_name,
                validation_code = validation_code,
                is_complex = FALSE
              )
            } else {
              arg_name <- "value"
              doc_str <- paste("Create", .y)

              list(
                name = .y,
                doc = doc_str,
                param_docs_str = sprintf("#' @param %s The value.", arg_name),
                args = paste0(arg_name, " = NA"),
                attr_names = NULL,
                has_attrs = FALSE,
                body = arg_name,
                validation_code = NULL,
                is_complex = FALSE
              )
            }
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
      # Collect all operations from all ports
      all_ops <- list()
      services <- self$wsdl$get_services()

      for (svc in services) {
        for (port in svc$ports) {
          ops <- self$wsdl$get_operations(port$binding)
          for (op_name in names(ops)) {
            if (is.null(all_ops[[op_name]])) {
              all_ops[[op_name]] <- ops[[op_name]]
            }
          }
        }
      }

      types <- self$wsdl$get_type_definitions() # Added to fix missing variable error

      # We want to generate Roxygen documentation for each operation
      # But independent of the client class (so users can read help easily)

      op_list <- purrr::imap(all_ops, function(op, name) {
        doc <- op$documentation %||% paste("Calling operation", name)

        args_info <- private$process_operation_args(name, op, types)

        usage_doc <- paste0(name, "(", args_info$args, ")")

        list(
          name = name,
          title = doc,
          usage_doc = usage_doc,
          param_docs_str = args_info$param_docs_str,
          alias = name
        )
      })

      op_list <- purrr::set_names(op_list, NULL)

      list(detached_docs = op_list)
    },
    get_client_view_model = function() {
      # Collect all operations from all ports
      all_ops <- list()
      services <- self$wsdl$get_services()

      for (svc in services) {
        for (port in svc$ports) {
          ops <- self$wsdl$get_operations(port$binding)
          for (op_name in names(ops)) {
            if (is.null(all_ops[[op_name]])) {
              all_ops[[op_name]] <- ops[[op_name]]
            }
          }
        }
      }
      types <- self$wsdl$get_type_definitions()
      tns <- self$wsdl$get_target_namespace()
      services_meta <- self$wsdl$get_services()
      services_meta_dump <- paste(
        trimws(capture.output(dput(services_meta))),
        collapse = "\n      "
      )

      op_list <- purrr::imap(all_ops, function(op, name) {
        doc <- op$documentation %||% paste("Call operation", name)

        args_info <- private$process_operation_args(name, op, types)

        list(
          name = name,
          doc = doc,
          args = args_info$args,
          param_docs_str = args_info$param_docs_str,
          validation_code = args_info$validation_code,
          arg_assignments = args_info$arg_assignments,
          arg_validations = args_info$arg_validations,
          tns = tns
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

      # Use input element if extracted by parser, otherwise fallback to operation name
      type_name <- op_def$input_element %||% op_name
      op_type <- types[[type_name]]

      if (!is.null(op_type) && op_type$kind == "complex") {
        # Elements of the wrapper are the arguments
        args_vec <- c()
        validations <- c()

        process_arg <- function(elem_name, elem_def) {
          type_name <- elem_def$type
          type_def <- types[[type_name]]

          is_enum <- FALSE
          is_complex <- FALSE
          choices_vec <- ""

          if (
            !is.null(type_def) &&
              type_def$kind == "simple" &&
              !is.null(type_def$values) &&
              length(type_def$values) > 0
          ) {
            is_enum <- TRUE
            vals_str <- paste(
              sprintf('"%s"', type_def$values),
              collapse = ", "
            )
            choices_vec <- sprintf("c(%s)", vals_str)
          } else if (!is.null(type_def) && type_def$kind == "complex") {
            is_complex <- TRUE
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
          } else if (is_complex) {
            validations <<- c(
              validations,
              sprintf(
                "check_soap_type(%s, \"%s\", \"%s\")",
                elem_name,
                type_name,
                elem_name
              )
            )
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
        param_docs_str <- if (length(param_docs) > 0) {
          paste(sprintf("#' @param %s", param_docs), collapse = "\n    ")
        } else {
          ""
        }
        arg_assignments <- unname(purrr::map_chr(arg_objs, "assignment"))

        list(
          args = args_str,
          param_docs_str = param_docs_str,
          validation_code = if (length(validations) > 0) {
            paste(validations, collapse = "\n      ")
          } else {
            NULL
          },
          arg_assignments = arg_assignments,
          arg_validations = unname(validations)
        )
      } else if (!is.null(op_type) && op_type$kind == "element") {
        # Simple Element Wrapper
        # The argument is the element itself
        arg_name <- op_type$name
        doc_text <- op_type$documentation %||% paste(arg_name)

        list(
          args = sprintf("%s = NA", arg_name),
          param_docs_str = sprintf("#' @param %s %s", arg_name, doc_text),
          validation_code = NULL,
          arg_assignments = sprintf("args[['%s']] <- %s", arg_name, arg_name),
          arg_validations = NULL
        )
      } else {
        # Fallback
        list(
          args = "...",
          param_docs_str = "#' @param ... Additional arguments.",
          validation_code = NULL,
          arg_assignments = NULL,
          arg_validations = NULL
        )
      }
    }
  )
)
