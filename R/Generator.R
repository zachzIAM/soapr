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
        default_val <- if (is_unbounded) "list()" else "NA" # TODO should it be a list if > 1?
        doc_text <- el_doc %||% paste(el_type)
        param_doc <- paste(el_name, doc_text)

        list(
          arg = paste(el_name, "=", default_val),
          body = as.name(el_name),
          param_doc = param_doc
        )
      }

      type_list <- purrr::imap(types, ~{
        if (.x$kind == "complex") {

          attributes_handling <- purrr::map(.x$attributes, process_attribute)
          elements_handling <- purrr::map(.x$elements, process_element)

          # build return elements
          args <- paste(c(
            purrr::map_chr(attributes_handling, ~.x$arg),
            purrr::map_chr(elements_handling, ~.x$arg)
          ), collapse = ", ")

          body <- c(
            purrr::map(attributes_handling, ~.x$body),
            purrr::map(elements_handling, ~.x$body)
          )
          body <- if (length(body) > 0) {
            paste(trimws(capture.output(dput(body))), collapse = " ")
          }

          param_docs <- c(
            purrr::map_chr(attributes_handling, ~.x$param_doc),
            purrr::map_chr(elements_handling, ~.x$param_doc)
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
      })

      # drop nulls and names
      type_list <- purrr::set_names(type_list[!purrr::map_lgl(type_list, is.null)], NULL)

      # set last element flag for whisker
      if (length(type_list) > 0) {
        type_list[[length(type_list)]]$last <- TRUE
      }

      return(list(types = type_list))

    },

    get_detached_docs_view_model = function() {
      # We need to find a service and port
      services <- self$wsdl$get_services()
      service <- services[[1]]                    # TODO handle cases with multiple services
      port <- service$ports[[1]]                  # TODO handle cases with multiple ports, e.g. CountryInfoService

      detached_docs <- private$create_client_view_model_detatched_docs(port)

      list(detached_docs = detached_docs)
    },

    get_client_view_model = function() {
      # We need to find a service and port
      services <- self$wsdl$get_services()
      service <- services[[1]]                    # TODO handle cases with multiple services
      port <- service$ports[[1]]                  # TODO handle cases with multiple ports, e.g. CountryInfoService

      tns <- self$wsdl$get_target_namespace()

      operations <- private$create_client_view_model_operations(port)

      list(
        service_url = port$location,
        tns = tns,
        operations = operations
      )
    },

    create_client_view_model_detatched_docs = function(port) {

      port_binding_ops <- purrr::set_names(self$wsdl$get_operations(port$binding), NULL)

      purrr::map(port_binding_ops, ~{
        type_def <- self$wsdl$get_type_definitions()[[.x$name]]

        doc_str <- .x$documentation %||% paste("Operation", .x$name)

        args_doc <- purrr::map(purrr::set_names(type_def$elements, NULL), ~{
          is_required <- !is.null(.x$minOccurs) && as.integer(.x$minOccurs) > 0
          use <- if (is_required) "required" else "optional"
          doc_text <- .x$documentation %||% sprintf("%s %s (%s)", .x$type, .x$name, use)
          doc_text <- .x$documentation %||% paste(.x$type, .x$name)
          paste(.x$name, doc_text)
        })

        usage_args <- paste(names(type_def$elements), collapse = ", ")
        usage_doc <- paste0("client$", .x$name, "(", usage_args, ")")

        list(
          title = doc_str,
          args_doc = args_doc,
          usage_doc = usage_doc,
          name = .x$name,
          alias = .x$name
        )

      })
    },

    create_client_view_model_operations = function(port) {

      tns <- self$wsdl$get_target_namespace()

      port_binding_ops <- purrr::set_names(self$wsdl$get_operations(port$binding), NULL)

      operations <- purrr::map(port_binding_ops, ~{
        type_def <- self$wsdl$get_type_definitions()[[.x$name]]

        args_handling <- purrr::map(type_def$elements, ~{

          is_required <- !is.null(.x$minOccurs) && as.integer(.x$minOccurs) > 0
          use <- if (is_required) "required" else "optional"
          doc_text <- .x$documentation %||% sprintf("%s %s (%s)", .x$type, .x$name, use)
          arg_doc <- paste(.x$name, doc_text)

          type_info <- self$wsdl$get_type_definitions()[[.x$type]]

          if (!is.null(type_info) && type_info$kind == "simple" && !is.null(type_info$values)) {
            default_vals_str <- paste(capture.output(dput(type_info$values)), collapse = "")
            arg_signature <- sprintf("%s = %s", .x$name, default_vals_str)
            arg_validation <- sprintf("%s <- match.arg(%s)", .x$name, .x$name)
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
        })

        args_doc <- purrr::map(purrr::set_names(args_handling, NULL), ~.x$doc)
        args_signature <- paste(purrr::map_chr(args_handling, ~.x$signature), collapse = ", ")
        arg_validations <- purrr::map_chr(args_handling, ~.x$validation %||% NA)
        arg_validations <- arg_validations[!is.na(arg_validations)]
        arg_assignments <- purrr::map_chr(purrr::set_names(args_handling, NULL), ~.x$assignment)

        list(
          name = .x$name,
          action = .x$action,
          args_doc = args_doc,
          args = args_signature,
          arg_validations = arg_validations,
          arg_assignments = arg_assignments,
          tns = tns,
          last = FALSE
        )

      })

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
