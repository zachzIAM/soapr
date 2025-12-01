library(testthat)
library(soapr)

test_that("Invalid WSDLs should fail to parse or create Service", {
  testthat::skip_on_ci()

  wsdl_dir <- test_path("testdata/invalid_wsdls")
  wsdl_files <- list.files(
    wsdl_dir,
    pattern = "\\.(xml|wsdl)$",
    full.names = TRUE
  )

  expect_gt(length(wsdl_files), 0)

  results <- list()

  for (wsdl_file in wsdl_files) {
    file_name <- basename(wsdl_file)
    message(paste("Testing Invalid WSDL:", file_name))

    tryCatch(
      {
        # Attempt to create Service
        service <- Service$new(wsdl_file, verbose = FALSE)

        # If it passes, check if it's functional
        ops <- service$client$operations

        if (length(ops) == 0) {
          message(paste(
            "PASSED (Empty):",
            file_name,
            "- Service created but has 0 operations."
          ))
          results[[file_name]] <- list(
            status = "PASS_EMPTY",
            message = "Service created with 0 operations"
          )
        } else {
          message(paste(
            "UNEXPECTED PASS (Functional):",
            file_name,
            "- Service created with",
            length(ops),
            "operations!"
          ))
          results[[file_name]] <- list(
            status = "PASS_FULL",
            message = paste(
              "Service created with",
              length(ops),
              "operations"
            )
          )
        }
      },
      error = function(e) {
        # If we get here, it FAILED as expected (mostly)
        message(paste("EXPECTED FAIL:", file_name, "-", e$message))
        results[[file_name]] <- list(
          status = "FAIL",
          message = e$message
        )
      }
    )
  }

  # Analyze results
  message("\n--- Invalid WSDL Test Summary ---")
  unexpected_passes <- list()

  for (name in names(results)) {
    res <- results[[name]]
    message(paste(name, ":", res$status, "-", res$message))

    if (res$status == "PASS") {
      unexpected_passes[[name]] <- res
    }
  }

  # We generally expect them to fail or be empty.
  # If any pass with operations, that's a problem.

  unexpected_full_passes <- list()
  for (name in names(results)) {
    if (results[[name]]$status == "PASS_FULL") {
      unexpected_full_passes[[name]] <- results[[name]]
    }
  }

  if (length(unexpected_full_passes) > 0) {
    fail(paste(
      "The following invalid WSDLs unexpectedly produced functional clients:",
      paste(names(unexpected_full_passes), collapse = ", ")
    ))
  } else {
    succeed("All invalid WSDLs either failed or produced empty clients.")
  }
})
