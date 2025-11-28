library(testthat)
library(soapr)

test_that("All testdata WSDLs can be parsed and Service created", {
    testthat::skip_on_ci()

    wsdl_dir <- test_path("testdata/valid_wsdls")
    wsdl_files <- list.files(
        wsdl_dir,
        pattern = "\\.(xml|wsdl)$",
        full.names = TRUE
    )

    expect_gt(length(wsdl_files), 0)

    results <- list()

    for (wsdl_file in wsdl_files) {
        file_name <- basename(wsdl_file)
        message(paste("Testing:", file_name))

        tryCatch(
            {
                # Attempt to create Service
                service <- Service$new(wsdl_file, verbose = FALSE)

                # Basic assertions
                expect_true(inherits(service, "Service"))
                expect_true(inherits(service$wsdl, "WSDLParser"))
                expect_true(inherits(service$factory, "Factory"))
                expect_true(inherits(service$client, "Client"))

                # Check if services and ports were found
                services <- service$wsdl$get_services()
                expect_gt(length(services), 0)

                results[[file_name]] <- "PASS"
            },
            error = function(e) {
                message(paste("FAILED:", file_name, "-", e$message))
                results[[file_name]] <<- paste("FAIL:", e$message)
            }
        )
    }

    # Print summary
    message("\n--- Smoke Test Summary ---")
    for (name in names(results)) {
        message(paste(name, ":", results[[name]]))
    }

    # Fail if any test failed
    failures <- results[grep("FAIL", results)]
    if (length(failures) > 0) {
        fail(paste("Smoke test failed for", length(failures), "WSDLs"))
    }
})
