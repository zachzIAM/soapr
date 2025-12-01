library(testthat)
library(soapr)

test_that("Static Client Generator works with Live Services", {
    skip_on_ci()

    services <- list(
        list(
            name = "CalculatorClient",
            url = "http://www.dneonline.com/calculator.asmx?WSDL"
        ),
        list(
            name = "NumberConversionClient",
            url = "https://www.dataaccess.com/webservicesserver/NumberConversion.wso?WSDL"
        ),
        list(
            name = "CountryInfoClient",
            url = "http://webservices.oorsprong.org/websamples.countryinfo/CountryInfoService.wso?WSDL"
        ),
        list(
            name = "ISBNClient",
            url = "http://webservices.daehosting.com/services/isbnservice.wso?WSDL"
        ),
        list(
            name = "TempConvertClient",
            url = "https://www.w3schools.com/xml/tempconvert.asmx?WSDL"
        )
    )

    temp_dir <- tempdir()

    for (service in services) {
        message("Testing generator with: ", service$name)

        # Generate package
        # Generate package
        expect_error(
            {
                wsdl <- WSDLParser$new(service$url)
                gen <- Generator$new(wsdl)
                gen$generate_package(temp_dir, service$name)
            },
            NA,
            info = paste("Failed to generate package for", service$name)
        )

        pkg_path <- file.path(temp_dir, service$name)

        # Verify structure
        expect_true(dir.exists(pkg_path))
        expect_true(file.exists(file.path(pkg_path, "DESCRIPTION")))
        expect_true(file.exists(file.path(pkg_path, "NAMESPACE")))
        expect_true(file.exists(file.path(pkg_path, "README.md")))
        expect_true(file.exists(file.path(pkg_path, "R", "client.R")))
        expect_true(file.exists(file.path(pkg_path, "R", "types.R")))
        expect_true(file.exists(file.path(pkg_path, "R", "utils.R")))

        # Verify loadability
        tryCatch(
            {
                pkgload::load_all(pkg_path)
                expect_true(exists(
                    "new_client",
                    where = as.environment(paste0("package:", service$name))
                ))

                # Verify new_client arguments
                client_func <- get(
                    "new_client",
                    pos = as.environment(paste0("package:", service$name))
                )
                args <- names(formals(client_func))
                expect_true("url" %in% args)
                expect_true("config" %in% args)
                expect_true("transport" %in% args)

                # Verify add_header exists
                expect_true(exists(
                    "add_header",
                    where = as.environment(paste0("package:", service$name))
                ))
            },
            error = function(e) {
                fail(paste(
                    "Failed to load generated package for",
                    service$name,
                    ":",
                    e$message
                ))
            }
        )

        # Clean up (unload if possible, though pkgload doesn't easily unload)
        # We rely on unique package names to avoid conflicts if run in same session
    }
})
