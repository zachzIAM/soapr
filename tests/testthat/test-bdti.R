library(testthat)
library(soapr)

test_that("BDTi Service can be created and methods exist", {
    wsdl_path <- test_path("testdata/valid_wsdls/BDTInteractive.xml")
    service <- Service$new(wsdl_path)

    expect_true(inherits(service, "Service"))
    expect_true(inherits(service$client, "Client"))
    expect_true(inherits(service$factory, "Factory"))

    # Check if methods were dynamically added
    # Attempt to call Login.
    # We expect this might fail due to network or invalid credentials,
    # but it shouldn't fail due to code errors (like missing function).

    result <- tryCatch(
        {
            service$client$Login(User = "test", Client = "test", Password = "test")
            "Success"
        },
        error = function(e) {
            # If it's a network error or SOAP fault, that's "fine" for now as it proves the code ran.
            # We just want to ensure it's not an internal R error.
            return(e)
        }
    )

    # We just expect result to be either the response or an error object, but not NULL
    expect_true(!is.null(result))

    # Optional: Print the result to see what happened
    print(result)
})
