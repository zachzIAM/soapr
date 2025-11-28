library(testthat)
library(soapr)
skip_on_ci()

test_that("ISBN Service Integration", {
    wsdl_path <- "http://webservices.daehosting.com/services/isbnservice.wso?WSDL"

    service <- Service$new(wsdl_path)

    # Test IsValidISBN13
    # Valid ISBN: 9780596520687 (Learning Python)
    res_valid <- service$client$IsValidISBN13(sISBN = "9780596520687")

    # Expect TRUE (boolean)
    # Depending on deserialization, it might be "true", TRUE, or a list containing it.
    # Our Factory converts "true"/"false" to logical if type is boolean.

    # Let's check what we get
    expect_true(
        isTRUE(res_valid) ||
            res_valid == "true" ||
            (is.list(res_valid) &&
                (isTRUE(res_valid[[1]]) || res_valid[[1]] == "true"))
    )

    # Test Invalid ISBN
    res_invalid <- service$client$IsValidISBN13(sISBN = "1234567890123")
    expect_false(isTRUE(res_invalid) || res_invalid == "true")
})
