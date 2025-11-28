library(testthat)
testthat::skip_on_ci()

test_that("NumberConversion service works", {
    wsdl_url <- "http://www.dataaccess.com/webservicesserver/NumberConversion.wso?WSDL"

    wsdl <- WSDLParser$new(wsdl_url)
    expect_s3_class(wsdl, "WSDLParser")

    services <- wsdl$get_services()
    expect_gt(length(services), 0)

    # Find the soap 1.1 or 1.2 port.
    port_def <- services[[1]]$ports[[1]]
    factory <- Factory$new(wsdl)
    client <- Client$new(wsdl, port_def, factory, verbose = TRUE)

    # NumberToWords
    # Input: ubiNum (unsignedLong)
    res_words <- client$NumberToWords(ubiNum = 123)
    expect_true(is.character(res_words$NumberToWordsResult))
    expect_match(res_words$NumberToWordsResult, "hundred", ignore.case = TRUE)

    # NumberToDollars
    # Input: dNum (decimal)
    res_dollars <- client$NumberToDollars(dNum = 123.45)
    expect_true(is.character(res_dollars$NumberToDollarsResult))
    expect_match(
      res_dollars$NumberToDollarsResult,
      "dollars.*cents",
      ignore.case = TRUE
    )
})
