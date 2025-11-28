library(testthat)
testthat::skip_on_ci()

test_that("Calculator service works", {
    wsdl_url <- "http://www.dneonline.com/calculator.asmx?WSDL"

    wsdl <- WSDLParser$new(wsdl_url)
    expect_s3_class(wsdl, "WSDLParser")

    services <- wsdl$get_services()
    expect_gt(length(services), 0)

    port_def <- services[[1]]$ports[[1]]
    factory <- Factory$new(wsdl)
    client <- Client$new(wsdl, port_def, factory, verbose = TRUE)

    # Add
    res_add <- client$Add(intA = 10, intB = 5)
    expect_equal(as.integer(res_add$AddResult), 15)

    # Subtract
    res_sub <- client$Subtract(intA = 10, intB = 5)
    expect_equal(as.integer(res_sub$SubtractResult), 5)

    # Multiply
    res_mul <- client$Multiply(intA = 10, intB = 5)
    expect_equal(as.integer(res_mul$MultiplyResult), 50)

    # Divide
    res_div <- client$Divide(intA = 10, intB = 5)
    expect_equal(as.integer(res_div$DivideResult), 2)
})
