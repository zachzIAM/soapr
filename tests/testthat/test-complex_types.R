test_that("Complex types are parsed and generated correctly", {
    wsdl_path <- "testdata/valid_wsdls/BDTInteractive.xml"
    service <- Service$new(wsdl_path)

    # Test Factory creation
    portfolio <- service$factory$create("Portfolio")
    expect_s3_class(portfolio, "soap_object")
    expect_equal(attr(portfolio, "soap_type"), "Portfolio")
    expect_true("Pos" %in% names(portfolio))

    # Test Attribute handling
    portfolio$Type <- "HOLDINGS"
    portfolio$PortfolioCurrency <- "USD"

    # Test Nested objects
    pos <- service$factory$create("Position")
    pos$Value <- 1000
    mid <- service$factory$create("MID")
    mid$ID <- "AAPL"
    mid$IDType <- "TICKER"

    pos$MID <- list(mid)
    portfolio$Pos <- list(pos)

    # We can't easily test the XML generation without mocking the private build_xml_node or intercepting the request.
    # But we can verify that call_operation doesn't crash before the network request.

    # Mock httr::POST to avoid network call and capture body
    with_mock(
        `httr::POST` = function(url, body, ...) {
            expect_true(grepl('Type="HOLDINGS"', body))
            expect_true(grepl('PortfolioCurrency="USD"', body))
            expect_true(
                grepl('<tns:MID ID="AAPL" IDType="TICKER"/>', body) ||
                    grepl('<tns:MID ID="AAPL" IDType="TICKER" />', body)
            )
            return(list(status_code = 200, content = charToRaw("<response/>")))
        },
        `httr::stop_for_status` = function(...) TRUE,
        `httr::content` = function(...) list(),
        {
            service$client$SetAdHocPortfolio(Portfolio = portfolio)
        }
    )
})
