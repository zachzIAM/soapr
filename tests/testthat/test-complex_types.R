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

  # Mock transport
  mock_transport <- function(url, handle, body, ...) {
    expect_true(grepl('Type="HOLDINGS"', body))
    expect_true(grepl('PortfolioCurrency="USD"', body))
    # No tns: prefix anymore
    expect_true(
      grepl('<MID [^>]*ID="AAPL" IDType="TICKER"[^>]*/>', body) ||
        grepl('<MID [^>]*ID="AAPL" IDType="TICKER"[^>]* />', body)
    )

    structure(
      list(
        url = url,
        status_code = 200,
        headers = list(`Content-Type` = "text/xml; charset=utf-8"),
        content = charToRaw(
          '
                    <soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
                        <soap:Body>
                            <SetAdHocPortfolioResponse xmlns="http://barra.com/cp/bdti">
                                <SetAdHocPortfolioResult>Success</SetAdHocPortfolioResult>
                            </SetAdHocPortfolioResponse>
                        </soap:Body>
                    </soap:Envelope>
                '
        )
      ),
      class = "response"
    )
  }

  # Inject mock transport
  service$client$transport <- mock_transport

  service$client$SetAdHocPortfolio(Portfolio = portfolio)
})
