library(testthat)
library(soapr)

test_that("Factory deserializes complex nested structures (Portfolio example)", {
    # Mock WSDL from verify_deserialization.R
    mock_wsdl <- list(
        get_type_definitions = function() {
            list(
                SetAdHocPortfolioResponse = list(
                    kind = "complex",
                    elements = list(),
                    attributes = list()
                ),
                Portfolio = list(
                    kind = "complex",
                    elements = list(
                        Pos = list(type = "Position", maxOccurs = "unbounded")
                    ),
                    attributes = list(
                        Type = list(type = "PortfolioType"),
                        PortfolioValue = list(type = "xsd:double"),
                        PortfolioCurrency = list(type = "xsd:string")
                    )
                ),
                Position = list(
                    kind = "complex",
                    elements = list(
                        MID = list(type = "MID", maxOccurs = "unbounded")
                    ),
                    attributes = list(
                        Value = list(type = "xsd:double"),
                        Currency = list(type = "xsd:string")
                    )
                ),
                MID = list(
                    kind = "complex",
                    elements = list(),
                    attributes = list(
                        ID = list(type = "xsd:string"),
                        IDType = list(type = "xsd:string")
                    )
                )
            )
        },
        get_operations = function(...) list()
    )

    factory <- Factory$new(mock_wsdl)

    portfolio_xml <- '
      <tns:Portfolio Type="HOLDINGS" PortfolioCurrency="USD" xmlns:tns="http://barra.com/cp/bdti">
        <tns:Pos Value="1000" Currency="USD">
          <tns:MID ID="AAPL" IDType="TICKER"/>
        </tns:Pos>
        <tns:Pos Value="2000" Currency="EUR">
          <tns:MID ID="GOOG" IDType="TICKER"/>
        </tns:Pos>
      </tns:Portfolio>
    '
    portfolio_node <- xml2::read_xml(portfolio_xml)

    portfolio_obj <- factory$deserialize(portfolio_node, "Portfolio")

    expect_s3_class(portfolio_obj, "soap_object")
    expect_equal(attr(portfolio_obj, "soap_type"), "Portfolio")
    expect_equal(portfolio_obj$Type, "HOLDINGS")
    expect_equal(portfolio_obj$PortfolioCurrency, "USD")

    expect_length(portfolio_obj$Pos, 2)

    pos1 <- portfolio_obj$Pos[[1]]
    expect_equal(pos1$Value, "1000")
    expect_equal(pos1$Currency, "USD")
    expect_length(pos1$MID, 1)
    expect_equal(pos1$MID[[1]]$ID, "AAPL")

    pos2 <- portfolio_obj$Pos[[2]]
    expect_equal(pos2$Value, "2000")
    expect_equal(pos2$Currency, "EUR")
    expect_length(pos2$MID, 1)
    expect_equal(pos2$MID[[1]]$ID, "GOOG")
})
