library(xml2)
library(httr)
devtools::load_all()

# Mock WSDL
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

# Initialize Factory
factory <- Factory$new(mock_wsdl)

# Create a sample XML response
xml_response <- '
<tns:SetAdHocPortfolioResponse xmlns:tns="http://barra.com/cp/bdti">
  <tns:Portfolio Type="HOLDINGS" PortfolioCurrency="USD">
    <tns:Pos Value="1000" Currency="USD">
      <tns:MID ID="AAPL" IDType="TICKER"/>
    </tns:Pos>
    <tns:Pos Value="2000" Currency="EUR">
      <tns:MID ID="GOOG" IDType="TICKER"/>
    </tns:Pos>
  </tns:Portfolio>
</tns:SetAdHocPortfolioResponse>
'
node <- xml2::read_xml(xml_response)

# Test deserialization
# Note: In our mock, SetAdHocPortfolioResponse is empty, but let's assume it contains a Portfolio
# Actually, let's update the mock to reflect reality if we knew it.
# But for testing generic deserialization, let's test deserializing a Portfolio directly.

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

print("Deserializing Portfolio...")
portfolio_obj <- factory$deserialize(portfolio_node, "Portfolio")
print(str(portfolio_obj))

# Check class
if (!inherits(portfolio_obj, "soap_object")) {
    stop("Result is not a soap_object")
}

# Check attributes
if (attr(portfolio_obj, "soap_type") != "Portfolio") {
    stop("Result has wrong soap_type")
}

if (portfolio_obj$Type != "HOLDINGS") {
    stop("Attribute Type not parsed correctly")
}

# Check elements
if (length(portfolio_obj$Pos) != 2) {
    stop("Pos list has wrong length")
}

print("Deserialization verification passed!")
