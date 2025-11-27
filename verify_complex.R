devtools::load_all()

wsdl_path <- "tests/testthat/testdata/valid_wsdls/BDTInteractive.xml"
service <- Service$new(wsdl_path, verbose = TRUE)

print("Creating Portfolio object...")
portfolio <- service$factory$create("Portfolio")
print(str(portfolio))

# Populate Portfolio
portfolio$Type <- "HOLDINGS"
portfolio$PortfolioCurrency <- "USD"

# Create Position
pos <- service$factory$create("Position")
pos$Value <- 1000
pos$Currency <- "USD"

# Create MID
mid <- service$factory$create("MID")
mid$ID <- "AAPL"
mid$IDType <- "TICKER"

pos$MID <- list(mid)
portfolio$Pos <- list(pos)

print("Attempting SetAdHocPortfolio...")
tryCatch(
    {
        service$client$SetAdHocPortfolio(Portfolio = portfolio)
        print("SetAdHocPortfolio Successful!")
    },
    error = function(e) {
        print("SetAdHocPortfolio Failed (Expected if no real server)!")
        print(e)
        # We can't see the request body easily here unless we modify Client to print it or return it.
        # But if we get a 404 or similar, it means the request was sent.
    }
)
