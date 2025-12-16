# Reproduce SetCurrentSettings Issue
devtools::load_all("c:/workspace/soapr")

# Redirect output to file for inspection
con <- file("c:/workspace/soapr/reproduce_log.txt", open = "wt")
sink(con, type = "output")
sink(con, type = "message")

# 1. Setup Dynamic Client
# 1. Setup Dynamic Client with Customizations
wsdl_url <- "https://www.barraone.com/bdti/BDTInteractive?wsdl"
customs <- list(convert_types = c("CurrentSettings"))
doc <- soapr::WSDLParser$new(wsdl_url, customizations = customs)

# Use Client directly (Service creates its own parser, so bypass it)
client <- soapr::Client$new(doc)
tf <- client$factory

user <- "iamquant"
password <- "TheCatSatOnTheMat99"
client_id <- "kzdr85j9hv"

cat("Logging in...\n")
client$Login(User = user, Client = client_id, Password = password)
cat("Logged in.\n")

# Set AdHoc Portfolio
cat("Setting AdHoc Portfolio...\n")
mid <- tf$create("MID")
mid$ID <- "2046251"
mid$IDType <- "SEDOL"
pos <- tf$create("Position")
pos$Value <- 1
pos$Currency <- "USD"
pos$MID <- list(mid)
p <- tf$create("Portfolio")
p$Type <- "WEIGHT"
p$PortfolioValue <- 1
p$PortfolioCurrency <- "USD"
p$Pos <- list(pos)
client$SetAdHocPortfolio(Portfolio = p)
cat("Portfolio Set.\n")

# 2. Try SetCurrentSettings with Benchmark and Owner
cat("\n--- Attempting SetCurrentSettings (Benchmark + Owner) ---\n")

tryCatch(
    {
        settings <- tf$create("CurrentSettings")
        settings$Benchmark <- "MSCIJPND"
        settings$BenchmarkOwner <- "SYSTEM"

        # Trace the request to see the XML
        trace(httr::POST, tracer = quote(print(body)), print = FALSE)

        cat("Calling SetCurrentSettings...\n")
        client$SetCurrentSettings(CurrentSettings = settings)
        cat("SetCurrentSettings returned\n")

        # Check if it actually changed
        cat("Checking settings...\n")
        current <- client$GetCurrentSettings()
        # Print simplified view
        # Assuming current is a list of KeyValue objects
        print(current)
    },
    error = function(e) {
        cat("SetCurrentSettings Failed:\n")
        print(e)
    }
)

# 3. Try BaseCurrency
cat("\n--- Attempting SetCurrentSettings (BaseCurrency) ---\n")
tryCatch(
    {
        settings_curr <- tf$create("CurrentSettings")
        settings_curr$BaseCurrency <- "EUR"

        cat("Calling SetCurrentSettings with BaseCurrency...\n")
        client$SetCurrentSettings(CurrentSettings = settings_curr)
        cat("SetCurrentSettings (BaseCurrency) returned\n")
    },
    error = function(e) {
        cat("SetCurrentSettings (BaseCurrency) Failed:\n")
        print(e)
    }
)

client$Logout()
