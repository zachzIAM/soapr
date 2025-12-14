# Check Fix Customizations
devtools::load_all("c:/workspace/soapr")

# Redirect output
con <- file("c:/workspace/soapr/check_fix_custom_log.txt", open = "wt")
sink(con, type = "output")
sink(con, type = "message")

wsdl_url <- "https://www.barraone.com/bdti/BDTInteractive?wsdl"

# Override: Force BaseCurrency (and Benchmark for good measure) to be elements
# This matches the "Hybrid" fix I tried earlier or "Elements" fix.
# Since "Clean" (Attributes) is ignored, forcing elements is safer to match "Entity Reference" crash report.
customs <- list(
    convert_types = c("CurrentSettings")
)

doc <- soapr::WSDLParser$new(wsdl_url, customizations = customs)
client <- soapr::Client$new(doc)
client$Login(User = "iamquant", Client = "kzdr85j9hv", Password = "TheCatSatOnTheMat99")
# Set Valid Portfolio
mid <- client$factory$create("MID")
mid$ID <- "2046251"
mid$IDType <- "SEDOL"
pos <- client$factory$create("Position")
pos$Value <- 1
pos$Currency <- "USD"
pos$MID <- list(mid)
p <- client$factory$create("Portfolio")
p$Type <- "WEIGHT"
p$PortfolioValue <- 1
p$PortfolioCurrency <- "USD"
p$Pos <- list(pos)
client$SetAdHocPortfolio(Portfolio = p)

cat("\n--- Testing SetCurrentSettings (With Customizations) ---\n")
settings <- client$factory$create("CurrentSettings")
settings$Benchmark <- "MSCIJPND"
settings$BaseCurrency <- "EUR"
settings$IsBalanceCurrency <- TRUE

tryCatch(
    {
        client$SetCurrentSettings(CurrentSettings = settings)
        cat("SetCurrentSettings SUCCESS call.\n")
    },
    error = function(e) {
        cat("Error caught:\n")
        print(e)
    }
)

cat("\n--- Checking Values ---\n")
curr <- client$GetCurrentSettings()
find_val <- function(res, key) {
    kvs <- res$CurrentSettings
    if (is.null(kvs)) kvs <- res
    for (kv in kvs) {
        if (kv$Key == key) {
            return(kv$Value)
        }
    }
    return(NA)
}
cat("Benchmark:", find_val(curr, "Benchmark"), "\n")
cat("BaseCurrency:", find_val(curr, "BaseCurrency"), "\n")

client$Logout()
