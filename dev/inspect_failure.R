# Inspect Failure Full + Check
devtools::load_all("c:/workspace/soapr")

# Redirect output
con <- file("c:/workspace/soapr/inspect_failure_log.txt", open = "wt")
sink(con, type = "output")
sink(con, type = "message")

wsdl_url <- "https://www.barraone.com/bdti/BDTInteractive?wsdl"
doc <- soapr::WSDLParser$new(wsdl_url)
client <- soapr::Client$new(doc)
client$verbose <- FALSE # Turn off for valid calls to reduce noise

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

cat("\n--- Producing Failing Request (Full Fields) ---\n")
settings <- client$factory$create("CurrentSettings")
settings$Benchmark <- "MSCIJPND"
settings$BenchmarkOwner <- "SYSTEM"
settings$BaseCurrency <- "EUR"
settings$IsBalanceCurrency <- TRUE

client$verbose <- TRUE # Only trace this one and next
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

client$verbose <- FALSE
cat("\n--- Checking Values ---\n")
# Capture raw response to check XML content
trace(httr::POST, tracer = quote({
    cat("Raw Response Body:\n")
    print(httr::content(res, "text"))
}), exit = quote({}), print = FALSE)

curr <- client$GetCurrentSettings()
untrace(httr::POST)

# Helper
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
