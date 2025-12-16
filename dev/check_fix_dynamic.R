# Check Fix Dynamic
devtools::load_all("c:/workspace/soapr")

# Redirect output
con <- file("c:/workspace/soapr/check_fix_log.txt", open = "wt")
sink(con, type = "output")
sink(con, type = "message")

wsdl_url <- "https://www.barraone.com/bdti/BDTInteractive?wsdl"
# Reload WSDL to pick up parser changes
doc <- soapr::WSDLParser$new(wsdl_url)

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

cat("\n--- Testing SetCurrentSettings (Unqualified Attributes) ---\n")
tryCatch(
    {
        settings <- client$factory$create("CurrentSettings")

        settings$Benchmark <- "MSCIJPND"
        settings$BenchmarkOwner <- "SYSTEM"
        settings$BaseCurrency <- "EUR"

        trace(httr::POST, tracer = quote(print(body)), print = FALSE)

        client$SetCurrentSettings(CurrentSettings = settings)
        cat("SetCurrentSettings SUCCESS.\n")

        cat("Checking Values:\n")
        curr <- client$GetCurrentSettings()

        # Helper to find key value
        find_val <- function(res, key) {
            # res has structure $CurrentSettings which is list of items
            # But wait, depending on deserializer, it might be flattened?
            # Let's inspect res structure first
            # print(str(res))

            kvs <- res$CurrentSettings
            if (is.null(kvs)) kvs <- res # Fallback if structure is different

            for (kv in kvs) {
                if (kv$Key == key) {
                    return(kv$Value)
                }
            }
            return(NA)
        }

        cat("Benchmark:", find_val(curr, "Benchmark"), "\n")
        cat("BaseCurrency:", find_val(curr, "BaseCurrency"), "\n")
    },
    error = function(e) {
        cat("FAILED:\n")
        print(e)
    }
)

client$Logout()
