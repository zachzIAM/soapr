# Check Fix Dynamic Namespace Debug
devtools::load_all("c:/workspace/soapr")

# Redirect output
con <- file("c:/workspace/soapr/check_fix_debug_log.txt", open = "wt")
sink(con, type = "output")
sink(con, type = "message")

wsdl_url <- "https://www.barraone.com/bdti/BDTInteractive?wsdl"
doc <- soapr::WSDLParser$new(wsdl_url)

cat("Element Form Default: ", doc$element_form_default, "\n")

client <- soapr::Client$new(doc)
client$verbose <- TRUE # Enable verbose to see XML

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

cat("\n--- Testing SetCurrentSettings (Standard) ---\n")
tryCatch(
    {
        settings <- client$factory$create("CurrentSettings")
        settings$Benchmark <- "MSCIJPND"
        settings$BenchmarkOwner <- "SYSTEM"
        settings$BaseCurrency <- "EUR"

        client$SetCurrentSettings(CurrentSettings = settings)
        cat("SetCurrentSettings SUCCESS.\n")

        cat("Checking Values:\n")
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
    },
    error = function(e) {
        cat("FAILED:\n")
        print(e)
    }
)

cat("\n--- Testing SetCurrentSettings (Qualified Forced) ---\n")
# Force qualified serialization for this execution
client$serializer$element_form_default <- "qualified"

tryCatch(
    {
        settings <- client$factory$create("CurrentSettings")
        settings$Benchmark <- "MSCIJPND"
        settings$BenchmarkOwner <- "SYSTEM"
        settings$BaseCurrency <- "EUR"

        client$SetCurrentSettings(CurrentSettings = settings)
        cat("SetCurrentSettings Qualified SUCCESS.\n")

        cat("Checking Values Qualified:\n")
        curr <- client$GetCurrentSettings()

        cat("Benchmark:", find_val(curr, "Benchmark"), "\n")
        cat("BaseCurrency:", find_val(curr, "BaseCurrency"), "\n")
    },
    error = function(e) {
        cat("FAILED Qualified:\n")
        print(e)
    }
)


client$Logout()
