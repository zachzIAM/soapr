# Check soapr XML Generation with force_qualified
devtools::load_all("c:/workspace/soapr")

# Redirect output
con <- file("c:/workspace/soapr/soapr_gen_log.txt", open = "wt")
sink(con, type = "output")
sink(con, type = "message")

# Trace httr::POST to capture the body
trace(httr::POST, tracer = quote(print(body)), print = FALSE)

wsdl_url <- "https://www.barraone.com/bdti/BDTInteractive?wsdl"

# Create parser with force_qualified
customs <- list(
    force_qualified = TRUE
)
doc <- soapr::WSDLParser$new(wsdl_url, customizations = customs)
client <- soapr::Client$new(doc)

user <- "iamquant"
password <- "TheCatSatOnTheMat99"
client_id <- "kzdr85j9hv"

cat("1. Logging in...\n")
client$Login(User = user, Client = client_id, Password = password)

cat("\n2. Calling SetCurrentSettings (Expect NPE, but capturing XML)...\n")
tryCatch(
    {
        # We don't care about async portfolio here, we just want to see the XML structure
        # that caused the NPE.
        client$SetCurrentSettings(CurrentSettings = list(Benchmark = "MSCIJPND", BenchmarkOwner = "SYSTEM"))
    },
    error = function(e) {
        cat("Caught expected error:\n")
        print(e)
    }
)

sink()
