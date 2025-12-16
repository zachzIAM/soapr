# Test Fix 2 (Corrected)
devtools::load_all("c:/workspace/soapr")

# Redirect output
con <- file("c:/workspace/soapr/test_fix_log_2.txt", open = "wt")
sink(con, type = "output")
sink(con, type = "message")

wsdl_url <- "https://www.barraone.com/bdti/BDTInteractive?wsdl"
doc <- soapr::WSDLParser$new(wsdl_url)
service <- soapr::Service$new(wsdl_url)
client <- service$client
tf <- client$factory
client$Login(User = "iamquant", Client = "kzdr85j9hv", Password = "TheCatSatOnTheMat99")

# Set AdHoc Portfolio
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

# Check Operations
cat("\n--- Operations ---\n")
if (!is.null(client$operations$SetCurrentSettingsKeyVal)) {
    cat("SetCurrentSettingsKeyVal found in client operations.\n")
} else {
    cat("SetCurrentSettingsKeyVal NOT found in client operations.\n")
}

# Case 1: SetCurrentSettings using ELEMENTS (Force Elements)
cat("\n--- Case 1: SetCurrentSettings (Force Elements) ---\n")
tryCatch(
    {
        settings <- tf$create("CurrentSettings")
        # Check attributes
        cat("Default soap_attrs:\n")
        print(attr(settings, "soap_attrs"))

        # Remove attributes to force element serialization
        attr(settings, "soap_attrs") <- NULL

        settings$Benchmark <- "MSCIJPND"
        settings$BenchmarkOwner <- "SYSTEM"

        trace(httr::POST, tracer = quote(print(body)), print = FALSE)

        client$SetCurrentSettings(CurrentSettings = settings)

        cat("Check Settings Case 1...\n")
        print(client$GetCurrentSettings())
    },
    error = function(e) {
        cat("Case 1 Failed:\n")
        print(e)
    }
)

# Case 2: SetCurrentSettingsKeyVal with KeyValue List
cat("\n--- Case 2: SetCurrentSettingsKeyVal (KeyValue List) ---\n")
tryCatch(
    {
        # Attempting to use SetCurrentSettingsKeyVal which might expect a sequence of KeyValues

        kv1 <- tf$create("KeyValue")
        kv1$Key <- "Benchmark"
        kv1$Value <- "MSCIJPND"

        kv2 <- tf$create("KeyValue")
        kv2$Key <- "BenchmarkOwner"
        kv2$Value <- "SYSTEM"

        # Try passing list of KVs as CurrentSettings arg
        # If the operation wrapper expects 'CurrentSettings' arg to be the ComplexType wrapper:
        settings_kv <- tf$create("CurrentSettings")
        attr(settings_kv, "soap_attrs") <- NULL
        # Add children
        settings_kv$KeyValue <- list(kv1, kv2)

        client$SetCurrentSettingsKeyVal(CurrentSettings = settings_kv)

        cat("Check Settings Case 2...\n")
        print(client$GetCurrentSettings())
    },
    error = function(e) {
        cat("Case 2 Failed:\n")
        print(e)
    }
)

# Case 3: SetCurrentSettings (Standard) with BaseCurrency (check entity error again)
cat("\n--- Case 3: SetCurrentSettings with BaseCurrency (Force Element) ---\n")
tryCatch(
    {
        settings <- tf$create("CurrentSettings")
        attr(settings, "soap_attrs") <- NULL # Force element
        settings$BaseCurrency <- "EUR"

        client$SetCurrentSettings(CurrentSettings = settings)
        cat("Case 3 Returned.\n")
    },
    error = function(e) {
        cat("Case 3 Failed:\n")
        print(e)
    }
)

client$Logout()
