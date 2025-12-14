# Verify Static Client Customizations
devtools::load_all("c:/workspace/soapr")
# Load the installed package
devtools::load_all("c:/workspace/soapr/generated_packages/barrawana")

# Redirect output
con <- file("c:/workspace/soapr/verify_static_log.txt", open = "wt")
sink(con, type = "output")
sink(con, type = "message")

cat("Creating TypeFactory...\n")
tf <- barrawana::TypeFactory$new()

cat("Creating CurrentSettings...\n")
settings <- tf$CurrentSettings(
    BaseCurrency = "EUR",
    Benchmark = "MSCIJPND"
)

cat("Inspecting SOAP Attributes of CurrentSettings object...\n")
attrs <- attr(settings, "soap_attrs")
print(attrs)

# Check if BaseCurrency is in attributes
if ("BaseCurrency" %in% attrs) {
    cat("FAIL: BaseCurrency is still an attribute!\n")
} else {
    cat("SUCCESS: BaseCurrency is NOT an attribute (Element).\n")
}

cat("\n--- Testing Operation ---\n")
client <- barrawana::new_client()
client$Login(User = "iamquant", Client = "kzdr85j9hv", Password = "TheCatSatOnTheMat99")

# Setup Portfolio
mid <- tf$MID(ID = "2046251", IDType = "SEDOL")
pos <- tf$Position(Value = 1, Currency = "USD", MID = list(mid))
p <- tf$Portfolio(Type = "WEIGHT", PortfolioValue = 1, PortfolioCurrency = "USD", Pos = list(pos))
client$SetAdHocPortfolio(Portfolio = p)

settings_full <- tf$CurrentSettings(
    BaseCurrency = "EUR",
    Benchmark = "MSCIJPND",
    IsBalanceCurrency = TRUE
)

cat("Calling SetCurrentSettings...\n")
tryCatch(
    {
        client$SetCurrentSettings(CurrentSettings = settings_full)
        cat("SetCurrentSettings returned SUCCESS (No Crash).\n")
    },
    error = function(e) {
        cat("SetCurrentSettings Failed:\n")
        print(e)
    }
)

client$Logout()
