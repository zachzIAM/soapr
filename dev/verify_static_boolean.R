# Verify Static Client Boolean Support
# Point to the local soapr to ensure we use the updated code (in case it wasn't installed globally)
devtools::load_all("c:/workspace/soapr")
# Load the generated package
devtools::load_all("c:/workspace/barrawana")
library(httr)
# httr::set_config(httr::verbose())

# Setup
user <- "iamquant"
password <- "TheCatSatOnTheMat99"
client_id <- "kzdr85j9hv"

cat("Creating static client...\n")
client <- barrawana::new_client()

cat("Logging in...\n")
client$Login(User = user, Client = client_id, Password = password)

cat("Setting Portfolio...\n")
client$SetCurrentPortfolio(PortfolioName = "FOGGEQUI", PortfolioOwner = "InvestecAd")
Sys.sleep(2)

cat("Testing Missing Argument (Expect Error)...\n")
tryCatch(
    {
        client$GetCurrentSettings()
        cat("FAILURE: GetCurrentSettings succeeded without mandatory argument!\n")
    },
    error = function(e) {
        cat("SUCCESS: Caught expected error for missing argument:\n")
        print(e)
    }
)

cat("Calling GetCurrentSettings(InclSessionOverride = TRUE)...\n")
settings <- client$GetCurrentSettings(InclSessionOverride = TRUE)

cat("Success! Settings retrieved.\n")
# If this succeeds, it means XML was valid ("true" sent)
# Start checking for known key to be sure
if (!is.null(settings$CurrentSettings)) {
    cat("CurrentSettings found in response.\n")
} else {
    cat("WARNING: CurrentSettings is empty/null, but call succeeded.\n")
}
