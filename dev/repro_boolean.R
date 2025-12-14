# Repro Boolean Issue
devtools::load_all("c:/workspace/soapr")
library(httr)
httr::set_config(httr::verbose())

wsdl_url <- "https://www.barraone.com/bdti/BDTInteractive?wsdl"
user <- "iamquant"
password <- "TheCatSatOnTheMat99"
client_id <- "kzdr85j9hv"

client_obj <- soapr::Client$new(soapr::WSDLParser$new(wsdl_url))

cat("Logging in...\n")
client_obj$Login(User = user, Client = client_id, Password = password)

cat("Setting Portfolio...\n")
# Set a portfolio to ensure context exists
client_obj$SetCurrentPortfolio(PortfolioName = "FOGGEQUI", PortfolioOwner = "InvestecAd")
# Simple sleep to wait for async (lazy, but sufficient for this specific repro)
Sys.sleep(2)

cat("Calling GetCurrentSettings(InclSessionOverride = TRUE)...\n")
# This should ideally work, but user says it fails or requires "true"
settings <- client_obj$GetCurrentSettings(InclSessionOverride = TRUE)

print(settings)
