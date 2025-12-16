# Validate Automatic Fix
devtools::load_all("c:/workspace/soapr")

# Logging
sink("c:/workspace/soapr/validate_auto_log.txt", split = TRUE)

wsdl_url <- "https://www.barraone.com/bdti/BDTInteractive?wsdl"

# NO CUSTOMIZATIONS - Should work automatically now using WSDL info
cat("Initializing Client (No Customizations)...\n")
client <- soapr::Client$new(soapr::WSDLParser$new(wsdl_url))

user <- "iamquant"
password <- "TheCatSatOnTheMat99"
client_id <- "kzdr85j9hv"

cat("1. Logging in...\n")
client$Login(User = user, Client = client_id, Password = password)
cat("Logged in.\n")

cat("2. Setting Analysis Date...\n")
client$SetAnalysisDate(AnalysisDate = "2025-11-30")

cat("3. Setting Portfolio (FOGGEQUI)...\n")
status <- client$SetCurrentPortfolio(PortfolioName = "FOGGEQUI", PortfolioOwner = "InvestecAd")

if (!is.null(status$StatusResponse$RequestId)) {
    req_id <- status$StatusResponse$RequestId
    is_done <- status$StatusResponse$IsDone
    cat("Waiting for ReqID:", req_id, "...")
    while (is_done == "false") {
        Sys.sleep(1)
        cat(".")
        check_resp <- client$CheckRequestStatus(RequestId = req_id)
        is_done <- check_resp$StatusResponse$IsDone
    }
    cat(" Done.\n")
}

# 4. SetCurrentSettings (Target Test)
target_loss <- "88.88"
cat("4. Setting LossTarget to", target_loss, "...\n")

# Use named list for complex type parameters
# soapr should automatically:
# 1. Qualify <CurrentSettings> (element form default = qualified)
# 2. Unqualify attributes (attribute form default = unqualified)
res <- client$SetCurrentSettings(CurrentSettings = list(
    LossTarget = target_loss,
    Benchmark = "MSCIJPND", # Providing other common fields just in case
    BenchmarkOwner = "SYSTEM"
))

# Check status if possible (returns response object)
# Usually SetCurrentSettings returns status
name_res <- names(res)
cat("Operation Result Names:", paste(name_res, collapse = ", "), "\n")

# 5. Verify
cat("5. Verifying...\n")
Sys.sleep(2)
curr_s <- client$GetCurrentSettings()

# Extract LossTarget
# Assuming structure: current_settings$CurrentSettings$LossTarget (unqualified named list keys)
val <- curr_s$CurrentSettings$LossTarget

cat("Current LossTarget:", val, "\n")

if (!is.null(val) && as.character(val) == target_loss) {
    cat("SUCCESS: Automatic fix confirmed!\n")
} else {
    cat("FAILURE: Expected", target_loss, "got", val, "\n")
}

client$Logout()
sink()
