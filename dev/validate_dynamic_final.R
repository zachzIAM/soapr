# Validate Dynamic Client with SetCurrentSettings and InclSessionOverride
devtools::load_all("c:/workspace/soapr")
library(httr)
httr::set_config(httr::verbose())

# --- Setup ---
wsdl_url <- "https://www.barraone.com/bdti/BDTInteractive?wsdl"
user <- "iamquant"
password <- "TheCatSatOnTheMat99"
client_id <- "kzdr85j9hv"

client_obj <- soapr::Client$new(soapr::WSDLParser$new(wsdl_url))

# 1. Login
cat("1. Logging in...\n")
client_obj$Login(User = user, Client = client_id, Password = password)
cat("Logged in.\n")

# 2. Set Analysis Date
cat("2. Setting Analysis Date...\n")
client_obj$SetAnalysisDate(AnalysisDate = "2025-11-30")

# 3. Set Current Portfolio (Async Wait)
cat("3. Setting Current Portfolio (FOGGEQUI/InvestecAd)...\n")
status <- client_obj$SetCurrentPortfolio(PortfolioName = "FOGGEQUI", PortfolioOwner = "InvestecAd")
if (!is.null(status$StatusResponse$RequestId)) {
    req_id <- status$StatusResponse$RequestId
    is_done <- status$StatusResponse$IsDone
    cat("Waiting for Portfolio Selection (ReqID:", req_id, ")...")
    while (is_done == "false") {
        Sys.sleep(1)
        cat(".")
        check_resp <- client_obj$CheckRequestStatus(RequestId = req_id)
        is_done <- check_resp$StatusResponse$IsDone
    }
    cat(" Done.\n")
}

# 4. Read Current Settings (Initial)
cat("4. Reading Initial Settings (InclSessionOverride=TRUE)...\n")
# Note: Passing TRUE as boolean, checking if XMLSerializer handles it (converts to "true")
current_settings <- client_obj$GetCurrentSettings(InclSessionOverride = TRUE)

# Find current LossTarget
target_key <- "LossTarget"
current_val <- "5.0" # Default
if (!is.null(current_settings$CurrentSettings)) {
    for (item in current_settings$CurrentSettings) {
        if (!is.null(item$Key) && item$Key == target_key) {
            current_val <- item$Value
            break
        }
    }
}
cat("Current", target_key, ":", current_val, "\n")

# Toggle value
new_val <- if (current_val == "5.5") "6.0" else "5.5"
cat("Targeting", target_key, ":", new_val, "\n")

# 5. Set Current Settings
cat("5. Setting Current Settings via Dynamic Client...\n")
# Construct the CurrentSettings object.
# SetCurrentSettings takes 'CurrentSettings' argument which is the struct.
# The struct has 'LossTarget' as an attribute.
# soapr should handle this if we pass a list.
settings_list <- list(LossTarget = new_val)

# We call SetCurrentSettings
# The argument name in WSDL is likely 'CurrentSettings'
tryCatch(
    {
        client_obj$SetCurrentSettings(CurrentSettings = settings_list)
        cat("SetCurrentSettings call completed (200 OK likely implicit).\n")
    },
    error = function(e) {
        cat("FAILURE: SetCurrentSettings threw error:\n")
        print(e)
        quit(status = 1)
    }
)

# 6. Verify Update
cat("6. Verifying Update (InclSessionOverride=TRUE)...\n")
Sys.sleep(5)
current_settings_after <- client_obj$GetCurrentSettings(InclSessionOverride = TRUE)

# Find Target
target_val_after <- NULL
if (!is.null(current_settings_after$CurrentSettings)) {
    for (item in current_settings_after$CurrentSettings) {
        if (!is.null(item$Key) && item$Key == target_key) {
            target_val_after <- item$Value
            break
        }
    }
}

if (is.null(target_val_after)) {
    cat("FAILURE: LossTarget not found in response.\n")
    print(current_settings_after)
    quit(status = 1)
}

if (target_val_after == new_val) {
    cat("SUCCESS: LossTarget updated to", new_val, "\n")
} else {
    cat("FAILURE: LossTarget is", target_val_after, "expected", new_val, "\n")
    quit(status = 1)
}
