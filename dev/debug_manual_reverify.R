# Debug Manual SetCurrentSettings - Reverification with Extended Payload
devtools::load_all("c:/workspace/soapr")
library(httr)
library(xml2)

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

cat("3.5. Getting Current Settings for Read-Modify-Write...\n")
current_settings <- client_obj$GetCurrentSettings()

cat("3.6. Mapping Current Settings to Attributes...\n")
# Create a named list for attributes
attr_map <- list()
if (!is.null(current_settings$CurrentSettings)) {
    for (item in current_settings$CurrentSettings) {
        if (!is.null(item$Key)) {
            attr_map[[item$Key]] <- item$Value
        }
    }
}

# Update LossTarget (Reverting to LossTarget for struct test as it is a direct member)
target_key <- "LossTarget"
current_val <- attr_map[[target_key]]
if (is.null(current_val)) current_val <- "5.0"

# Toggle value
if (current_val == "5.8") {
    new_val <- "6.0"
} else {
    new_val <- "5.8"
}

cat("Updating", target_key, "from", current_val, "to", new_val, "\n")

# Manual XML Construction for SetCurrentSettings (Struct Based)
# Structure: <ns1:SetCurrentSettings><ns1:CurrentSettings LossTarget="..."/></ns1:SetCurrentSettings>
# Confirmed by gSOAP soap_out_ns1__CurrentSettings using attributes
xml_body <- paste0(
    '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:ns1="http://barra.com/cp/bdti" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">',
    "<soapenv:Header/>",
    "<soapenv:Body>",
    "<ns1:SetCurrentSettings>",
    sprintf('<ns1:CurrentSettings LossTarget="%s"/>', new_val),
    "</ns1:SetCurrentSettings>",
    "</soapenv:Body>",
    "</soapenv:Envelope>"
)

# Explicitly set SOAPAction for SetCurrentSettings
soap_action <- "http://barra.com/cp/bdti/SetCurrentSettings"

cat("Sending SetCurrentSettings Request (Struct-Based) with Manual XML:\n")
cat(xml_body, "\n")

# Send the request
res <- httr::POST(
    url = "https://www.barraone.com/bdti/BDTInteractive", # Using original URL
    body = xml_body,
    handle = client_obj$handle, # Using original handle
    httr::add_headers(
        "Content-Type" = "text/xml; charset=utf-8",
        "SOAPAction" = soap_action
    )
)

cat("Status Code:", status_code(res), "\n")
flush.console()
body_text <- content(res, "text", encoding = "UTF-8")
cat("--- BODY START ---\n")
cat(body_text, "\n")
cat("--- BODY END ---\n")
flush.console()

if (status_code(res) != 200) {
    cat("FAILURE: Server returned non-200 status.\n")
    # quit(status = 1)
    # Don't quit immediately, let's see check
}

# 5. GetCurrentSettings (Verification with InclSessionOverride)
cat("5. calling GetCurrentSettings(InclSessionOverride='true')...\n")
Sys.sleep(5) # Give server time to persist
# Crucial Fix: Add InclSessionOverride="true" to see session-specific changes
current_settings_after <- client_obj$GetCurrentSettings(InclSessionOverride = "true")

# Dump settings for inspection
dump_file <- "c:/workspace/soapr/settings_dump.txt"
sink(dump_file)
print(current_settings_after)
sink()
cat("Settings dumped to", dump_file, "\n")

# Find Target
target_val <- NULL
if (!is.null(current_settings_after$CurrentSettings)) {
    for (item in current_settings_after$CurrentSettings) {
        if (!is.null(item$Key) && item$Key == target_key) {
            target_val <- item$Value
            break
        }
    }
}

if (is.null(target_val)) {
    message("FAILURE: ", target_key, " not found in response.")
    quit(status = 1)
}

# Check if value changed
if (target_val == new_val) {
    message("SUCCESS: ", target_key, " updated to ", new_val)
} else {
    message("FAILURE: ", target_key, " is '", target_val, "', expected '", new_val, "'")
    message("Original value was '", current_val, "'")
    quit(status = 1)
}
