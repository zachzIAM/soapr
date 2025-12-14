# Debug Manual SetCurrentSettings Workflow - Iteration 14
devtools::load_all("c:/workspace/soapr")

# Redirect output
con <- file("c:/workspace/soapr/debug_manual_log.txt", open = "wt")
sink(con, type = "output")
sink(con, type = "message")

# Trace httr::POST to see what soapr sends for SetCurrentPortfolio
trace(httr::POST, tracer = quote(print(body)), print = FALSE)

# 1. Setup
wsdl_url <- "https://www.barraone.com/bdti/BDTInteractive?wsdl"
# Using standard parser
doc <- soapr::WSDLParser$new(wsdl_url)
client <- soapr::Client$new(doc)
tf <- client$factory

user <- "iamquant"
password <- "TheCatSatOnTheMat99"
client_id <- "kzdr85j9hv"

cat("1. Logging in...\n")
client$Login(User = user, Client = client_id, Password = password)
cat("Logged in.\n")

# Helper to send raw XML
send_soap_xml <- function(xml_body_content) {
    envelope <- paste0(
        '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ',
        'xmlns:tns="http://www.barra.com/cp/bdti" ',
        'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">',
        "<soapenv:Header/>",
        "<soapenv:Body>",
        xml_body_content,
        "</soapenv:Body>",
        "</soapenv:Envelope>"
    )

    resp <- httr::POST(
        url = "https://www.barraone.com/bdti/BDTInteractive",
        body = envelope,
        httr::content_type("text/xml; charset=utf-8"),
        httr::add_headers(SOAPAction = ""),
        handle = client$handle
    )

    cat("Status:", httr::status_code(resp), "\n")
    cat("Response:\n")
    cat(httr::content(resp, "text"), "\n")
    return(resp)
}

# 2. Set Analysis Date
cat("\n2. Setting Analysis Date...\n")
client$SetAnalysisDate(AnalysisDate = "2025-11-30")

# 3. Set Current Portfolio (Async Wait)
cat("\n3. Setting Current Portfolio (FOGGEQUI/InvestecAd)...\n")
tryCatch(
    {
        status <- client$SetCurrentPortfolio(PortfolioName = "FOGGEQUI", PortfolioOwner = "InvestecAd")
        if (!is.null(status$StatusResponse$RequestId)) {
            req_id <- status$StatusResponse$RequestId
            is_done <- status$StatusResponse$IsDone
            cat("Waiting for Portfolio Selection (ReqID:", req_id, ")...")
            while (is_done == "false") {
                Sys.sleep(1)
                cat(".")
                check_resp <- client$CheckRequestStatus(RequestId = req_id)
                is_done <- check_resp$StatusResponse$IsDone
            }
            cat(" Done.\n")
        }
        cat("Current Portfolio Set.\n")
    },
    error = function(e) {
        print(e)
    }
)

# Check Initial Settings
cat("\n3b. Initial Settings...\n")
init_settings <- client$GetCurrentSettings()
# Print LossTarget specifically
print(init_settings$CurrentSettings[sapply(init_settings$CurrentSettings, function(x) x$Key == "LossTarget")])
print(init_settings$CurrentSettings[sapply(init_settings$CurrentSettings, function(x) x$Key == "Benchmark")])

# 4. ITERATION 14: Q Wrapper, U Attributes (LossTarget), Explicit Type
# Testing if LossTarget=12.34 updates.
cat("\n4. Testing Variation 14 (LossTarget, Explicit Type)...\n")
xml_14 <- '
<tns:SetCurrentSettings xmlns:tns="http://barra.com/cp/bdti">
    <tns:CurrentSettings xsi:type="tns:CurrentSettings" LossTarget="12.34" />
</tns:SetCurrentSettings>
'
send_soap_xml(xml_14)

# Check Settings
cat("Checking 14...\n")
c14 <- client$GetCurrentSettings()
print(c14$CurrentSettings[sapply(c14$CurrentSettings, function(x) x$Key == "LossTarget")])
print(c14$CurrentSettings[sapply(c14$CurrentSettings, function(x) x$Key == "Benchmark")])


cat("\n7. Logging out...\n")
client$Logout()
