# Verify Fixed soapr Client with Async Portfolio
devtools::load_all("c:/workspace/soapr")

# Redirect output
con <- file("c:/workspace/soapr/verify_fix_log.txt", open = "wt")
sink(con, type = "output")
sink(con, type = "message")

# Trace httr::POST for visibility
trace(httr::POST, tracer = quote(print(body)), print = FALSE)

wsdl_url <- "https://www.barraone.com/bdti/BDTInteractive?wsdl"

# Use force_qualified based on my analysis of the "cs is null" error
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

cat("\n2. Setting Portfolio (Async)...\n")
status <- client$SetCurrentPortfolio(PortfolioName = "FOGGEQUI", PortfolioOwner = "InvestecAd")

# Async Wait Loop (User provided)
if (!is.null(status$StatusResponse$RequestId)) {
    req_id <- status$StatusResponse$RequestId
    is_done <- status$StatusResponse$IsDone

    cat("Waiting for ReqID:", req_id, "...")
    while (is_done == "false") {
        Sys.sleep(1)
        cat(".")
        check_resp <- client$CheckRequestStatus(RequestId = req_id)
        # Handle potential nesting diffs in CheckRequestStatus response if any
        is_done <- check_resp$StatusResponse$IsDone
    }
    cat(" Done.\n")
} else {
    cat("No RequestId, assuming sync success.\n")
}

cat("\n3. Checking Initial Settings...\n")
init_s <- client$GetCurrentSettings()
print(init_s$CurrentSettings[sapply(init_s$CurrentSettings, function(x) x$Key == "Benchmark")])

cat("\n4. Calling SetCurrentSettings (Target: MSCIJPND)...\n")
# Using the standard operation structure (not KeyVal)
# The patched XMLSerializer should handle this Named List correctly now.
res <- client$SetCurrentSettings(CurrentSettings = list(
    Benchmark = "MSCIJPND",
    BenchmarkOwner = "SYSTEM"
))
print(res)

cat("\n5. Verifying Update...\n")
curr_s <- client$GetCurrentSettings()
print(curr_s$CurrentSettings[sapply(curr_s$CurrentSettings, function(x) x$Key == "Benchmark")])

client$Logout()
sink()
