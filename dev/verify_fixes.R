# Verify Fixes

# 1. Load updated soapr
devtools::load_all("c:/workspace/soapr")

# Redirect output to file
con <- file("c:/workspace/soapr/verify_log.txt", open = "wt")
sink(con, type = "output")
sink(con, type = "message")

# 2. Regenerate barrawana package
cat("Regenerating barrawana...\n")
wsdl_url <- "https://www.barraone.com/bdti/BDTInteractive?wsdl"
wsdl <- soapr::WSDLParser$new(wsdl_url)
generator <- soapr::Generator$new(wsdl)
generator$generate_package("c:/workspace/soapr/generated_packages", "barrawana")
cat("Regeneration complete.\n")

# 3. Run validation scripts
cat("\n--- Running validate_static.R ---\n")
tryCatch(
    {
        # Load package
        cat("Loading generated package...\n")
        devtools::load_all("./generated_packages/barrawana")
        cat("Loaded generated package.\n")

        cat("Creating TypeFactory...\n")
        tf <- barrawana::TypeFactory$new()
        cat("Created TypeFactory.\n")

        cat("Creating Client...\n")
        client <- barrawana::new_client()
        cat("Created Client.\n")

        user <- "iamquant"
        password <- "TheCatSatOnTheMat99"
        client_id <- "kzdr85j9hv"

        cat("Logging in...\n")
        client$Login(User = user, Client = client_id, Password = password)
        cat("Logged in.\n")

        # Create valid portfolio
        mid <- tf$MID(ID = "2046251", IDType = "SEDOL")
        pos <- tf$Position(Value = 1, Currency = "USD", MID = list(mid))
        p <- tf$Portfolio(
            Type = "WEIGHT",
            PortfolioValue = 1,
            PortfolioCurrency = "USD",
            Pos = list(pos)
        )

        cat("Calling SetAdHocPortfolio...\n")
        client$SetAdHocPortfolio(Portfolio = p)
        cat("SetAdHocPortfolio SUCCESS!\n")

        # Get current settings
        cat("Getting Current Settings...\n")
        current_settings_resp <- client$GetCurrentSettings()

        # The response is a list of KeyValue pairs (because CurrentSettings is a list of KeyValue in the output)
        # BUT SetCurrentSettings expects a CurrentSettings object (which has attributes).
        # This is a mismatch in the WSDL?
        # Let's check the type of current_settings_resp
        print(class(current_settings_resp))
        print(attr(current_settings_resp, "soap_type"))

        # If GetCurrentSettings returns a list of KeyValues, we can't just pass it back to SetCurrentSettings.
        # We need to construct a CurrentSettings object using the values.

        # Let's try to construct a full CurrentSettings object manually
        cat("\n--- Case 1: SetCurrentSettings (Full Object) ---\n")
        tryCatch(
            {
                settings <- tf$CurrentSettings(
                    BaseCurrency = "EUR",
                    Benchmark = "CASH",
                    BenchmarkOwner = "system",
                    ParamVarHorizon = 10,
                    ParamVarConfidenceLevel = 99,
                    LossTarget = 5.5,
                    IsBalanceCurrency = TRUE
                )

                # Trace XML
                trace(httr::POST, tracer = quote(print(body)), print = FALSE)

                client$SetCurrentSettings(CurrentSettings = settings)
                cat("Case 1: SUCCESS (Check if settings updated)\n")
                print(client$GetCurrentSettings())
            },
            error = function(e) {
                cat("Case 1: FAILED with error:\n")
                print(e)
            }
        )

        client$Logout()
    },
    error = function(e) {
        cat("Error in validate_static.R:\n")
        print(e)
        traceback()
    }
)
