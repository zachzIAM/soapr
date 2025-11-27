devtools::load_all()

wsdl_path <- "tests/testthat/testdata/valid_wsdls/BDTInteractive.xml"
service <- Service$new(wsdl_path)

user <- "<user>"
password <- "<password>"
client_id <- "<client_id>"

print("---------------------------------------------------")
print("Verifying Login with provided credentials...")
print(paste("User:", user))
print(paste("Client:", client_id))
print("---------------------------------------------------")

tryCatch(
    {
        # The Login operation usually returns an empty response on success or a specific object
        # We capture the result
        result <- service$client$Login(User = user, Client = client_id, Password = password)

        print("LOGIN SUCCESSFUL!")
        print("Server responded with:")
        print(result)
    },
    error = function(e) {
        print("LOGIN FAILED!")
        print("Error details:")
        print(e)
        if (!is.null(e$response)) {
            cat("Response Body:\n")
            cat(httr::content(e$response, as = "text", encoding = "UTF-8"))
            cat("\n")
        }
    }
)
print("---------------------------------------------------")
