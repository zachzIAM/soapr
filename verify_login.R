devtools::load_all()

wsdl_path <- "tests/testthat/testdata/valid_wsdls/BDTInteractive.xml"
service <- Service$new(wsdl_path)

user <- "iamquant"
password <- "TheCatSatOnTheMat99"
client_id <- "kzdr85j9hv"

print("Attempting Login...")
tryCatch(
    {
        service$client$Login(
            User = user,
            Client = client_id,
            Password = password
        )
        print("Login Successful!")
    },
    error = function(e) {
        print("Login Failed!")
        print(e)
        if (!is.null(e$response)) {
            cat("Response Body:\n")
            cat(httr::content(e$response, as = "text", encoding = "UTF-8"))
            cat("\n")
        }
    }
)
