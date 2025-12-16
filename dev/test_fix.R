# Test Fix for SetCurrentSettings
devtools::load_all("c:/workspace/soapr")

# Redirect output
con <- file("c:/workspace/soapr/test_fix_log.txt", open = "wt")
sink(con, type = "output")
sink(con, type = "message")

wsdl_url <- "https://www.barraone.com/bdti/BDTInteractive?wsdl"
cat("Parsing WSDL...\n")
doc <- soapr::WSDLParser$new(wsdl_url)

service <- soapr::Service$new(wsdl_url)
client <- service$client
tf <- client$factory

user <- "iamquant"
password <- "TheCatSatOnTheMat99"
client_id <- "kzdr85j9hv"

client$Login(User = user, Client = client_id, Password = password)

# Set AdHoc Portfolio
# Need to set portfolio to avoid "No session portfolio" error
mid <- tf$create("MID")
mid$ID <- "2046251"
mid$IDType <- "SEDOL"
pos <- tf$create("Position")
pos$Value <- 1
pos$Currency <- "USD"
pos$MID <- list(mid)
p <- tf$create("Portfolio")
p$Type <- "WEIGHT"
p$PortfolioValue <- 1
p$PortfolioCurrency <- "USD"
p$Pos <- list(pos)
client$SetAdHocPortfolio(Portfolio = p)

# Inspect CurrentSettings type definition
cat("\n--- Inspecting CurrentSettings Type ---\n")
cs_type <- doc$get_type_definitions()[["CurrentSettings"]]
print(str(cs_type))

# Construct KeyValue list
cat("\n--- Constructing KeyValue List ---\n")
kv1 <- tf$create("KeyValue")
kv1$Key <- "Benchmark"
kv1$Value <- "MSCIJPND"

kv2 <- tf$create("KeyValue")
kv2$Key <- "BenchmarkOwner"
kv2$Value <- "SYSTEM"

# In dynamic client, if we want to pass a list for a complex type argument "CurrentSettings", we usually pass a list of soap_objects?
# Or do we create a "CurrentSettings" object which IS a list?
# If CurrentSettings is a sequence, it acts like a list.

# Attempt 1: Using a list directly as CurrentSettings
# The element name in the request will be 'CurrentSettings' (from argument name).
# The children will be serialized from the list items.
# XMLSerializer handles list by serializing each item with the PARENT name?
# Wait, XMLSerializer:106 `self$serialize(parent, name, item, ns)`
# If I pass `CurrentSettings = list(kv1, kv2)`, `name` is "CurrentSettings".
# It will produce:
# <CurrentSettings>...kv1...</CurrentSettings>
# <CurrentSettings>...kv2...</CurrentSettings>
# This is NOT what we want if `CurrentSettings` is a container element that holds KeyValues.

# We want:
# <CurrentSettings>
#   <KeyValue>...</KeyValue>
#   <KeyValue>...</KeyValue>
# </CurrentSettings>

# To achieve this structure:
# We pass ONE object for "CurrentSettings".
# This object should be a soap_object named "CurrentSettings" (so it has the right type metadata if needed?)
# But soap_object validation/creation is loose in dynamic client.

# Using tf$create("CurrentSettings") gives us a soap_object.
# We then add unnamed elements? list elements?
# If we add:
# settings$foo <- kv1
# Serializer does: for elem_name in names(value).
# If element is named, it adds child with that name.
# WE WANT the child name to be "KeyValue".
# So `settings$KeyValue <- list(kv1, kv2)`?

# Let's check `cs_type` output to be sure.
# If `CurrentSettings` has an element named "KeyValue" that is unbounded.

# I will try constructing it assuming it has a field named "KeyValue" which is a list.

# Attempt:
tryCatch(
    {
        settings <- tf$create("CurrentSettings")
        # Assuming the repeating element name is "KeyValue"
        settings$KeyValue <- list(kv1, kv2)

        trace(httr::POST, tracer = quote(print(body)), print = FALSE)

        cat("Calling SetCurrentSettings with KeyValue list...\n")
        client$SetCurrentSettings(CurrentSettings = settings)

        cat("Check Settings...\n")
        print(client$GetCurrentSettings())
    },
    error = function(e) {
        cat("Failed:\n")
        print(e)
    }
)

client$Logout()
