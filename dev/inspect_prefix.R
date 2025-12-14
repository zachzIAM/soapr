# Inspect Prefix
devtools::load_all("c:/workspace/soapr")
library(xml2)
library(httr)

# Redirect output
con <- file("c:/workspace/soapr/inspect_prefix_log.txt", open = "wt")
sink(con, type = "output")
sink(con, type = "message")

wsdl_url <- "https://www.barraone.com/bdti/BDTInteractive?wsdl"
doc <- soapr::WSDLParser$new(wsdl_url)
client <- soapr::Client$new(doc)
client$Login(User = "iamquant", Client = "kzdr85j9hv", Password = "TheCatSatOnTheMat99")

# Set Valid Portfolio
mid <- client$factory$create("MID")
mid$ID <- "2046251"
mid$IDType <- "SEDOL"
pos <- client$factory$create("Position")
pos$Value <- 1
pos$Currency <- "USD"
pos$MID <- list(mid)
p <- client$factory$create("Portfolio")
p$Type <- "WEIGHT"
p$PortfolioValue <- 1
p$PortfolioCurrency <- "USD"
p$Pos <- list(pos)
client$SetAdHocPortfolio(Portfolio = p)

cat("\n--- Manual Request Constrution (Prefixed, Unqualified Attributes) ---\n")
# Construct XML manually
# <soap:Envelope ... xmlns:tns="http://barra.com/cp/bdti">
#   <soap:Body>
#     <tns:SetCurrentSettings>
#       <CurrentSettings Benchmark="MSCIJPND" .../>

envelope <- xml_new_root("soap:Envelope",
    "xmlns:soap" = "http://schemas.xmlsoap.org/soap/envelope/",
    "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
    "xmlns:xsd" = "http://www.w3.org/2001/XMLSchema",
    "xmlns:tns" = "http://barra.com/cp/bdti"
)
body <- xml_add_child(envelope, "soap:Body")
op <- xml_add_child(body, "tns:SetCurrentSettings")
# CurrentSettings Qualified with xsi:type + Elements
cs <- xml_add_child(op, "tns:CurrentSettings")
xml_attr(cs, "xsi:type") <- "tns:CurrentSettings"

# Children Unqualified
xml_add_child(cs, "Benchmark", "MSCIJPND")
xml_add_child(cs, "BaseCurrency", "EUR")
xml_add_child(cs, "IsBalanceCurrency", "true")

cat("Sending XML:\n")
print(envelope)

# Send
response <- POST(
    "https://www.barraone.com/bdti/BDTInteractive",
    body = as.character(envelope),
    content_type("text/xml; charset=utf-8"),
    add_headers(SOAPAction = ""),
    handle = client$handle
)

cat("Response:\n")
print(content(response, "text"))

cat("\n--- Checking Values ---\n")
curr <- client$GetCurrentSettings()
find_val <- function(res, key) {
    kvs <- res$CurrentSettings
    if (is.null(kvs)) kvs <- res
    for (kv in kvs) {
        if (kv$Key == key) {
            return(kv$Value)
        }
    }
    return(NA)
}
cat("Benchmark:", find_val(curr, "Benchmark"), "\n")
cat("BaseCurrency:", find_val(curr, "BaseCurrency"), "\n")

client$Logout()
