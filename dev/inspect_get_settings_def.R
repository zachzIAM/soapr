# Inspect GetCurrentSettings Definition
devtools::load_all("c:/workspace/soapr")

wsdl_url <- "https://www.barraone.com/bdti/BDTInteractive?wsdl"
parser <- soapr::WSDLParser$new(wsdl_url)

# Get operation details
# We need to find the input message type for GetCurrentSettings
# Usually the element name matches the operation name in doc/lit wrapped
type_def <- parser$get_type_definitions()[["GetCurrentSettings"]]

if (!is.null(type_def)) {
    override <- type_def$elements$InclSessionOverride
    if (!is.null(override)) {
        print(override)
        cat("minOccurs:", override$minOccurs, "\n")
    } else {
        cat("InclSessionOverride element not found.\n")
    }
} else {
    cat("Type 'GetCurrentSettings' not found.\n")
}
