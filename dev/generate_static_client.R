# Generate Static Client
devtools::load_all("c:/workspace/soapr")

wsdl_url <- "https://www.barraone.com/bdti/BDTInteractive?wsdl"
parser <- soapr::WSDLParser$new(wsdl_url)
generator <- soapr::Generator$new(parser)

# Output to c:/workspace which should create c:/workspace/barrawana
target_dir <- "c:/workspace/barrawana"
if (dir.exists(target_dir)) {
    unlink(target_dir, recursive = TRUE)
}
generator$generate_package("c:/workspace", "barrawana")
