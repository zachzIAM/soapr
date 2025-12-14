# Dump Types
devtools::load_all("c:/workspace/soapr")
wsdl_url <- "https://www.barraone.com/bdti/BDTInteractive?wsdl"
parser <- soapr::WSDLParser$new(wsdl_url)
types <- parser$get_type_definitions()
cat(names(types), file = "c:/workspace/soapr/types_dump.txt", sep = "\n")
