# Inspect Operations
devtools::load_all("c:/workspace/soapr")
sink("c:/workspace/soapr/inspect_op_log.txt")

wsdl_url <- "https://www.barraone.com/bdti/BDTInteractive?wsdl"
doc <- soapr::WSDLParser$new(wsdl_url)
client <- soapr::Client$new(doc)

op_keyval <- client$operations$SetCurrentSettingsKeyVal
op_settings <- client$operations$SetCurrentSettings

cat("--- SetCurrentSettings Operation ---\n")
print(str(op_keyval))

cat("\n--- SetCurrentSettings Operation ---\n")
print(str(op_settings))

# Also print the type definition for input messages if possible
# The operation structure usually has `input` field with message name
# but we need to trace back to type.
# I'll rely on `str(op)` showing enough info or I'll assume standard wrapped.
