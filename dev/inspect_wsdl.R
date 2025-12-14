# Inspect CurrentSettings

devtools::load_all("c:/workspace/soapr")
wsdl_url <- "https://www.barraone.com/bdti/BDTInteractive?wsdl"
wsdl <- soapr::WSDLParser$new(wsdl_url)
types <- wsdl$get_type_definitions()

cat("--- CurrentSettings Definition ---\n")
print(names(types[["CurrentSettings"]]$attributes))
print(names(types[["CurrentSettings"]]$elements))

cat("\n--- SetCurrentSettingsKeyVal Operation ---\n")
services <- wsdl$get_services()
ops <- wsdl$get_operations(services[[1]]$ports[[1]]$binding)
op <- ops[["SetCurrentSettingsKeyVal"]]
print(op)

cat("\n--- Input Message Parts ---\n")
# The input message name is usually op$input
# We need to find the message definition in the WSDL definitions
# Since we don't have direct access to messages via WSDLParser public API easily,
# we rely on what we know: the operation object usually contains the input parts if parsed.
print(op$elements)
# If op$elements is empty, it might be using a message reference.
# Let's check the type definition of the element with the same name as the operation,
# as it's likely document/literal wrapped.
cat("\n--- WSDL Object ---\n")
print(ls(wsdl))
print(length(wsdl$operations))
