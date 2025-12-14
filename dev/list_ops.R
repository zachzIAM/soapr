devtools::load_all("c:/workspace/soapr")
wsdl_url <- "https://www.barraone.com/bdti/BDTInteractive?wsdl"
doc <- soapr::WSDLParser$new(wsdl_url)
services <- doc$get_services()
binding_name <- NULL
for (svc in services) {
    for (port in svc$ports) {
        binding_name <- port$binding
        break
    }
    if (!is.null(binding_name)) break
}

if (is.null(binding_name)) {
    stop("No binding found")
}

ops <- doc$get_operations(binding_name)

sink("ops_info.txt")

cat("\n--- SOAP Actions ---\n")
op_name <- "SetCurrentSettings"
if (!is.null(ops[[op_name]])) {
    cat(sprintf("%s Action: %s\n", op_name, ops[[op_name]]$soap_action))
}
op_name <- "SetCurrentSettingsKeyVal"
if (!is.null(ops[[op_name]])) {
    cat(sprintf("%s Action: %s\n", op_name, ops[[op_name]]$soap_action))
}

sink()
