# Check Binding Style
devtools::load_all("c:/workspace/soapr")
library(xml2)

wsdl_url <- "https://www.barraone.com/bdti/BDTInteractive?wsdl"
doc <- read_xml(wsdl_url)
ns <- xml_ns(doc)

# Find binding
binding <- xml_find_first(doc, "//wsdl:binding/soap:binding", ns)
if (inherits(binding, "xml_node")) {
    cat("Binding Style:", xml_attr(binding, "style"), "\n")
    cat("Transport:", xml_attr(binding, "transport"), "\n")
} else {
    cat("Binding NOT FOUND.\n")
}

# Check Operation specific style
op_bind <- xml_find_first(doc, "//wsdl:binding/wsdl:operation[@name='SetCurrentSettings']/soap:operation", ns)
if (inherits(op_bind, "xml_node")) {
    cat("Operation SetCurrentSettings Style:", xml_attr(op_bind, "style"), "\n")
}
