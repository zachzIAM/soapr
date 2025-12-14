# Check xml2 namespace behavior
library(xml2)

root <- xml_new_root("Envelope",
    "xmlns:ns1" = "http://example.com/ns1",
    "xmlns:soapenv" = "http://schemas.xmlsoap.org/soap/envelope/"
)

body <- xml_add_child(root, "soapenv:Body")
# Add child with namespace URI
child <- xml_add_child(body, "MyElement", xmlns = "http://example.com/ns1")

cat(as.character(root))
cat("\n")
