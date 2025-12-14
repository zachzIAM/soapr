# Inspect WSDL CurrentSettings Definition
library(xml2)

wsdl_url <- "https://www.barraone.com/bdti/BDTInteractive?wsdl"
doc <- read_xml(wsdl_url)
ns <- xml_ns(doc)

# Find CurrentSettings complexType
# It might be named "CurrentSettings" or similar.
# The type of the element "CurrentSettings" in "SetCurrentSettings"
# But gSOAP says "ns1__CurrentSettings".
# Let's search for complexType name="CurrentSettings"
ct <- xml_find_first(doc, ".//xsd:complexType[@name='CurrentSettings']", ns = ns)

if (inherits(ct, "xml_node")) {
    cat("Found ComplexType: CurrentSettings\n")

    # Check for attributes
    attrs <- xml_find_all(ct, ".//xsd:attribute", ns = ns)
    cat(sprintf("Number of attributes: %d\n", length(attrs)))
    for (a in attrs) {
        cat(sprintf(
            "  Attribute: %s (Type: %s, Use: %s)\n",
            xml_attr(a, "name"),
            xml_attr(a, "type"),
            xml_attr(a, "use")
        ))
    }

    # Check for elements
    elems <- xml_find_all(ct, ".//xsd:sequence/xsd:element", ns = ns)
    cat(sprintf("Number of elements: %d\n", length(elems)))
    for (e in elems) {
        cat(sprintf("  Element: %s (Type: %s)\n", xml_attr(e, "name"), xml_attr(e, "type")))
    }
} else {
    cat("Could not find ComplexType 'CurrentSettings'.\n")
}
