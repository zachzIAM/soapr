# Inspect CurrentSettings Type
devtools::load_all("c:/workspace/soapr")

wsdl_url <- "https://www.barraone.com/bdti/BDTInteractive?wsdl"
parser <- soapr::WSDLParser$new(wsdl_url)

# Get type definitions
types <- parser$get_type_definitions()
ct <- types[["CurrentSettings"]]

if (!is.null(ct)) {
    # Check elements or attributes
    # Based on previous analysis, these are attributes but soapr might have mapped them.
    # Actually, previous analysis said they are attributes.

    elem <- ct$elements[["ParamVarConfidenceLevel"]]
    attr <- ct$attributes[["ParamVarConfidenceLevel"]]

    if (!is.null(elem)) {
        cat("Element ParamVarConfidenceLevel found.\n")
        print(elem)
        cat("minOccurs:", elem$minOccurs, "\n")
    } else if (!is.null(attr)) {
        cat("Attribute ParamVarConfidenceLevel found.\n")
        print(attr)
        # Attributes are required if use="required"
        cat("use:", attr$use, "\n")
    } else {
        cat("ParamVarConfidenceLevel not found in CurrentSettings.\n")
        print(names(ct$elements))
        print(names(ct$attributes))
    }
} else {
    cat("Type 'CurrentSettings' not found.\n")
}
