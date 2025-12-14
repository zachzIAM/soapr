# Find Required Enum
devtools::load_all("c:/workspace/soapr")

wsdl_url <- "https://www.barraone.com/bdti/BDTInteractive?wsdl"
parser <- soapr::WSDLParser$new(wsdl_url)
types <- parser$get_type_definitions()
cat("DEBUG: Types count in finder:", length(types), "\n")
matches <- grep("JobExportSettings", names(types), value = TRUE)
cat("DEBUG: Grep matches for JobExportSettings:", paste(matches, collapse = ", "), "\n")

cat("Scanning for required enums...\n")

for (type_name in names(types)) {
    def <- types[[type_name]]

    # Check elements
    if (!is.null(def$elements)) {
        for (elem in def$elements) {
            # Is it required?
            min_occurs <- if (!is.null(elem$minOccurs)) as.integer(elem$minOccurs) else 1
            if (min_occurs > 0) {
                # Is it an enum?
                elem_type <- types[[elem$type]]
                if (!is.null(elem_type) && elem_type$kind == "simple" && !is.null(elem_type$values)) {
                    cat(sprintf("FOUND ELEMENT: %s.%s (Type: %s) -> Required Enum\n", type_name, elem$name, elem$type))
                    print(elem_type$values)
                }
            }
        }
    }

    # Check attributes
    if (!is.null(def$attributes)) {
        for (attr in def$attributes) {
            # Is it required?
            is_req <- !is.null(attr$use) && attr$use == "required"
            if (is_req) {
                # Is it an enum?
                attr_type <- types[[attr$type]]
                if (!is.null(attr_type) && attr_type$kind == "simple" && !is.null(attr_type$values)) {
                    cat(sprintf("FOUND ATTRIBUTE: %s.%s (Type: %s) -> Required Enum\n", type_name, attr$name, attr$type))
                    print(attr_type$values)
                }
            }
        }
    }
}
