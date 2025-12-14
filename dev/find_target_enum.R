# Find Enum in Dump
devtools::load_all("c:/workspace/soapr")

wsdl_url <- "https://www.barraone.com/bdti/BDTInteractive?wsdl"
parser <- soapr::WSDLParser$new(wsdl_url)
types <- parser$get_type_definitions()

# Filter types to only those in the dump (if different, but they should be same)
# We trust 'names(types)' is what we see in dump.

for (type_name in names(types)) {
    def <- types[[type_name]]

    # Elements
    if (!is.null(def$elements)) {
        for (elem in def$elements) {
            # Required?
            min_occurs <- if (!is.null(elem$minOccurs)) as.integer(elem$minOccurs) else 1
            if (min_occurs > 0) {
                # Is it an enum?
                elem_type_name <- elem$type
                # Remove namespace prefix if present
                clean_type <- sub(".*:", "", elem_type_name)

                elem_type <- types[[clean_type]]
                if (!is.null(elem_type) && elem_type$kind == "simple" && !is.null(elem_type$values)) {
                    cat(sprintf("TARGET: %s.%s (Type: %s)\n", type_name, elem$name, clean_type))
                    # print(elem_type$values)
                }
            }
        }
    }
}
