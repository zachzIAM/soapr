# Debug Soapr Definitions
devtools::load_all("c:/workspace/soapr")

wsdl_url <- "https://www.barraone.com/bdti/BDTInteractive?wsdl"
client_obj <- soapr::Client$new(soapr::WSDLParser$new(wsdl_url))

cat("Calling get_type_definitions()...\n")
types <- client_obj$wsdl_parser$get_type_definitions()

cat("Checking for CurrentSettings...\n")
if (!is.null(types[["CurrentSettings"]])) {
    def <- types[["CurrentSettings"]]
    print(def)

    cat("Checking attributes of CurrentSettings...\n")
    print(names(def$attributes))

    if ("LossTarget" %in% names(def$attributes)) {
        cat("LossTarget IS an attribute.\n")
    } else {
        cat("LossTarget is NOT an attribute. Checking elements...\n")
        print(names(def$elements))
        if ("LossTarget" %in% names(def$elements)) {
            cat("LossTarget IS an element.\n")
        }
    }
} else {
    cat("CurrentSettings type not found.\n")
    print(names(types))
}
