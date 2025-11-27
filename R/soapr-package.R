#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

#' Generate a static SOAP client package
#'
#' @param wsdl_path Path or URL to the WSDL file.
#' @param output_path Directory where the package should be created.
#' @param package_name Name of the package to create.
#' @export
generate_package <- function(wsdl_path, output_path, package_name) {
    wsdl <- WSDLParser$new(wsdl_path)
    generator <- Generator$new(wsdl)
    generator$generate_package(output_path, package_name)
}
