devtools::load_all()
wsdl_path <- "tests/testthat/testdata/valid_wsdls/BDTInteractive.xml"
parser <- WSDLParser$new(wsdl_path)
services <- parser$get_services()
print("Services:")
print(services)

if (length(services) > 0) {
    port <- services[[1]]$ports[[1]]
    print("Port:")
    print(port)

    ops <- parser$get_operations(port$binding)
    print("Operations (first 5):")
    print(head(ops, 5))

    # Check for NULL names
    null_names <- Filter(function(x) is.null(x$name), ops)
    if (length(null_names) > 0) {
        print("Found operations with NULL names!")
        print(null_names)
    }
}
