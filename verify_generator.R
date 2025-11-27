devtools::load_all()

wsdl_path <- "tests/testthat/testdata/valid_wsdls/BDTInteractive.xml"
output_path <- "generated_packages"
package_name <- "BDTIClient"

if (dir.exists(output_path)) {
    unlink(output_path, recursive = TRUE)
}
dir.create(output_path)

print("Generating package...")
generate_package(wsdl_path, output_path, package_name)

print("Checking generated files...")
pkg_dir <- file.path(output_path, package_name)
print(list.files(pkg_dir, recursive = TRUE))

print("Checking generated types.R content (first 20 lines)...")
types_file <- file.path(pkg_dir, "R", "types.R")
if (file.exists(types_file)) {
    writeLines(readLines(types_file, n = 20))
}

print("Checking generated client.R content (first 20 lines)...")
client_file <- file.path(pkg_dir, "R", "client.R")
if (file.exists(client_file)) {
    writeLines(readLines(client_file, n = 20))
}
