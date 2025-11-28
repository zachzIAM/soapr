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

print("Session Info:")
print(sessionInfo())

print("Running roxygen2::roxygenize() manually to debug...")
tryCatch(
    {
        if (requireNamespace("roxygen2", quietly = TRUE)) {
            roxygen2::roxygenize(
                file.path(output_path, package_name),
                clean = TRUE
            )
        } else {
            print("roxygen2 is NOT installed!")
        }
    },
    error = function(e) {
        print(paste("Error running roxygenize:", e$message))
    }
)

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

print("Checking for man directory...")
man_dir <- file.path(pkg_dir, "man")
if (dir.exists(man_dir)) {
    print(list.files(man_dir))
} else {
    print("man directory NOT found!")
}

print("Checking NAMESPACE...")
namespace_file <- file.path(pkg_dir, "NAMESPACE")
if (file.exists(namespace_file)) {
    writeLines(readLines(namespace_file))
} else {
    print("NAMESPACE NOT found!")
}
