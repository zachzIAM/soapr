# Generate Barrawana Package with Customizations
devtools::load_all("c:/workspace/soapr")

wsdl_url <- "https://www.barraone.com/bdti/BDTInteractive?wsdl"
opts <- list(
    convert_types = c("CurrentSettings")
)

# 1. Parse WSDL with customizations
doc <- soapr::WSDLParser$new(wsdl_url, customizations = opts)

# 2. Generate Package
gen <- soapr::Generator$new(doc)
output_dir <- "c:/workspace/soapr/generated_packages"
if (!dir.exists(output_dir)) dir.create(output_dir)

# output to: c:/workspace/soapr/generated_packages/barrawana
gen$generate_package(output_dir, "barrawana")

# 3. Install Package
# uninstalls previous version first
pkg_path <- file.path(output_dir, "barrawana")

try(remove.packages("barrawana"), silent = TRUE)
install.packages(pkg_path, repos = NULL, type = "source")

message("Barrawana package generated and installed with customizations.")
