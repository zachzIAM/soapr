# Inspect Function Body
devtools::load_all("c:/workspace/soapr")
devtools::load_all("c:/workspace/barrawana")

tf <- barrawana::TypeFactory$new()
print(body(tf$JobExportSettings))
