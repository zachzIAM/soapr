# Inspect Signature
devtools::load_all("c:/workspace/soapr")
devtools::load_all("c:/workspace/barrawana")

tf <- barrawana::TypeFactory$new()
print(args(tf$CurrentSettings))
