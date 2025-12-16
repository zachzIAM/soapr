# Inspect TypeFactory Members
devtools::load_all("c:/workspace/soapr")
devtools::load_all("c:/workspace/barrawana")

tf <- barrawana::TypeFactory$new()
print(ls(tf))
