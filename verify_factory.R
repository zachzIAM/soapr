devtools::load_all()

wsdl_path <- "tests/testthat/testdata/valid_wsdls/BDTInteractive.xml"
service <- Service$new(wsdl_path)

print("Creating Portfolio object...")
portfolio <- service$factory$create("Portfolio")

print("Portfolio structure:")
print(str(portfolio))

print("Portfolio names:")
print(names(portfolio))
