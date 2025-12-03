test_that("WSDLParser parses BDTInteractive.xml correctly", {
  wsdl_path <- test_path("testdata/valid_wsdls/BDTInteractive.xml")
  wsdl <- WSDLParser$new(wsdl_path)

  expect_s3_class(wsdl, "WSDLParser")

  # Test target namespace
  expect_equal(wsdl$get_target_namespace(), "http://barra.com/cp/bdti")

  # Test services
  services <- wsdl$get_services()
  expect_length(services, 1)
  expect_equal(services[[1]]$name, "BDTInteractive")

  # Test ports
  ports <- services[[1]]$ports
  expect_length(ports, 1)
  expect_equal(ports[[1]]$name, "BDTIWebServicePort")
  expect_equal(ports[[1]]$binding, "BDTIWebServiceBinding")
  expect_match(ports[[1]]$location, "^https://")

  # Test operations
  ops <- wsdl$get_operations("BDTIWebServiceBinding")
  expect_gt(length(ops), 0)
  expect_true("Login" %in% names(ops))
  expect_equal(ops[["Login"]]$action, "")
  expect_equal(ops[["Login"]]$input_message, "LoginInputMessage")
  expect_equal(ops[["Login"]]$output_message, "LoginOutputMessage")

  # Test types
  types <- wsdl$get_type_definitions()
  expect_gt(length(types), 0)
  expect_true("Login" %in% names(types))
  expect_true("LoginResponse" %in% names(types))

  # Check Login type structure
  login_type <- types[["Login"]]
  expect_equal(login_type$kind, "complex")
  expect_true("User" %in% names(login_type$elements))
  expect_true("Password" %in% names(login_type$elements))
})
