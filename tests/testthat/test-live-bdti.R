library(testthat)

test_that("BDTi Service can be created and methods exist", {
  skip_if_not_live()

  service <- Service$new("https://www.barraone.com/bdti/BDTInteractive?wsdl")

  expect_true(inherits(service, "Service"))
  expect_true(inherits(service$client, "Client"))
  expect_true(inherits(service$factory, "Factory"))

  expect_true(is.function(service$client$Login))

  user <- "DUMMY_USER"
  password <- "DUMMY_PASSWORD"
  client_id <- "DUMMY_CLIENT_ID"

  # Test loogging in expecting specific errors
  expect_error(
    service$client$Login(),
    regexp = "(?i)SOAP Fault.*either.*must"
  )
  expect_error(
    service$client$Login(User = user, Client = client_id, Password = password),
    regexp = "(?i)SOAP Fault.*invalid.*credential"
  )

})
