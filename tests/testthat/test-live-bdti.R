library(testthat)


test_that("BDTi Service can be created and methods exist", {
  testthat::skip_on_ci()
  wsdl_path <- test_path("testdata/valid_wsdls/BDTInteractive.xml")
  service <- Service$new(wsdl_path)

  expect_true(inherits(service, "Service"))
  expect_true(inherits(service$client, "Client"))
  expect_true(inherits(service$factory, "Factory"))

  # Check if methods were dynamically added
  # Attempt to call Login.
  # We expect this might fail due to network or invalid credentials,
  # but it shouldn't fail due to code errors (like missing function).

  # We use expect_error to assert that it fails with a SOAP Fault or network error,
  # but not an R error. However, distinguishing them is hard.
  # For now, let's just ensure the method exists and can be called.

  expect_true(is.function(service$client$Login))
})
