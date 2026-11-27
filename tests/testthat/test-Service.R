test_that("Service lists services and ports as type-safe character vectors", {
  wsdl_path <- test_path("testdata/valid_wsdls/MultiService.xml")
  svc <- Service$new(wsdl_path)

  services <- svc$list_services()
  expect_identical(services, c("ServiceA", "ServiceB"))
  expect_true(is.character(services))
  expect_null(names(services))

  ports_a <- svc$list_ports()
  expect_identical(ports_a, "PortA")
  expect_true(is.character(ports_a))
  expect_null(names(ports_a))

  svc_b <- Service$new(wsdl_path, service_name = "ServiceB", port_name = "PortB")
  expect_identical(svc_b$list_ports(), "PortB")

  # Empty services / unmatched service should return character(0), never list()
  svc$wsdl <- list(get_services = function() list())
  expect_identical(svc$list_services(), character(0))
  expect_identical(svc$list_ports(), character(0))

  svc$wsdl <- list(get_services = function() list(list(name = "ServiceA", ports = list())))
  expect_identical(svc$list_ports(), character(0))
})
