test_that("Static Client Generator support for multiple ports", {
  wsdl_path <- test_path("testdata/valid_wsdls/CountryInfoService.xml")

  # Generate package
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  generator <- Generator$new(WSDLParser$new(wsdl_path))
  generator$generate_package(temp_dir, "CountryInfoPkg")

  pkg_path <- file.path(temp_dir, "CountryInfoPkg")
  expect_true(dir.exists(pkg_path))

  # Load generated package
  pkgload::load_all(pkg_path, quiet = TRUE)
  on.exit(pkgload::unload("CountryInfoPkg"), add = TRUE)

  mock_transport <- function(url, action, body, config, handle) {
    response_xml <- paste0(
      '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">',
      "<soap:Body>",
      '<ListOfContinentsByNameResponse xmlns="http://www.oorsprong.org/websamples.countryinfo">',
      "<ListOfContinentsByNameResult>",
      "<tContinent><sCode>AF</sCode><sName>Africa</sName></tContinent>",
      "</ListOfContinentsByNameResult>",
      "</ListOfContinentsByNameResponse>",
      "</soap:Body>",
      "</soap:Envelope>"
    )
    structure(
      list(
        url = url,
        status_code = 200,
        headers = list(`Content-Type` = "text/xml; charset=utf-8"),
        content = charToRaw(response_xml)
      ),
      class = "response"
    )
  }

  # 1. Default Initialization
  client <- CountryInfoPkg::new_client(transport = mock_transport)
  expect_equal(client$service_name, "CountryInfoService")
  expect_equal(client$port_name, "CountryInfoServiceSoap")

  # 2. Specific Port (Soap12)
  client12 <- CountryInfoPkg::new_client(
    port = "CountryInfoServiceSoap12",
    transport = mock_transport
  )
  expect_equal(client12$port_name, "CountryInfoServiceSoap12")
  expect_equal(client12$port_def$binding, "CountryInfoServiceSoapBinding12")

  # 3. Invalid Port
  expect_error(
    CountryInfoPkg::new_client(port = "InvalidPort"),
    "Port not found"
  )

  # 4. Check Operation Support (both ports support same ops so this is harder to test failure on)
  # But we can check that it doesn't error on valid op
  res <- client$ListOfContinentsByName()
  expect_equal(res$ListOfContinentsByNameResult$tContinent[[1]]$sCode, "AF")
  expect_error(client12$ListOfContinentsByName(), NA)

  # If we had a WSDL with disjoint operations, we could test "Operation not supported"
  # But verifying the port selection logic is sufficient for now.
})
