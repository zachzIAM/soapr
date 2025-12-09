test_that("Dynamic Client support for multiple ports", {
    # Use CountryInfoService as it has multiple bindings
    wsdl_path <- test_path("testdata/valid_wsdls/CountryInfoService.xml")

    # 1. Default Initialization (First Service, First Port)
    service <- Service$new(wsdl_path)
    expect_equal(service$service_name, "CountryInfoService")
    expect_equal(service$port_name, "CountryInfoServiceSoap")

    # Verify listing
    services <- service$list_services()
    expect_equal(services, "CountryInfoService")

    ports <- service$list_ports()
    # CountryInfoService has Soap and Soap12 ports usually, let's check parsing
    # It has binding="CountryInfoServiceSoap" and "CountryInfoServiceSoap12"
    # The ports are named same as binding in this WSDL or similar?
    # Let's inspect the file content earlier:
    # <wsdl:service name="CountryInfoService">
    #   <wsdl:port name="CountryInfoServiceSoap" binding="tns:CountryInfoServiceSoap">
    #   <wsdl:port name="CountryInfoServiceSoap12" binding="tns:CountryInfoServiceSoap12">
    expect_true("CountryInfoServiceSoap" %in% ports)
    expect_true("CountryInfoServiceSoap12" %in% ports)

    # 2. Initialize with specific port (Soap12)
    service12 <- Service$new(wsdl_path, port_name = "CountryInfoServiceSoap12")
    expect_equal(service12$port_name, "CountryInfoServiceSoap12")
    expect_equal(service12$client$port_def$name, "CountryInfoServiceSoap12")

    # Verify the binding name in the client is correct for Soap12
    expect_equal(
        service12$client$port_def$binding,
        "CountryInfoServiceSoapBinding12"
    )

    # 3. Initialize with invalid port
    expect_error(
        Service$new(wsdl_path, port_name = "InvalidPort"),
        "Port 'InvalidPort' not found"
    )

    # 4. Initialize with invalid service
    expect_error(
        Service$new(wsdl_path, service_name = "InvalidService"),
        "Service 'InvalidService' not found"
    )
})
