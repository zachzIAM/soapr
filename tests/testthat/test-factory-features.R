library(testthat)
library(soapr)

test_that("Factory enhancements: Initialization", {
    wsdl_path <- test_path("testdata/valid_wsdls/BDTInteractive.xml")
    service <- Service$new(wsdl_path)
    factory <- service$factory

    # 1. Initialization with values
    # Portfolio has attributes: Type, PortfolioCurrency, etc.
    obj <- factory$create("Portfolio", Type = "HOLDINGS", PortfolioCurrency = "USD")

    expect_equal(obj$Type, "HOLDINGS")
    expect_equal(obj$PortfolioCurrency, "USD")
    # Check metadata is preserved
    expect_true(inherits(obj, "soap_object"))
    expect_equal(attr(obj, "soap_type"), "Portfolio")

    # 2. Defaults
    # CustomizeReportSetting has attributes with defaults, e.g., InclBenchmarkNotHeld default="false"
    # Note: The current parser might store defaults. We need to check if Factory uses them.
    crs <- factory$create("CustomizeReportSetting")

    # If implemented, this should be "false" or FALSE (depending on type conv), not NA
    # The WSDL says: <xsd:attribute name="InclBenchmarkNotHeld" type="xsd:boolean" use="optional" default="false"/>
    # So we expect FALSE or "false".
    expect_false(is.na(crs$InclBenchmarkNotHeld))
    # expect_equal(crs$InclBenchmarkNotHeld, FALSE) # logical conversion desirable
})

test_that("Factory enhancements: Simple Types", {
    wsdl_path <- test_path("testdata/valid_wsdls/BDTInteractive.xml")
    service <- Service$new(wsdl_path)
    factory <- service$factory

    # xsd:string is likely not in factory$types because WSDLParser might only expose defined types?
    # Or maybe WSDLParser includes schemas.
    # Let's check a simple type defined in WSDL: BaseValueType is a restriction of string.

    # <xsd:simpleType name="BaseValueType"> <xsd:restriction base="xsd:string"> ...

    # For a simpleType (Enum), create should probably return NA or allowed value?
    val <- factory$create("BaseValueType")
    # BaseValueType is simpleType based on string. Should return NA_character_.
    expect_true(is.na(val))
    expect_true(is.character(val))

    # Standard types
    val_int <- factory$create("xsd:int")
    expect_true(is.na(val_int))
    expect_true(is.integer(val_int))

    val_str <- factory$create("xsd:string")
    expect_true(is.na(val_str))
    expect_true(is.character(val_str))
})

test_that("Factory enhancements: List Conversion", {
    wsdl_path <- test_path("testdata/valid_wsdls/BDTInteractive.xml")
    service <- Service$new(wsdl_path)
    factory <- service$factory

    data <- list(Type = "HOLDINGS", PortfolioCurrency = "USD")

    # Cast to Portfolio
    obj <- factory$from_list(data, "Portfolio")

    expect_s3_class(obj, "soap_object")
    expect_equal(attr(obj, "soap_type"), "Portfolio")
    expect_false(is.null(attr(obj, "soap_attrs")))
    expect_equal(obj$Type, "HOLDINGS")
})
