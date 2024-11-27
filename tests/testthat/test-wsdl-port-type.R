test_that("test parsing types from complete WSDLs", {

  files <- dir(file.path(testthat::test_path(), "testdata", "valid_wsdls"), full.names = TRUE)

  purrr::walk(files, ~{
    port_types <- parse_port_types(xml2::read_xml(.x))
    expect_type(port_types, "list")
    purrr::walk(port_types, ~{

      expect_s3_class(.x, "wsdl_port_type")
      expect_gt(length(.x$operations), 0)
      expect_false(is.null(names(.x$operations)))

      purrr::walk(.x$operations, ~expect_s3_class(.x, "wsdl_operation"))

    })
  })

})
