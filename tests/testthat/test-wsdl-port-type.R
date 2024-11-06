# doc <- xml2::read_xml('<portType name="OrderProcessing">
#     <operation name="PlaceOrder">
#         <documentation>
#             This operation places an order for a specified product.
#         </documentation>
#
#         <input message="tns:PlaceOrderRequest">
#             <documentation>
#                 Contains order details like product ID, quantity, and customer info.
#             </documentation>
#         </input>
#
#         <output message="tns:PlaceOrderResponse">
#             <documentation>
#                 Returns confirmation of the placed order with a unique order ID.
#             </documentation>
#         </output>
#
#         <fault name="OrderFault" message="tns:OrderError">
#             <documentation>
#                 Indicates an error if the order could not be processed due to invalid details.
#             </documentation>
#         </fault>
#
#         <fault name="InvalidOrderFault" message="tns:InvalidOrderError"/>
#
#         <fault name="PaymentFault" message="tns:PaymentError"/>
#
#     </operation>
# </portType>
# ')

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
