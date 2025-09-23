# wsdl_service <- S7::new_class(
#   name = "wsdl_service",
#   properties = list(
#     ports = S7::new_property(
#       class = S7::class_list,
#       validator = function(value) {
#         if (length(value) == 0 || is.null(value)) return(NULL)
#         bad <- !purrr::map_lgl(value, ~S7::S7_inherits(.x, class = wsdl_port_type))
#         if (any(bad)) {
#           return("all elements of @ports must be <wsdl_port_type> objects")
#         }
#       }
#     ),
#     names = S7::class_character,
#     is_resolved = S7::class_logical
#   ),
#   constructor = function(node) {
#
#     stop("Not implemented")
#
#     rv <- S7::new_object(
#       S7::S7_object(),
#       ports = list(),
#       names = "test",
#       is_resolved = FALSE
#     )
#
#   }
# )
