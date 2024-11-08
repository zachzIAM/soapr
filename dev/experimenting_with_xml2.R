x <- xml2::read_xml("./tests/testthat/testdata/valid_wsdls/CalibayesServices1.wsdl")
x_ns <- xml2::xml_ns(x)
content <- xml2::xml_contents(x)
portType <- content[[17]]
ops <- xml2::xml_contents(portType)
op1 <- ops[[1]] |> xml2::xml_contents()
op1_fault <- op1[[3]]

op1_fault


x_lst <- xml2::as_list(x)
x_lst$definitions[purrr::map_lgl()]
messages <- x_lst$definitions[names(x_lst$definitions) == "message"]
messages[purrr::map_lgl(messages, ~attr(.x, "name") == "MyException")]

xml2::xml_find_all(x, ".//message", ns = NULL)

xml2::xml_ns_strip(x)
xml2::xml_find_all(x, ".//message")
xml2::xml_find_all(x, '/definitions/message[@name="MyException"]')
xml2::xml_find_all(x, '/definitions/message/part[@element="tns:MyException"]')
xml2::xml_find_all(x, '//@element="tns:MyException"')


x <- xml2::read_xml("./tests/testthat/testdata/valid_wsdls/CalibayesServices1.wsdl")
x_ns <- xml2::xml_ns(x)
new_ns <- xml2::xml_ns_rename(x_ns, d1 = "wsdl")
xml2::xml_find_all(x, ".//wsdl:message", new_ns)

x <- xml2::read_xml("./tests/testthat/testdata/valid_wsdls/CalibayesServices1.wsdl")
x_ns1 <- xml2::xml_ns(x)
xml2::xml_ns_strip(x)
x_ns2 <- xml2::xml_ns(x)

