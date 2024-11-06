parse_documentation <- function(node) {
  # TODO we should achieve this via dispatch rather than checking for the class
  checkmate::assert_class(node, "xml_node")

  node_content <- xml2::xml_contents(node)
  node_content_names <- xml2::xml_name(node_content)
  doc_idx <- match("documentation", node_content_names)

  if (is.na(doc_idx)) {
    return(NULL)
  }

  doc_str <- xml2::xml_text(node_content[[doc_idx]])
  rv <- trimws(doc_str)

  return(rv)
}
