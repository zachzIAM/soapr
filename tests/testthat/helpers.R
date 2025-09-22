Person <- S7::new_class(
  name = "Person",
  properties = list(
    name = strict_string_prop("name", read_only = TRUE),
    nickname = strict_string_prop("nickname"),
    description = nullable_string_prop("description"),
    commentary = nullable_string_prop("commentary", read_only = TRUE)
  )
)
