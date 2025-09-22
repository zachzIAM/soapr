frozen_setter <- function(name) {
  function(self, value) {
    if (length(value) == 0) return(self)
    if (!is.null(S7::prop(self, name))) {
      stop(sprintf("@%s is read-only", name), call. = FALSE)
    }
    S7::prop(self, name) <- value
    return(self)
  }
}


nullable_string_prop <- function(prop_name, read_only = FALSE) {
  setter <- if (read_only) frozen_setter(prop_name)

  rv <- S7::new_property(
    class = S7::class_any,
    name = prop_name,
    validator = function(value) {
      check <- checkmate::check_string(value, min.chars = 1, na.ok = FALSE, null.ok = TRUE)
      if (grepl("at least.*characters", check)) check <- "Cannot be an empty string."
      if (!checkmate::test_true(check)) return(check)
    },
    setter = setter
  )

  return(rv)
}


strict_string_prop <- function(prop_name, read_only = FALSE) {
  setter <- if (read_only) frozen_setter(prop_name)

  rv <- S7::new_property(
    class = S7::class_any,
    name = prop_name,
    validator = function(value) {
      check <- checkmate::check_string(value, min.chars = 1, na.ok = FALSE, null.ok = FALSE)
      if (grepl("at least.*characters", check)) check <- "Cannot be an empty string."
      if (!checkmate::test_true(check)) return(check)
    },
    setter = setter
  )

  return(rv)
}
