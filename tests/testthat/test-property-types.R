# --- strict_string_prop ---

test_that("strict_string_prop accepts valid strings", {
  p <- Person(name = "Alice", nickname = "Al", description = NULL)
  expect_equal(p@name, "Alice")
  expect_equal(p@nickname, "Al")
})

test_that("strict_string_prop rejects NULL, NA, empty string", {
  expect_error(Person(name = NULL, nickname = "Al"), "Must be of type 'string'")
  expect_error(Person(name = NA_character_, nickname = "Al"), "May not be NA")
  expect_error(Person(name = "", nickname = "Al"), "Cannot be an empty string")

  expect_error(Person(name = "Alice", nickname = NULL), "Must be of type 'string'")
  expect_error(Person(name = "Alice", nickname = NA_character_), "May not be NA")
  expect_error(Person(name = "Alice", nickname = ""), "Cannot be an empty string")
})

test_that("strict_string_prop respects read_only flag", {
  p <- Person(name = "Alice", nickname = "Al")
  expect_error(p@name <- "Bob", "@name is read-only")
  p@nickname <- "Bobby"
  expect_equal(p@nickname, "Bobby")
})

# --- nullable_string_prop ---

test_that("nullable_string_prop accepts NULL and valid strings", {
  p <- Person(name = "Alice", nickname = 'Al', description = NULL)
  expect_null(p@description)

  p@description <- "Something"
  expect_equal(p@description, "Something")
})

test_that("nullable_string_prop rejects empty string and NA", {
  expect_error(Person(name = "Alice", nickname = "Al", description = ""), "Cannot be an empty string")
  expect_error(Person(name = "Alice", nickname = "Al", description = NA_character_), "May not be NA")
})

test_that("nullable_string_prop respects read_only flag", {
  p <- Person(name = "Alice", nickname = "Al", commentary = "note")
  expect_equal(p@commentary, "note")
  expect_error(p@commentary <- "updated", "@commentary is read-only")
})

# --- General robustness ---

test_that("validators handle multiple bad types", {
  expect_error(Person(name = 123L, nickname = "Al"), "Must be of type 'string'")
  expect_error(Person(name = TRUE, nickname = "Al"), "Must be of type 'string'")
  expect_error(Person(name = letters, nickname = "Al"), "Must have length 1")
  expect_error(Person(name = list("hi"), nickname = "Al"), "Must be of type 'string'")
  expect_error(Person(name = data.frame(x = 1:10), nickname = "Al"), "Must be of type 'string'")

  expect_error(Person(nickname = 123L, name = "Al"), "Must be of type 'string'")
  expect_error(Person(nickname = TRUE, name = "Al"), "Must be of type 'string'")
  expect_error(Person(nickname = letters, name = "Al"), "Must have length 1")
  expect_error(Person(nickname = list("hi"), name = "Al"), "Must be of type 'string'")
  expect_error(Person(nickname = data.frame(x = 1:10), name = "Al"), "Must be of type 'string'")
})
