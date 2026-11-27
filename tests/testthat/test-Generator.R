test_that("Generator parameterizes author, email, and license_holder with sensible defaults", {
  wsdl_path <- test_path("testdata/valid_wsdls/MultiService.xml")
  temp_dir <- tempfile("gen_meta")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  gen <- Generator$new(WSDLParser$new(wsdl_path))

  # 1. Default metadata
  gen$generate_package(temp_dir, "DefaultMetaPkg")
  default_desc <- readLines(file.path(temp_dir, "DefaultMetaPkg", "DESCRIPTION"))
  default_lic <- readLines(file.path(temp_dir, "DefaultMetaPkg", "LICENSE"))

  expect_true(any(grepl(
    'Authors@R: person\\("Package Author", email = "author@example.com", role = c\\("aut", "cre"\\)\\)',
    default_desc
  )))
  expect_true(any(grepl("COPYRIGHT HOLDER: DefaultMetaPkg authors", default_lic)))

  # 2. Custom metadata
  gen$generate_package(
    temp_dir,
    "CustomMetaPkg",
    author = "Jane Doe",
    email = "jane.doe@example.com",
    license_holder = "Acme Corp"
  )
  custom_desc <- readLines(file.path(temp_dir, "CustomMetaPkg", "DESCRIPTION"))
  custom_lic <- readLines(file.path(temp_dir, "CustomMetaPkg", "LICENSE"))

  expect_true(any(grepl(
    'Authors@R: person\\("Jane Doe", email = "jane.doe@example.com", role = c\\("aut", "cre"\\)\\)',
    custom_desc
  )))
  expect_true(any(grepl("COPYRIGHT HOLDER: Acme Corp", custom_lic)))

  # 3. Fallback handling for NULL, NA, empty, and non-scalar metadata values
  gen$generate_package(
    temp_dir,
    "FallbackMetaPkg",
    author = NA_character_,
    email = "",
    license_holder = NULL
  )
  fallback_desc <- readLines(file.path(temp_dir, "FallbackMetaPkg", "DESCRIPTION"))
  fallback_lic <- readLines(file.path(temp_dir, "FallbackMetaPkg", "LICENSE"))

  expect_true(any(grepl(
    'Authors@R: person\\("Package Author", email = "author@example.com", role = c\\("aut", "cre"\\)\\)',
    fallback_desc
  )))
  expect_true(any(grepl("COPYRIGHT HOLDER: FallbackMetaPkg authors", fallback_lic)))

  gen$write_description(
    file.path(temp_dir, "FallbackMetaPkg"),
    "FallbackMetaPkg",
    author = c("A", "B"),
    email = character(0)
  )
  multi_desc <- readLines(file.path(temp_dir, "FallbackMetaPkg", "DESCRIPTION"))
  expect_true(any(grepl(
    'Authors@R: person\\("Package Author", email = "author@example.com", role = c\\("aut", "cre"\\)\\)',
    multi_desc
  )))
})
