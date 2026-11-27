test_that("generated static package passes R CMD check cleanly", {
  skip_if_not_installed("rcmdcheck")

  fixture_wsdl <- test_path("testdata", "valid_wsdls", "CountryInfoService.xml")
  skip_if_not(file.exists(fixture_wsdl), "Local CountryInfoService.xml fixture not found")

  work_dir <- tempfile("soapr_gen_check_")
  check_out <- tempfile("soapr_gen_rcmdcheck_")
  dir.create(work_dir, recursive = TRUE)
  dir.create(check_out, recursive = TRUE)
  on.exit({
    unlink(work_dir, recursive = TRUE)
    unlink(check_out, recursive = TRUE)
  }, add = TRUE)

  parser <- WSDLParser$new(fixture_wsdl)
  gen <- Generator$new(parser)
  pkg_name <- "GeneratedParityCheck"
  gen$generate_package(
    path = work_dir,
    package_name = pkg_name,
    author = "Package Maintainer",
    email = "maintainer@example.com"
  )

  pkg_path <- file.path(work_dir, pkg_name)
  expect_true(dir.exists(pkg_path))
  expect_true(file.exists(file.path(pkg_path, "LICENSE")))
  expect_true(any(grepl("^License: MIT \\+ file LICENSE$", readLines(file.path(pkg_path, "DESCRIPTION")))))

  # Avoid inheriting R_TESTS from an outer R CMD check runner into the child check process
  old_r_tests <- Sys.getenv("R_TESTS", unset = NA_character_)
  if (!is.na(old_r_tests)) {
    Sys.unsetenv("R_TESTS")
    on.exit(Sys.setenv(R_TESTS = old_r_tests), add = TRUE)
  }

  res <- rcmdcheck::rcmdcheck(
    path = pkg_path,
    args = c("--no-manual"),
    check_dir = check_out,
    quiet = TRUE,
    error_on = "never"
  )

  expect_length(res$errors, 0L)
  expect_length(res$warnings, 0L)
  expect_length(res$notes, 0L)
})
