skip_if_not_live <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_not(
    identical(Sys.getenv("SOAPR_RUN_LIVE_TESTS"), "true"),
    "Live tests disabled (set SOAPR_RUN_LIVE_TESTS=true to run)"
  )
  testthat::skip_if_offline()
}
