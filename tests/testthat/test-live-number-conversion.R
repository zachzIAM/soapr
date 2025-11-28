library(testthat)
testthat::skip_on_ci()

test_that("NumberConversion service works", {
  wsdl_url <- "http://www.dataaccess.com/webservicesserver/NumberConversion.wso?WSDL"

  service <- Service$new(wsdl_url, verbose = FALSE)
  client <- service$client

  # NumberToWords
  # Input: ubiNum (unsignedLong)
  res_words <- client$NumberToWords(ubiNum = 123)
  expect_true(is.character(res_words$NumberToWordsResult))
  expect_match(res_words$NumberToWordsResult, "hundred", ignore.case = TRUE)

  # NumberToDollars
  # Input: dNum (decimal)
  res_dollars <- client$NumberToDollars(dNum = 123.45)
  expect_true(is.character(res_dollars$NumberToDollarsResult))
  expect_match(
    res_dollars$NumberToDollarsResult,
    "dollars.*cents",
    ignore.case = TRUE
  )
})
