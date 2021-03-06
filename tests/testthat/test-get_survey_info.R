test_that("Returns error if no credentials registered.", {
  mockery::stub(get_survey_info, 'Sys.getenv', "")
  expect_error(
    get_survey_info(),
    "You need to register your Qualtrics credentials using the register_qualtrics_credentials() function.",
    fixed = TRUE
  )
})
