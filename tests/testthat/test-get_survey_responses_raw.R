test_that("Returns error for invalid session ID.", {
  expect_error(get_survey_responses_raw(session = 6), "You specified an invalid session number. Devaluation sessions range from 0 to 5.")
})

test_that("Returns error if no credentials registered.", {
  mockery::stub(get_survey_responses_raw, 'Sys.getenv', "")
  expect_error(
    get_survey_responses_raw(session = 0),
    "You need to register your Qualtrics credentials using the register_qualtrics_credentials() function.",
    fixed = TRUE
  )
})

