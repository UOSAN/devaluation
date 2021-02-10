test_that("Returns proper errors.", {
  expect_error(get_survey_responses_raw(session = 6), "You specified an invalid session number. Devaluation sessions range from 0 to 5.")
  expect_error(get_survey_responses_raw(session = 0, path_to_creds = ""), "A credentials file does not exist at that path.")
})

## How can I write unit tests that will access credentials from external file?
# test_that("Output is a tibble", {
#   cred_path <- "../credentials.csv"
#   s0_data <- get_survey_responses_raw(session = 0, path_to_creds = cred_path)
#   expect_true(tibble::is_tibble(s0_data))
# })


test_that('Credential file errors', {
  mockery::stub(get_survey_responses_raw, 'file.exists', TRUE)
  mockery::stub(get_survey_responses_raw, 'read_csv', tibble::tibble(invalid_key = "key"))
  expect_error(get_survey_responses_raw(session = 0, path_to_creds = ""),
               "Your credentials file must have the following columns: data_source, base_url, api_token")
})
