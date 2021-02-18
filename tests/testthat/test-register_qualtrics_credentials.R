test_that("Returns proper errors.", {
  expect_error(get_survey_responses_raw(session = 0, path_to_creds = ""), "A credentials file does not exist at that path.")

  # stub out the function to avoid including real API credentials
  mockery::stub(get_survey_responses_raw, 'file.exists', TRUE)
  mockery::stub(get_survey_responses_raw, 'read_csv', tibble::tibble(invalid_key = "key"))
  expect_error(get_survey_responses_raw(session = 0, path_to_creds = ""),
               "Your credentials file must have the following columns: data_source, base_url, api_token")
})
