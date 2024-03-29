test_that("Returns proper errors.", {
  expect_error(
    register_qualtrics_credentials(path_to_creds = ""),
    "A credentials file does not exist at that path.",
    fixed = TRUE
  )

  # stub out the function to avoid including real API credentials
  mockery::stub(register_qualtrics_credentials, 'file.exists', TRUE)
  mockery::stub(register_qualtrics_credentials, 'read_csv', tibble::tibble(invalid_key = "key"))
  expect_error(
    register_qualtrics_credentials(path_to_creds = ""),
    "Your credentials file must have the following columns: data_source, base_url, api_token"
  )
})

test_that("Returns success message", {
  expect_message(
    register_qualtrics_credentials(path_to_creds = "fake_credentials.csv"),
    "To install your credentials for use in future sessions, run this function with `install = TRUE`.",
    fixed = TRUE
  )
})
