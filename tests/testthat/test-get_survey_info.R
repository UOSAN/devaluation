test_that("Returns error if no credentials registered.", {
  mockery::stub(get_survey_info, 'Sys.getenv', "")
  expect_error(
    get_survey_info(),
    "You need to register your Qualtrics credentials using the register_qualtrics_credentials() function.",
    fixed = TRUE
  )
})

fake_survey_info <- tibble::tribble(
  ~id, ~name, ~ownerId, ~lastModified, ~creationDate, ~isActive,
  "xxx", "DEV Session 0 Surveys", "x", "x","x", "TRUE",
  "yyy", "DEV Session 1 Surveys", "y", "y", "y", "TRUE",
  "zzz", "Irrelevant Surveys", "z", "z", "z", "TRUE"
)

fake_data <- tibble::tribble(
  ~Login,   ~BSCS_1, ~BSCS_2, ~BSCS_3,
  "DEV001", 1, 1, 1,
  "DEV002", 1, 1, 1,
  "DEV003", 1, 1, 1
)

expected_output <- tibble::tribble(
  ~id, ~name, ~lastModified, ~n_obs,
  "xxx", "DEV Session 0 Surveys", "x", 3L,
  "yyy", "DEV Session 1 Surveys", "y", 3L
)

expected_output_nested <-
  expected_output %>%
  mutate(survey_data = list(fake_data, fake_data))

test_that("Returns tibble with expected structure", {
  mockery::stub(get_survey_info, 'Sys.getenv', "X")
  mockery::stub(get_survey_info, 'all_surveys', fake_survey_info)
  mockery::stub(get_survey_info, 'get_survey_responses_raw', fake_data)
  # raw data not appended
  expect_identical(
    get_survey_info(append_raw_data = FALSE),
    expected_output
  )
  # raw data appended
  expect_identical(
    get_survey_info(append_raw_data = TRUE),
    expected_output_nested
  )
})

