# list with extraneous rows
fake_data <- list(data = tibble::tribble(
  ~dev_id,   ~redcap_event_name,
  "DEV001", "session_0_arm_1",
  "DEV001", "session_1_arm_1",
  "DEV001", "t1_arm_1",
  "DEV001", "t8_arm_1"
  )
)

# tibble with filtered rows
fake_data2 <- tibble::tribble(
  ~dev_id,   ~redcap_event_name,
  "DEV001", "session_0_arm_1",
  "DEV001", "session_1_arm_1"
  )

test_that("Filters correct rows", {
  mockery::stub(get_redcap_raw, 'file.exists', TRUE)
  mockery::stub(get_redcap_raw, 'redcap_read_oneshot', fake_data)
  expect_identical(
    get_redcap_raw(path_to_creds = "fake_credentials.csv"),
    fake_data2
  )
})


