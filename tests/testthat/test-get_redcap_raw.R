# list with extraneous rows
fake_data <- list(data = tibble::tribble(
  ~dev_id,   ~redcap_event_name, ~first_name, ~last_name, ~email, ~phone, ~address, ~friend,
  "DEV001", "session_0_arm_1", "a", "b", "c", "d", "e", "f",
  "DEV001", "session_1_arm_1", "a", "b", "c", "d", "e", "f",
  "DEV001", "t1_arm_1", "a", "b", "c", "d", "e", "f",
  "DEV001", "t8_arm_1", "a", "b", "c", "d", "e", "f"
  )
)

# tibble with filtered rows
fake_data2 <- tibble::tribble(
  ~dev_id,   ~redcap_event_name,
  "DEV001", "session_0_arm_1",
  "DEV001", "session_1_arm_1"
)

# tibble with filtered rows and identifiable vars
fake_data3 <- tibble::tribble(
  ~dev_id,   ~redcap_event_name, ~first_name, ~last_name, ~email, ~phone, ~address, ~friend,
  "DEV001", "session_0_arm_1", "a", "b", "c", "d", "e", "f",
  "DEV001", "session_1_arm_1", "a", "b", "c", "d", "e", "f"
)


test_that("Filters correct rows", {
  mockery::stub(get_redcap_raw, 'file.exists', TRUE)
  mockery::stub(get_redcap_raw, 'redcap_read_oneshot', fake_data)
  expect_identical(
    get_redcap_raw(path_to_creds = "fake_credentials.csv"),
    fake_data2
  )
})

test_that("Deselects identifiable vars", {
  mockery::stub(get_redcap_raw, 'file.exists', TRUE)
  mockery::stub(get_redcap_raw, 'redcap_read_oneshot', fake_data)
  expect_identical(
    get_redcap_raw(path_to_creds = "fake_credentials.csv", identifiable = TRUE),
    fake_data3
  )
})

