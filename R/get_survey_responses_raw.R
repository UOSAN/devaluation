#' Get raw responses for a set of Qualtrics surveys for a given session.
#'
#' @param session Session number (0-5)
#' @param path_to_creds (Full path to credentials file containing API token and base URL.
#' The credentials file must have the following columns: data_source, base_url, api_token.
#'
#'
#' @return A tibble containing raw qualtrics responses for the specified session
#' @export
#'
get_survey_responses_raw <- function(session, path_to_creds){

  if (!session %in% 0:5) {
    stop("You specified an invalid session number. Devaluation sessions range from 0 to 5.")
  }

  if (!file.exists(path_to_creds)) {
    stop("A credentials file does not exist at that path.")
  }

  # read in credentials
  credentials <- read_csv(path_to_creds)

  if (!identical(sort(c("data_source", "base_url", "api_token")), sort(names(credentials)))) {
    stop("Your credentials file must have the following columns: data_source, base_url, api_token")
  }

  # extract API token
  qualtrics_token <- credentials %>%
    filter(.data$data_source == "qualtrics") %>%
    pull(.data$api_token)

  # extract baseURL
  base_url <- credentials %>%
    filter(.data$data_source == "qualtrics") %>%
    pull(base_url)

  # register credentials
  qualtRics::qualtrics_api_credentials(
    api_key = qualtrics_token,
    base_url = base_url,
    install = FALSE)

  # extract id of selected session's surveys
  session_id <- qualtRics::all_surveys() %>%
    filter(str_detect(.data$name, "DEV Session \\d Surveys")) %>%
    mutate(session_number = as.numeric(str_extract(.data$name, "\\d"))) %>%
    filter(session == .data$session_number) %>%
    pull(.data$id)

  # extract selected session's surveys
  session_surveys <- qualtRics::fetch_survey(session_id)

  # return a tibble
  as_tibble(session_surveys)
}
