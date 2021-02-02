#' Get raw responses for a set of Qualtrics surveys for a given session.
#'
#' @param session session number (0-5)
#' @param path_to_creds (file path to credentials file containing API token and base URL)
#'
#' @return A tibble containing raw qualtrics responses for the specified session
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
get_survey_responses_raw <- function(session, path_to_creds){

  if (!session %in% 0:5) {
    stop("You specified an invalid session number. Devaluation sessions range from 0 to 5.")
  }

  # read in credentials
  credentials <- readr::read_csv(path_to_creds)

  # extract API token
  qualtrics_token <- credentials %>%
    dplyr::filter(data_source == "qualtrics") %>%
    dplyr::pull(api_token)

  # extract baseURL
  base_url <- credentials %>%
    dplyr::filter(data_source == "qualtrics") %>%
    dplyr::pull(base_url)

  # register credentials
  qualtRics::qualtrics_api_credentials(
    api_key = qualtrics_token,
    base_url = base_url,
    install = FALSE)

  # extract id of selected session's surveys
  session_id <- qualtRics::all_surveys() %>%
    dplyr::filter(stringr::str_detect(name, "DEV Session \\d Surveys")) %>%
    dplyr::mutate(session_number = as.numeric(stringr::str_extract(name, "\\d"))) %>%
    dplyr::filter(session == session_number) %>%
    dplyr::pull(id)

  # extract selected session's surveys
  session_surveys <- qualtRics::fetch_survey(session_id)

  # return a tibble
  tibble::as_tibble(session_surveys)
}
