#' Raw survey responses
#'
#' Get raw responses from Qualtrics surveys for a given DEV session
#'
#' @param session DEV session number (0-5)
#'
#' @return A tibble containing raw survey responses for the specified session.
#' @export
#'
get_survey_responses_raw <- function(session){

  if (!session %in% 0:5) {
    stop("You specified an invalid session number. Devaluation sessions range from 0 to 5.")
  }

  if (Sys.getenv("QUALTRICS_BASE_URL") == "" | Sys.getenv("QUALTRICS_API_KEY") == "") {
    stop("You need to register your Qualtrics credentials using the register_qualtrics_credentials() function.")
  }

  # extract id of selected session's surveys
  survey_id <- all_surveys() %>%
    filter(str_detect(.data$name, "DEV Session \\d Surveys")) %>%
    mutate(session_number = as.numeric(str_extract(.data$name, "\\d"))) %>%
    filter(session == .data$session_number) %>%
    pull(.data$id)

  # extract selected session's surveys
  session_surveys <- fetch_survey(survey_id,
                                  label = FALSE, # numerical values instead of Choice Text
                                  convert = FALSE, # don't automatically convert multiple choice q's, etc.
                                  force_request = TRUE) # don't create tmp directory, always retrieve from API

  # return a tibble
  as_tibble(session_surveys)
}
