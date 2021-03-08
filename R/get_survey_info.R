#' Get Qualtrics survey info
#'
#' Get information about all DEV Qualtrics surveys, including number of
#' observations in raw data and date of most recent response
#'
#' @param append_raw_data A logical value indicating whether raw survey data should
#' be included as a column in the output
#'
#' @return A tibble with the following columns:
#' * id (unique survey identifier)
#' * name (survey name)
#' * lastModified (date of most recent response)
#' * n_obs (number of observations in raw survey data)
#'
#' If \code{append_raw_data} is set to \code{TRUE}, output includes an additional
#' list column, survey_data.
#' @export
get_survey_info <- function(append_raw_data = FALSE){

  if (Sys.getenv("QUALTRICS_BASE_URL") == "" | Sys.getenv("QUALTRICS_API_KEY") == "") {
    stop("You need to register your Qualtrics credentials using the register_qualtrics_credentials() function.")
  }

  # get survey metadata
  surveys_df <- all_surveys() %>%
    filter(str_detect(.data$name, "DEV Session \\d Surveys")) %>%
    mutate(session_number = as.numeric(str_extract(.data$name, "\\d"))) %>%
    arrange(.data$session_number)

  # get nested data frame with survey responses in list column
  surveys_df_nested <- surveys_df %>%
    rowwise() %>%
    mutate(survey_data = list(get_survey_responses_raw(session = .data$session_number))) %>%
    mutate(n_obs = nrow(.data$survey_data)) %>%
    ungroup() %>%
    select(.data$id, .data$name, .data$lastModified, .data$n_obs, .data$survey_data)

  if (append_raw_data) {
    return(surveys_df_nested)
    } else {
    return(surveys_df_nested %>% select(-.data$survey_data))
  }
}
