#' Get survey information.
#'
#' A wrapper function for \code{\link[qualtRics]{all_surveys}}. This function returns metadata
#' for surveys administered at Devaluation sessions (0-5).
#'
#' @return A tibble including the following variables:
#' * id
#' * name
#' * ownerId
#' * lastModified
#' * creationDate
#' * isActive
#'
#' For more information, see \code{\link[qualtRics]{all_surveys}}
#' @export
get_survey_info <- function(){

  if (Sys.getenv("QUALTRICS_BASE_URL") == "" | Sys.getenv("QUALTRICS_API_KEY") == "") {
    stop("You need to register your qualtrics credentials using the register_qualtrics_credentials() function.")
    }

  # extract id of selected session's surveys
  surveys_df <- qualtRics::all_surveys() %>%
    filter(str_detect(.data$name, "DEV Session \\d Surveys")) %>%
    mutate(session_number = as.numeric(str_extract(.data$name, "\\d"))) %>%
    arrange(session_number) %>%
    select(-session_number)

  # return a tibble
  as_tibble(surveys_df)
}
