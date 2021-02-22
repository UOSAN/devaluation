#' Get survey data
#'
#' @param surveys_df A tibble as returned by \code{\link{get_survey_info}}.
#' @param pid_col A character with the column name specifying user ID or a regular expression
#' identifying the column(s) that you'd like to keep in wide format. Defaults to a regular expression
#' that selects columns that are useful for determining valid (e.g. non-duplicate) responses:
#' * "Login" (participant ID)
#' * "ResponseId" (alphanumeric sequence that uniquely identifies a set of survey
#' responses for an individual)
#' * "Finished" (indicates whether the survey was completed or not)
#'
#' @return Returns a long format tibble of survey data with columns identified by \code{pid_col}
#' along with columns labeled "item", "value", and "survey_name"
#' @export
get_survey_data <- function(surveys_df, pid_col = '(Login|ResponseId|Finished)'){

  # get nested data frame with survey responses in list column
  survey_data <- surveys_df %>%
    select(survey_id = id, survey_name = name) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(survey_data = list(get_survey_responses(survey_id = survey_id)))

  # wrangle into long format
  # BC: Struggling to refactor this...
  long_survey_data <- survey_data %>%
    dplyr::filter(dim(survey_data)[1]>0) %>%
    dplyr::do({
      gather_cols <- names(.$survey_data)[!grepl(pid_col, names(.$survey_data))]
      aDF <- tidyr::gather(.$survey_data,
                           item,
                           value,
                           all_of(gather_cols))
      aDF$survey_name <- .$survey_name
      aDF
    })

  return(long_survey_data)
}


#' Return a tibble of the survey responses for a given survey
#'
#' See \code{\link[qualtRics]{fetch_survey}} for complete information on available options.
#'
#' @param survey_id Unique survey ID as returned from \code{\link{get_survey_info}}.
#' @param label default is \code{FALSE}.
#' @param convert default is \code{FALSE}.
#' @param force_request default is \code{TRUE}.
get_survey_responses<-function(survey_id, label = FALSE, convert = FALSE, force_request = TRUE) {
  responses_df <- qualtRics::fetch_survey(survey_id,
                                         label = label,
                                         convert = convert,
                                         force_request = force_request)
  return(responses_df)
}
