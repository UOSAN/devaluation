#' Get survey data
#'
#' @param surveys_df A tibble as returned by the \code{\link{get_survey_info}}.
#' @param pid_col A character with the column name specifying user ID.
#'
#' @return Returns a long format tibble of survey data with names "SID variable name", "item", "value", "survey_name"
#' @export
get_survey_data <- function(surveys_df, pid_col='Login'){

  survey_data <- surveys_df %>%
    dplyr::group_by(name) %>%
    dplyr::do(
      survey_name = .$name[[1]],
      survey_data = get_survey_responses(
        survey_id = .$id[[1]])
    )

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
#' @export
get_survey_responses<-function(survey_id, label = FALSE, convert = FALSE, force_request = TRUE) {
  responsesDF <- qualtRics::fetch_survey(survey_id,
                                         label = label,
                                         convert = convert,
                                         force_request = force_request)
  return(responsesDF)
}
