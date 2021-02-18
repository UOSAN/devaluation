#' Get survey data
#'
#' @param surveysDF A data.frame as returned by the \code{\link{get_surveys}}.
#' @param pid_col A character with the column name specifying user ID.
#'
#' @return Returns a long format data.frame of survey data with names "SID variable name", "item", "value", "survey_name"
#' @import dplyr
#' @import tidyr
#' @export
get_survey_data<-function(surveysDF, pid_col='ID'){
  requireNamespace('dplyr', quietly = TRUE)
  requireNamespace('tidyr', quietly = TRUE)

  survey_data <- surveysDF %>%
    dplyr::group_by(id) %>%
    dplyr::do(
      survey_name = .$name[[1]],
      survey_data = get_survey_responses(
        surveyID = .$id[[1]])
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

#' Return a list of all surveys available to the user
#'
#' @import qualtRics
#' @export
get_surveys <- function() {
  surveysDF <- qualtRics::all_surveys()
  #for redundancy with scripts written with previous versions of this package
  if('name' %in% names(surveysDF)){
    if(! 'SurveyName' %in% names(surveysDF)){
      surveysDF$SurveyName <- surveysDF$name
    }
  }
  return(surveysDF)
}

#' Return a data.frame of the survey responses for a given survey
#'
#' See \code{\link[qualtRics]{fetch_survey}} for complete information on available options.
#'
#' @param surveyID Unique survey ID as returned from \code{\link{get_surveys}}.
#' @param label default is \code{FALSE}.
#' @param convert default is \code{FALSE}.
#' @param force_request default is \code{TRUE}.
#' @param ... See \code{\link[qualtRics]{fetch_survey}.}
#' @import qualtRics
#' @export
get_survey_responses<-function(surveyID, label = FALSE, convert = FALSE, force_request = TRUE, ...) {
  responsesDF <- qualtRics::fetch_survey(surveyID,
                                         label = label,
                                         convert = convert,
                                         force_request = force_request, ...)
  return(responsesDF)
}

#' Get rubrics
#'
#' @param rubric_filenames Data frame with column of file paths named "file".
#' @param type "scoring" for special handling of scoring rubrics, or "recoding"
#' for special handling of recoding rubrics.
#' @param source Unused, default's to 'csv' for now.
#'
#' @return If \code{type='scoring'}, returns a long data frame of rubrics with names:
#' "file"           "data_file_name" "scale_name"     "column_name"
#' "reverse"        "transform"      "scored_scale"   "include"
#' "min"            "max". Otherwise, it returns the transforming rubric with names:
#' "file"           "data_file_name" "scale_name"     "column_name"    "answer"
#' "response"       "score"
#' @import dplyr
#' @import tidyr
#' @export
get_rubrics <- function (rubric_filenames, type = 'scoring', source = "csv")
{
  requireNamespace('dplyr', quietly = TRUE)
  requireNamespace('tidyr', quietly = TRUE)
  if(! type %in% c('scoring', 'recoding')){
    stop('Option `type` must be either "scoring" or "recoding"')
  }

  csv_rubrics <- rubric_filenames %>%
    mutate(file = as.character(file)) %>%
    group_by(file) %>% do({
      data_frame(rubric = list(read.csv(.$file[[1]], header = T,
                                        stringsAsFactors = F)))
    })

  rubric_data_long <- csv_rubrics %>%
    group_by(file) %>%
    do({
      thisDF <- .$rubric[[1]]

      names(thisDF) <- tolower(gsub(" ",
                                    "_",
                                    gsub("\\.",
                                         "_", names(thisDF))))
      if(type == 'scoring'){
        aDF <- gather(thisDF,
                      scored_scale,
                      include,
                      -one_of("data_file_name",
                              "scale_name", "column_name", "reverse", "transform",
                              "min", "max")) %>%
          mutate_all(funs(as.character))
      } else if (type == 'recoding') {

        aDF <- thisDF %>%
          mutate_all(funs(as.character))
      }
      aDF
    })

  return(rubric_data_long)
}


#' score questionnaire psych
#'
#' @param dataDF dataDF
#' @param rubricsDF rubricsDF
#' @param scale_name scale_name
#' @param return_with_data return_with_data
#'
#' @import psych
#' @import dplyr
#' @import tidyr
score_questionnaire_psych <- function(dataDF, rubricsDF, scale_name = NULL, return_with_data = FALSE){
  requireNamespace(psych, quietly = TRUE)

  key_list <- create_key_from_rubric(rubricsDF = rubricsDF, scale_name = scale_name)

  dataDF_w <- spread(select(dataDF, SID, item, value),
                     item, value)
  scored_scales <- scoreItems(key_list, dataDF_w)
  if(return_with_data){
    scored_scales$input_data <- dataDF_w
  }
  rownames(scored_scales$scores) <- dataDF_w$SID
  return(scored_scales)
}
