

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
