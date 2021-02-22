#' Get scoring rubrics
#'
#' Wrangle scoring rubrics into a long-format tibble.
#'
#' @param path_to_rubrics Character string specifying full path to directory where
#' scoring rubrics are saved.
#'
#' @return Returns a long-format tibble of rubrics with the following column names:
#' "file"           "data_file_name" "scale_name"     "column_name"
#' "reverse"        "transform"      "scored_scale"   "include"
#' "min"            "max".
#' @export
get_rubrics <- function(path_to_rubrics){

  rubric_filenames <- tibble(file = dir(file.path(path_to_rubrics),
                                            pattern = '.*scoring_rubric.*.csv',
                                            full.names = TRUE))

  csv_rubrics <- rubric_filenames %>%
    rowwise() %>%
    mutate(rubric = list(read_csv(file) %>%
                        janitor::clean_names())) %>%
    ungroup()

  rubric_data_long <- csv_rubrics %>%
    group_by(file) %>%
    do({
      thisDF <- .$rubric[[1]]
      aDF <- gather(thisDF,
                    scored_scale,
                    include,
                    -any_of(c("data_file_name",
                            "scale_name", "column_name", "reverse", "transform",
                            "min", "max"))) %>%
        mutate(across(everything(), as.character))
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
