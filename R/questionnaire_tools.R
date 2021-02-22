#' Get scoring rubrics
#'
#' Wrangle scoring rubrics into a long-format tibble.
#'
#' @param path_to_rubrics Character string specifying full path to directory where
#' scoring rubrics are saved.
#'
#' @return Returns a long-format tibble of scoring rubrics with the following column names:
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

#' Create \code{psych} key from rubric
#'
#' @param rubricsDF A long-format tibble of scoring rubrics as returned by \code{\link{get_rubrics}}.
#' @param scale_name Name of the scale you wish to create a scoring key for, as a character string.
#' By default, all scales represented in \code{rubricsDF} are selected (i.e. \code{scale_name} is set to \code{NULL}).
#'
#' @return A named list containing scoring keys to be used by the \code{psych} package.
create_key_from_rubric <- function(rubricsDF, scale_name = NULL){
  if(!is.null(scale_name)){
    rubricsDF <-
      rubricsDF %>%
      filter(.data$scale_name == scale_name)
  }

  if('include' %in% names(rubricsDF)){
    rubricsDF <-
      rubricsDF %>%
      ungroup() %>%
      filter(.data$include == 1)
  }
  if('reverse' %in% names(rubricsDF)){
    rubricsDF <-
      rubricsDF %>%
      ungroup() %>%
      mutate(reverse = ifelse(is.na(.data$reverse), 0, .data$reverse),
             rscore_col_name = paste0(ifelse(.data$reverse == 1, '-', ''), .data$column_name))
  } else {
    warning('No reverse-keyed items.')
    rubricsDF <-
      rubricsDF %>%
      ungroup() %>%
      mutate(rscore_col_name = .data$column_name)
  }

  keys_list_l <-
    rubricsDF %>%
    select(.data$rscore_col_name, .data$scored_scale)

  scored_scale_names <- unique(keys_list_l$scored_scale)

  key_list <- lapply(scored_scale_names, function(x){
    keys_list_l$rscore_col_name[keys_list_l$scored_scale == x]
  })
  names(key_list) <- scored_scale_names
  return(key_list)
}
