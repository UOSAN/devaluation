#' Get raw Redcap data
#'
#' @param path_to_creds Full path to credentials file containing API token and base URL.
#' The credentials file must have the following columns: data_source, base_url, api_token.
#' @param identifiable A logical value indicating whether identifiable variables should be
#' included in the output. This identifiable information includes:
#' * first name
#' * last name
#' * email address
#' * phone number
#' * address
#' * name of friend (emergency contact)
#'
#' @return A tibble containing raw Redcap data
#' @export
#'
get_redcap_raw <- function(path_to_creds, identifiable = FALSE){

  # read in credentials
  credentials <- read_csv(path_to_creds)

  if (!identical(sort(c("data_source", "base_url", "api_token")), sort(names(credentials)))) {
    stop("Your credentials file must have the following columns: data_source, base_url, api_token")
  }

  # extract API token
  redcap_token <- credentials %>%
    filter(.data$data_source == "redcap") %>%
    pull(.data$api_token)

  # extract base URL (i.e. URI, or "uniform resource identifier")
  base_url <- credentials %>%
    filter(.data$data_source == "redcap") %>%
    pull(base_url)

  # get list with raw data inside of it
  raw_list <- redcap_read_oneshot(redcap_uri = base_url, token = redcap_token)

  # only select session data (this removes empty rows labeled "t1_arm1" - "t8_arm1")
  redcap_data_raw <- raw_list$data %>%
    as_tibble() %>%
    filter(str_detect(.data$redcap_event_name, "session"))

  # identifiable variables
  identifiable_vars <- c("first_name", "last_name", "email", "phone", "address", "friend")

  if (identifiable) {
    return(redcap_data_raw)
    } else {
    return(redcap_data_raw %>%
             select(-starts_with(all_of(identifiable_vars)))) # remove identifiable variables
    }
  }