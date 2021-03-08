#' Register Qualtrics API credentials in your current session
#'
#' Reads the API token and base URL from a credentials file (in .csv format) and sets up the environment
#' variables necessary for accessing data. Uses
#' \code{\link[qualtRics]{qualtrics_api_credentials}}, but does not save
#' credentials to the .Renviron file.
#'
#' @param path_to_creds Full path to credentials file containing API token and base URL.
#' The credentials file must have the following columns: data_source, base_url, api_token.
#'
#' @export
register_qualtrics_credentials <- function(path_to_creds){

  if (!file.exists(path_to_creds)) {
    stop("A credentials file does not exist at that path.")
  }

  # read in credentials
  credentials <- read_csv(path_to_creds)

  if (!identical(sort(c("data_source", "base_url", "api_token")), sort(names(credentials)))) {
    stop("Your credentials file must have the following columns: data_source, base_url, api_token")
  }

  # extract API token
  qualtrics_token <- credentials %>%
    filter(.data$data_source == "qualtrics") %>%
    pull(.data$api_token)

  # extract baseURL
  base_url <- credentials %>%
    filter(.data$data_source == "qualtrics") %>%
    pull(base_url)

  # register credentials -- returns TRUE if successful
  qualtRics::qualtrics_api_credentials(
    api_key = qualtrics_token,
    base_url = base_url,
    install = FALSE)
}
