% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_redcap_raw.R
\name{get_redcap_raw}
\alias{get_redcap_raw}
\title{Get raw Redcap data}
\usage{
get_redcap_raw(path_to_creds, identifiable = FALSE)
}
\arguments{
\item{path_to_creds}{Full path to credentials file containing API token and base URL.
The credentials file must have the following columns: data_source, base_url, api_token.}

\item{identifiable}{A logical value indicating whether identifiable variables should be
included in the output. This identifiable information includes:
\itemize{
\item first name
\item last name
\item email address
\item phone number
\item address
\item name of friend (emergency contact)
}}
}
\value{
A tibble containing raw Redcap data
}
\description{
Uses \code{\link[REDCapR]{redcap_read_oneshot}} to interface with Redcap API and fetch raw data.
}
