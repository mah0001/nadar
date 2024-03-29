% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/editor_api.R
\name{set_editor_api}
\alias{set_editor_api}
\title{Set Editor API settings}
\usage{
set_editor_api(api_url, api_key, verbose = FALSE)
}
\arguments{
\item{api_url}{API base endpoint (URL)}

\item{api_key}{API key}

\item{verbose}{Verbose setting for API call (default is FALSE)}
}
\description{
`set_editor_api` is used to set the API settings consisting of the API URL, the API key and the verbose option.
`set_editor_api` is a wrapper for the functions `set_editor_api_url`, `set_editor_api_key` and `set_editor_api_verbose`.
}
\examples{
#set_api("http://mynadacatalog.myorganization.org/index.php/api/", "abc123", verbose = TRUE)
}
