% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\name{thumbnail_delete}
\alias{thumbnail_delete}
\title{Delete thumbnail for a study}
\usage{
thumbnail_delete(idno, api_key = NULL, api_base_url = NULL)
}
\arguments{
\item{idno}{(required) Study unique identifier}

\item{api_key}{API key (optional if API key is set using set_api_key)}

\item{api_base_url}{API base endpoint (optional if API base endpoint is set using set_api_url)}
}
\description{
Delete thumbnail for a study
}
\examples{

delete_thumbnail (
  idno="survey-idno-test"
)

}
