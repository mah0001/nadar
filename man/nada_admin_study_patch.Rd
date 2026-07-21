\name{nada_admin_study_patch}
\alias{nada_admin_study_patch}
\title{Patch study metadata using JSON Patch}
\usage{
nada_admin_study_patch(
  type,
  idno,
  patches,
  api_key = NULL,
  api_base_url = NULL
)
}
\arguments{
\item{type}{(required) Type of study - survey, geospatial, table, document, timeseries, timeseriesdb, image, video}

\item{idno}{(required) Study unique identifier}

\item{patches}{(required) List of JSON Patch operations (each operation must include \code{op} and \code{path})}

\item{api_key}{API key (optional if API key is set using nada_set_api_key)}

\item{api_base_url}{API base endpoint (optional if API base endpoint is set using nada_set_api_url)}
}
\value{
list
}
\description{
Apply RFC 6902 JSON Patch operations to an existing study metadata document.
}
\examples{
\dontrun{
patches <- list(
  list(
    op = "replace",
    path = "/path/to/the/field",
    value = "value"
  )
)

nada_admin_study_patch(
  type = "survey",
  idno = "survey-idno-test",
  patches = patches
)
}
}
