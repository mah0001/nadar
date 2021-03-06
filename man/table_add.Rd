% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tables.R
\name{table_add}
\alias{table_add}
\title{Create new table}
\usage{
table_add(
  idno,
  metadata,
  repositoryid = NULL,
  access_policy = NULL,
  data_remote_url = NULL,
  published = 0,
  overwrite = "no",
  thumbnail = NULL,
  api_key = NULL,
  api_base_url = NULL
)
}
\arguments{
\item{idno}{(required) Study unique identifier}

\item{metadata}{\strong{(required)} Metadata using the Table Schema}

\item{repositoryid}{Collection ID that owns the study}

\item{access_policy}{Select the access policy suitable for your data. Valid values - "open" "direct" "public" "licensed" "enclave" "remote" "other"}

\item{data_remote_url}{Link to the website where the data is available. Required if access_policy is set to 'remote'.}

\item{published}{Set status for study - 0 = Draft, 1 = Published}

\item{overwrite}{Overwrite if a study with the same ID already exists? Valid values "yes", "no"}
}
\description{
Create a new table
}
\examples{

metadata=list(
"metadata_information"= list(
"title"= "string",
"idno"= "string",
"producers"= list(
 list(
      "name"= "string",
      "abbr"= "string",
      "affiliation"= "string",
      "role"= "string"
    )
  ),
  "production_date"= "string",
  "version"= "string"
),
"table_description"= list(
  "title_statement"= list(
    "idno"= "table-unique-id",
    "title"= "table title",
    "sub_title"= "string",
    "alternate_title"= "string",
    "abbreviated_title"= "string"
  ),
),
"files"= list(
  list(
    "file_uri"= "http://example.com/files/file.xls",
    "format"= "application/excel",
    "location"= "sheet1",
    "note"= "some note"
  )
)
)

table_add (
  idno="table-idno",
  published = 1,
  overwrite = "yes",
  metadata = metadata,
  thumbnail ="images/thumbnail.jpg"
)




}
