% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datafile_utils.R
\name{.read_data}
\alias{.read_data}
\title{read data file}
\usage{
.read_data(variables, file = "", type = "csv", digits = "", ...)
}
\arguments{
\item{variables}{variable description}

\item{file}{input data(csv) file path}

\item{type}{Type of data file (csv | tsv)}
}
\value{
A dataframe
}
\description{
read data(csv) file and return the dataframe
}
\examples{
 .read_data({},'D:\\\\MetadataEditorSource\\\\MetadataEditor_Dev\\\\MetadataEditor\\\\Sample Data\\\\in\\\\HND_2012_L2L.csv')

}
