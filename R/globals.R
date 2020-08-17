#global variables for the package
pkg.globals <- new.env()

#API key
pkg.globals$api_key <- ""

#API base url
pkg.globals$api_base_url <- ""

#Enable/disable Verbose mode
pkg.globals$verbose <- FALSE


#' SetApiKey
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param api_key API Key
#' @return A matrix of the infile
#' @export
set_api_key <- function(api_key) {
  pkg.globals$api_key <- api_key
}

#' GetApiKey
#'
#' Get API key from global Environment variable
#'
#' @return API Key
#' @export
get_api_key <- function()
{
  return (pkg.globals$api_key)
}



#' set_api_url
#'
#' Set API Base URL
#'
#' @param api_url API base endpoint
#' @export
set_api_url <- function(api_url) {
  pkg.globals$api_base_url <- api_url
}

#' get_api_url
#'
#' Get API base endpoint
#'
#' @return API base url
#' @export
get_api_url <- function(endpoint=NULL)
{
  url=pkg.globals$api_base_url

  if(!is.null(endpoint)){
    url=paste0(url,"/",endpoint)
  }

  return (url)
}





#' set_verbose
#'
#' Set API calls verbose options
#'
#' @param verbose Verbose output to TRUE or FALSE
#' @export
set_verbose <- function(verbose=FALSE) {
  pkg.globals$verbose <- verbose
}

#' get_verbose
#'
#' Get Verbose value
#'
#' @return Verbose value
#' @export
get_verbose <- function()
{
  if (!is.logical(pkg.globals$verbose)){
    return (FALSE)
  }

  return (pkg.globals$verbose)
}


get_disposition_filename <- function(httpResponse) {
  filename=sub(".*filename=", "", headers(httpResponse)$`content-disposition`)
  filename=gsub('"','',filename)
  return (noquote(filename))
}


