#' Set API settings
#'
#' `nada_set_api` is used to set the API settings consisting of the API URL, the API key and the verbose option.
#' `nada_set_api` is a wrapper for the functions `nada_set_api_url`, `nada_set_api_key` and `nada_set_api_verbose`.
#'
#' @param api_url API base endpoint (URL)
#' @param api_key API key
#' @param verbose Verbose setting for API call (default is FALSE)
#'
#' @examples
#' #nada_set_api("http://mynadacatalog.myorganization.org/index.php/api/", "abc123", verbose = TRUE)
#' @export
nada_set_api <- function(api_url, api_key, verbose = FALSE) {
  nada_set_api_url(api_url)
  nada_set_api_key(api_key)
  nada_set_api_verbose(verbose)
}

#' Set API key
#'
#' `nada_set_api_key` is used to set the API key in the global environment variables.
#'
#' @param api_key API Key
#'
#' @export
nada_set_api_key <- function(api_key) {
  pkg.globals$api_key <- api_key
}

#' Get API key
#'
#' `nada_get_api_key` returns the API key from global environment variables.
#'
#' @return API key generated in NADA
#'
#' @export
nada_get_api_key <- function()
{
  return(pkg.globals$api_key)
}

#' Set API URL
#'
#' `nada_set_api_url` is used to set the API base endpoint URL in the global environment variables.
#'
#' @param api_url API base endpoint
#'
#' @export
nada_set_api_url <- function(api_url) {
  pkg.globals$api_base_url <- api_url
}

#' Get API URL
#'
#' `nada_get_api_url` returns the API base endpoint URL from global environment variables.
#'
#' @param endpoint Endpoint to be added to the base endpoint URL, e.g., ""
#'
#' @return API base URL with optional endpoint appended
#'
#' @export
nada_get_api_url <- function(endpoint = NULL)
{
  url = pkg.globals$api_base_url

  if(!is.null(endpoint)){
    url = paste0(url,"/",endpoint)
  }

  return(url)
}

#' Set API verbose mode
#'
#' Set API calls verbose options
#'
#' @param verbose Verbose output to TRUE or FALSE
#' @export
nada_set_api_verbose <- function(verbose=FALSE) {
  pkg.globals$verbose <- verbose
}

#' Get API verbose mode
#'
#' `nada_get_api_verbose` returns the API verbose parameter from the global environment variables.
#'
#' @return verbose parameter value
#' @export
nada_get_api_verbose <- function()
{
  if (!is.logical(pkg.globals$verbose)){
    return (FALSE)
  }

  return (pkg.globals$verbose)
}

#' Get verbose mode (alias for nada_get_api_verbose)
#'
#' `nada_get_verbose` returns the API verbose parameter from the global environment variables.
#' This is an alias for `nada_get_api_verbose()`.
#'
#' @return verbose parameter value
#' @export
nada_get_verbose <- function()
{
  return(nada_get_api_verbose())
}
