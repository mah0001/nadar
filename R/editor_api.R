#' Set Editor API settings
#'
#' `set_editor_api` is used to set the API settings consisting of the API URL, the API key and the verbose option.
#' `set_editor_api` is a wrapper for the functions `set_editor_api_url`, `set_editor_api_key` and `set_editor_api_verbose`.
#'
#' @param api_url API base endpoint (URL)
#' @param api_key API key
#' @param verbose Verbose setting for API call (default is FALSE)
#'
#' @examples
#' #set_api("http://mynadacatalog.myorganization.org/index.php/api/", "abc123", verbose = TRUE)
set_editor_api <- function(api_url, api_key, verbose = FALSE) {
  set_editor_api_url(api_url)
  set_editor_api_key(api_key)
  set_editor_api_verbose(verbose)
}

#' Set API key
#'
#' `set_editor_api_key` is used to set the API key in the global environment variables.
#'
#' @param api_key API Key
#'
#' @export
set_editor_api_key <- function(api_key) {
  pkg.globals$editor_api_key <- api_key
}

#' Get Editor API key
#'
#' `get_editor_api_key` returns the API key from global environment variables.
#'
#' @return API key generated in NADA
#'
#' @export
get_editor_api_key <- function()
{
  return(pkg.globals$editor_api_key)
}

#' Set Editor API URL
#'
#' `editor_api_url` is used to set the API base endpoint URL in the global environment variables.
#'
#' @param api_url API base endpoint
#'
#' @export
set_editor_api_url <- function(api_url) {
  pkg.globals$editor_api_base_url <- api_url
}

#' Get Editor API URL
#'
#' `get_editor_api_key` returns the API base endpoint URL from global environment variables.
#'
#' @param endpoint Endpoint to be added to the base endpoint URL, e.g., ""
#'
#' @return API
#'
#' @export
get_editor_api_url <- function(endpoint = NULL)
{
  url = pkg.globals$editor_api_base_url

  if(!is.null(endpoint)){
    url = paste0(url,"/",endpoint)
  }

  return(url)
}

#' set_editor_verbose
#'
#' Set API calls verbose options
#'
#' @param verbose Verbose output to TRUE or FALSE
#' @export
set_editor_api_verbose <- function(verbose=FALSE) {
  pkg.globals$editor_verbose <- verbose
}

#' get_editor_verbose
#'
#' `get_api_verbose`  returns the API verbose parameter from the global environment variables.
#'
#' @return verbose parameter value
#' @export
get_editor_api_verbose <- function()
{
  if (!is.logical(pkg.globals$editor_verbose)){
    return (FALSE)
  }

  return (pkg.globals$editor_verbose)
}
