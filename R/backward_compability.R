#' (deprecated - use set_api_verbose) Set API verbose option
#'
#' Deprecated function, available for backward compability
#'
#' @param verbose	Verbose output to TRUE or FALSE
#'
#' @export
set_verbose <- function(verbose=FALSE) {
  .Deprecated("set_api_verbose")
  set_api_verbose(verbose=verbose)
}
