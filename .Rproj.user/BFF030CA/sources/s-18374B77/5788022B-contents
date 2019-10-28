library(curl)
library(httr)
library(jsonlite)


#' List External Resources
#'
#' List external resources for a study
#'
#' @return List of external resources
#' @param idno Study IDNo
#' @export
datasetExternalResources <- function(api_key=NULL, api_base_url=NULL, idno){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  endpoint=paste0('datasets/',idno,'/resources')
  url=get_api_url(endpoint)

  httpResponse <- GET(url, add_headers("X-API-KEY" = api_key), accept_json(), verbose(get_verbose()))
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }else{
    output=fromJSON(content(httpResponse,"text"))
  }

  return (output)
}


#' ImportRDF
#'
#' Import an RDF file
#'
#' @return NULL
#' @param dataset_idno Study IDNo
#' @param rdf_file RDF file path
#' @export
importRDF <- function(api_key=NULL,
                      api_base_url=NULL,
                      dataset_idno,
                      rdf_file
                      ){

  endpoint=paste0('datasets/',dataset_idno,'/resources/import_rdf')

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  url=get_api_url(endpoint)

  options=list(
    "file"=upload_file(rdf_file)
  )

  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key),body=options, accept_json(), verbose(get_verbose()))
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }else{
    output=fromJSON(content(httpResponse,"text"))
  }

  return (output)
}




#' Upload external resources
#'
#' Upload an external resource file
#'
#' @return NULL
#' @param dataset_idno Study IDNo
#' @param resource_id (Optional) External resource ID
#' @param file External resource file to be uploaded
#' @export
uploadFile <- function(api_key=NULL,
                      api_base_url=NULL,
                      dataset_idno,
                      resource_id=NULL,
                      file
){

  endpoint=paste0('datasets/',dataset_idno,'/files')

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  url=get_api_url(endpoint)

  options=list(
    "file"=upload_file(file)
  )

  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key),body=options, accept_json(), verbose(get_verbose()))
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }else{
    output=fromJSON(content(httpResponse,"text"))
  }

  return (output)
}
