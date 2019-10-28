library(curl)
library(httr)
library(jsonlite)


#' GetDatasets
#'
#' Load a list of all datasets or get info for a single dataset/study
#'
#' @return List of studies or a single study info
#' @param idno (Optional) Dataset IDNo
#' @export
datasets <- function(api_key=NULL, api_base_url=NULL, idno=NULL){

  endpoint='datasets'

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  if(!is.null(idno)){
    endpoint=paste0(endpoint,'/',idno)
  }

  url=get_api_url(endpoint)

  httpResponse <- GET(url, add_headers("X-API-KEY" = api_key), accept_json())
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }else{
    output=fromJSON(content(httpResponse,"text"))
  }

  return (output)
}





#' ImportDDI
#'
#' Import a DDI file
#'
#' @return NULL
#' @param xml_file (Required) DDI/XML file path
#' @param repositoryid Collection ID that owns the study
#' @param overwrite Overwrite if a study with the same ID already exists? Valid values "yes", "no"
#' @param access_policy Select the access policy suitable for your data. Valid values - "open" "direct" "public" "licensed" "enclave" "remote" "other"
#' @param data_remote_url Link to the website where the data is available. Required if access_policy is set to 'remote'.
#' @param rdf_file RDF file path
#' @param published Set status for study - 0 = Draft, 1 = Published
#' @param verbose Show verbose output - True, False
#' @export
importDDI <- function(api_key=NULL,
                      api_base_url=NULL,
                      xml_file=NULL,
                      rdf_file=NULL,
                      repositoryid=NULL,
                      overwrite='no',
                      access_policy=NULL,
                      data_remote_url=NULL,
                      published=NULL){

  endpoint='datasets/import_ddi'

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  url=get_api_url(endpoint)

  options=list(
    "file"=upload_file(xml_file),
    "overwrite"=overwrite,
    "published"=published,
    "repositoryid"=repositoryid,
    "access_policy"=access_policy,
    "data_remote_url"=data_remote_url
  )

  if (!is.null(rdf_file) && file.exists(rdf_file)){
    options["rdf"]=upload_file(rdf_file)
  }

  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key),body=options, accept_json(), verbose(get_verbose()))
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }else{
    output=fromJSON(content(httpResponse,"text"))
  }

  return (output)
}


