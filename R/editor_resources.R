#' Editor - list External Resources
#'
#' List external resources for a study
#'
#' @return List of external resources
#' @param idno Project IDNo
#' @export
editor_resources_list <- function(idno, api_key=NULL, api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_editor_api_key();
  }

  endpoint=paste0('resources/',idno)
  url=get_editor_api_url(endpoint)

  httpResponse <- GET(url, add_headers("X-API-KEY" = api_key), accept_json(), verbose(get_verbose()))
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  output=list(
    "status_code"=httpResponse$status_code,
    "response"=nada_http_response_json(httpResponse)
  )

  return (output)

}


#' Editor upload external resource file
#'
#' Upload an external resource file
#'
#' @return NULL
#' @param idno Project IDNo
#' @param file External resource file to be uploaded
#' @export
editor_resources_upload <- function(
    idno,
    file,
    api_key=NULL,
    api_base_url=NULL){

  endpoint=paste0('files/',idno,'/documentation')

  if(is.null(api_key)){
    api_key=get_editor_api_key();
  }

  url=get_editor_api_url(endpoint)

  options=list(
    "file"=upload_file(file)
  )

  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key),body=options, accept_json(), verbose(get_verbose()))
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  output=list(
    "status_code"=httpResponse$status_code,
    "response"= nada_http_response_json(httpResponse)
  )

  return (output)
}



#' Editor create new resource
#'
#' Create a new resource
#'
#' @return NULL
#' @param idno \strong{(required)} Study IDNO
#' @param dctype Resource document type
#' @param title Resource title
#' @param dcformat Resource file format
#' @param author Author name
#' @param dcdate Date using YYYY-MM-DD format
#' @param country Country name
#' @param language Language or Language code
#' @param contributor Contributor name
#' @param publisher Publisher name
#' @param rights Rights
#' @param description Resource detailed description
#' @param abstract  Resource abstract
#' @param toc Table of contents
#' @param file_path File path for uploading
#' @param overwrite Overwrite if resource already exists? Accepted values "yes", "no"
#'
#'
#'
#'
#' @export
editor_resources_add <- function(
    idno,
    dctype,
    title,
    dcformat=NULL,
    author=NULL,
    dcdate=NULL,
    country=NULL,
    language=NULL,
    contributor=NULL,
    publisher=NULL,
    rights=NULL,
    description=NULL,
    abstract=NULL,
    toc=NULL,
    file_path=NULL,
    overwrite="no",
    api_key=NULL,
    api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_editor_api_key();
  }

  options=list(
    idno=idno,
    dctype=dctype,
    dcformat=dcformat,
    title=title,
    author=author,
    dcdate=dcdate,
    country=country,
    language=language,
    contributor=contributor,
    publisher=publisher,
    rights=rights,
    description=description,
    abstract=abstract,
    toc=toc,
    overwrite=overwrite
  )

  if (file.exists(file_path)){
    options$file=upload_file(file_path)
  }
  else if(is_valid_url(file_path)){
    options[['filename']]=file_path
  }

  url=get_editor_api_url(paste0('resources/',idno))
  print(url)
  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body=options,
                       accept_json(),
                       verbose(get_verbose()))

  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  output=list(
    "status_code"=httpResponse$status_code,
    "response"=nada_http_response_json(httpResponse)
  )

  return (output)
}



#' Editor - delete External Resources
#'
#' Delete external resources for a project
#'
#' @return
#' @param idno Project IDNo
#' @param resource_id Resource ID
#' @export
editor_resources_delete <- function(idno, resource_id, api_key=NULL, api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_editor_api_key();
  }

  endpoint=paste0('resources/delete/',idno,'/',resource_id)
  url=get_editor_api_url(endpoint)

  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key), accept_json(), verbose(get_verbose()))
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  output=list(
    "status_code"=httpResponse$status_code,
    "response"= nada_http_response_json(httpResponse)
  )

  return (output)
}



#' Editor import RDF
#'
#' Import an RDF file
#'
#' @return NULL
#' @param idno Project IDNo
#' @param rdf_file RDF file path
#' @param skip_uploads TRUE/FALSE - If TRUE, won't upload files
#' @param overwrite yes/no - Overwrite existing resources
#' @export
editor_resources_import_rdf <- function(
    idno,
    rdf_file,
    api_key=NULL,
    api_base_url=NULL
){

  if(is.null(api_key)){
    api_key=get_editor_api_key();
  }

  url=get_editor_api_url(paste0('resources/import/',idno))

  options=list(
    "file"=upload_file(rdf_file)
  )

  print(url)


  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key),body=options, accept_json(), verbose(get_verbose()))
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  output=list(
    "status_code"=httpResponse$status_code,
    "response"= nada_http_response_json(httpResponse)
  )

  return (output)
}
