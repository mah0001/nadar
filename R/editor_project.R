#' Create new project
#'
#' Create a new project
#'
#' @return NULL
#' @param type (required) Type of project - survey, geospatial, table, document, timeseries
#' @param idno \strong{(required)} Project unique IDNO
#' @param metadata \strong{(required)} Metadata list depending on the type of study
#' @param api_key API key (optional if API key is set using set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using set_api_url)
#'
#' @examples
#'
#'
#' create (
#'   type="survey",
#'   idno = "unique-idno-for-project',
#'   metadata = list()
#' )
#'
#'
#'
#'
#' @export
editor_create <- function(
    type,
    metadata,
    idno=NULL,
    thumbnail=NULL,
    api_key=NULL,
    api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_editor_api_key();
  }

  options=list(
    "idno"=idno
  )

  options= c(options,metadata)

  # Create url
  endpoint <- paste0('editor/create/',type)
  if(is.null(api_base_url)){
    url=get_editor_api_url(endpoint=endpoint)
  } else {
    url = paste0(api_base_url,"/",endpoint)
  }

  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body=options,
                       content_type_json(),
                       encode="json",
                       accept_json(),
                       verbose(get_verbose()))

  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  thumbnail_result=NULL

  #upload thumbnail
  if(!is.null(thumbnail) && file.exists(thumbnail)) {
    thumbnail_result=editor_thumbnail_upload(idno=idno,thumbnail = thumbnail)
  }

  #set default thumbnail
  if(!is.null(thumbnail) && thumbnail == 'default'){
    thumbnail_result= editor_thumbnail_delete(idno=idno)
  }

  output=list(
    "status_code"=httpResponse$status_code,
    "response"= nada_http_response_json(httpResponse),
    "thumbnail"=thumbnail_result
  )

  return (output)
}





#' Find a project by IDNO
#'
#' Find a project by IDNO
#'
#' @return NULL
#' @param idno (required) Study unique identifier
#' @param api_key API key (optional if API key is set using set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using set_api_url)
#'
#' @examples
#'
#' find_by_idno (
#'   idno="survey-idno-test"
#' )
#'
#' @export
find_by_idno <- function(
    idno,
    api_key=NULL,
    api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  # Create url
  endpoint <- paste0('datasets/',idno)
  if(is.null(api_base_url)){
    url=get_api_url(endpoint=endpoint)
  } else {
    url = paste0(api_base_url,"/",endpoint)
  }

  httpResponse <- GET(url, add_headers("X-API-KEY" = api_key))

  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  output=list(
    "status_code"=httpResponse$status_code,
    "response"=fromJSON(content(httpResponse,"text"))
  )

  return (output)
}

