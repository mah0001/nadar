#' List all widgets
#'
#' List all widgets
#'
#' @return List of widgets
#' @param api_key API key (optional if API key is set using nada_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using nada_set_api_url)
#' @export
nada_widget_list <- function(api_key=NULL,
                        api_base_url=NULL){

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  endpoint=paste0('widgets/')
  url=nada_get_api_url(endpoint)

  httpResponse <- GET(url, add_headers("X-API-KEY" = api_key), accept_json(), verbose(nada_get_verbose()))
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }else{
    output=fromJSON(content(httpResponse,"text"))
  }

  output=list(
    content = output,
    api_url = url,
    status_code = httpResponse$status_code
  )

  return(output)
}


#' Create widget
#'
#' Create widget
#'
#' @return List with status_code and response
#' @param uuid \strong{(required)} Unique identifier for widget
#' @param options (list) widget options (title, description, thumbnail)
#' @param zip_file Zip file
#' @param api_key API key (optional if API key is set using nada_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using nada_set_api_url)
#' @export
nada_admin_widget_create <- function(uuid, options=list(), zip_file, api_key=NULL, api_base_url=NULL){

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  if (!file.exists(zip_file)){
    stop(paste0("zip_file not found: ",zip_file), call. = FALSE)
  }

  options[["file"]]=upload_file(zip_file)
  options[["uuid"]]=uuid

  url=nada_get_api_url(paste0('widgets/',uuid))
  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body=options)

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




#' Attach widget to a study
#'
#' Attach widget to a study
#'
#' @return NULL
#' @param idno (required) Study IDNo
#' @param uuid (required) Widget ID
#'
#' @export
nada_admin_widget_attach <- function(
  idno,
  uuid,
  api_key=NULL,
  api_base_url=NULL){

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  options=list(
    "idno"=idno,
    "uuid"=uuid
  )

  url=nada_get_api_url(paste0('widgets/attach_to_study'))
  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body=options,
                       content_type_json(),
                       encode="json",
                       accept_json(),
                       verbose(nada_get_verbose()))

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


#' Detach widget from a study
#'
#' Detach widgetfrom study
#'
#' @return NULL
#' @param idno (required) Study IDNo
#' @param uuid (required) Widget ID
#'
#' @export
nada_admin_widget_detach <- function(
  idno,
  uuid,
  api_key=NULL,
  api_base_url=NULL){

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  options=list(
    "idno"=idno,
    "uuid"=uuid
  )

  url=nada_get_api_url(paste0('widgets/detach_study'))
  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body=options,
                       content_type_json(),
                       encode="json",
                       accept_json(),
                       verbose(nada_get_verbose()))

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





#' Delete widget
#'
#' Delete widget
#'
#' @return NULL
#' @param uuid (required) Widget ID
#'
#' @export
nada_admin_widget_delete <- function(
  uuid,
  api_key=NULL,
  api_base_url=NULL){

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  url=nada_get_api_url(paste0('widgets/',uuid))
  httpResponse <- DELETE(url,
                       add_headers("X-API-KEY" = api_key),
                       content_type_json(),
                       encode="json",
                       accept_json(),
                       verbose(nada_get_verbose()))

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
