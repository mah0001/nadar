#' Create new collection
#'
#' @return NULL
#' @param repositoryid (Required) new collection ID
#' @param title Title of the collection
#' @param short_text Short description of the collection
#' @param long_text Long description of the collection
#' @param thumbnail Thumbnail file path
#' @param weight Weight
#' @param section Section
#' @param ispublished Publish the collection on specified catalog (1 - publish, 0 - not publish)
#' @param verbose Show verbose output - True, False
#' @export
collection_add <- function(
  repositoryid = NULL,
  title = NULL,
  short_text = NULL,
  long_text = NULL,
  thumbnail = NULL,
  weight = 0,
  section = 0,
  ispublished = 0,
  api_key = NULL,
  api_base_url = NULL){

  endpoint='collections'

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  url=get_api_url(endpoint)

  options = list(
    "repositoryid" = repositoryid,
    "title" = title,
    "short_text" = short_text,
    "long_text" = long_text,
    "weight" = weight,
    "section" = section,
    "ispublished" = ispublished
  )


  if (!is.null(thumbnail) && file.exists(thumbnail)){
    options[["thumbnail"]] = upload_file(thumbnail)
  }

  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key),
                       body=options,
                       verbose(get_verbose()))

  output = NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  output=list(
    "status_code"=httpResponse$status_code,
    "response"=httpResponse
  )

  return (output)
}

#' update collection
#'
#' @return NULL
#' @param repositoryid (Required) Collection ID to be updated
#' @param title Title of the collection
#' @param short_text Short description of the collection
#' @param long_text Long description of the collection
#' @param thumbnail Thumbnail file path
#' @param weight Weight
#' @param section Section
#' @param ispublished Publish the collection on specified catalog (1 - publish, 0 - not publish)
#' @param verbose Show verbose output - True, False
#' @export
collection_update <- function(
  repositoryid = NULL,
  title = NULL,
  short_text = NULL,
  long_text = NULL,
  thumbnail = NULL,
  weight = 0,
  section = 0,
  ispublished = 0,
  api_key = NULL,
  api_base_url = NULL){

  endpoint='collections/update'

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  url=get_api_url(endpoint)

  options = list(
    "repositoryid" = repositoryid,
    "title" = title,
    "short_text" = short_text,
    "long_text" = long_text,
    "weight" = weight,
    "section" = section,
    "ispublished" = ispublished
  )


  if (!is.null(thumbnail) && file.exists(thumbnail)){
    options[["thumbnail"]] = upload_file(thumbnail)
  }

  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key),
                       body=options,
                       verbose(get_verbose()))

  output = NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  output=list(
    "status_code"=httpResponse$status_code,
    "response"=httpResponse
  )

  return (output)
}
