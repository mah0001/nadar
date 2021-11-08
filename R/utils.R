#' Capture cover page of a PDF
#'
#' Capture cover page of a PDF file
#'
#' @return image file
#' @param file_path \strong{(required)} PDF file path
#' @param file_name_jpg \strong{(optional)} file name to store jpg. If empty, file name of jpg is same as pdf
#' @param dpi set image DPI, default is 72 dpi
#' @export
capture_pdf_cover <- function(file_path, file_name_jpg = NULL, dpi = 72) {

  if (!file.exists(file_path)){
    warning(paste("file not found:: ", file_path))
    return (NULL)
  }

  if(is.null(file_name_jpg)){
    jpg <- gsub(".pdf", ".jpg", file_path)
  } else {
    if(!grepl(".jpg", file_name_jpg, fixed = TRUE)){
      file_name_jpg <- paste0(file_name_jpg, ".jpg")
    }
    jpg <- paste0(dirname(file_path), "/", file_name_jpg)
  }

  if(!file.exists(jpg)) {
    pdf_convert(file_path,
                format = "jpg",
                pages = 1,
                filenames = jpg,
                dpi = dpi,
                verbose = FALSE)
  }

  return(jpg)
}

#' Check string is a URL
#'
#' @param url
#'
#' @return TRUE or FALSE
#'
#' @export
is_valid_url <- function(url){

  if(startsWith(url, 'http://')  ||
     startsWith(url, 'https://') ||
     startsWith(url, 'ftp://')   ||
     startsWith(url, 'sftp://')
  ){
      return (TRUE)
  }

    return (FALSE)
}


#' Make an http GET request
#'
#' @param url
#'
#' @return http request output
#'
#' @export
nada_http_get <- function(
  url,
  options=list(),
  api_key=NULL,
  api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  url=get_api_url(paste0(url))
  httpResponse <- GET(url, add_headers("X-API-KEY" = api_key), body=options)

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


#' Make an http POST request
#'
#' @param url
#'
#' @return http request output
#'
#' @export
nada_http_post <- function(
  url,
  options=list(),
  api_key=NULL,
  api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  url=get_api_url(paste0(url))
  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key), body=options)

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  return (httpResponse)
}


#' Make an http PUT request
#'
#' @param url
#'
#' @return http request output
#'
#'
#' request_encode = c("multipart", "form", "json", "raw")
#'
#' @export
nada_http_put <- function(
  url,
  options=list(),
  request_encode="json",
  api_key=NULL,
  api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  url=get_api_url(paste0(url))
  httpResponse <- PUT(url, add_headers("X-API-KEY" = api_key), body=options, encode=request_encode)

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  return (httpResponse)
}



#' Make an http DELETE request
#'
#' @param url
#'
#' @return http request output
#'
#' @export
nada_http_delete <- function(
  url,
  options=list(),
  api_key=NULL,
  api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  url=get_api_url(paste0(url))
  httpResponse <- DELETE(url, add_headers("X-API-KEY" = api_key), body=options)

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  return (httpResponse)
}


nada_http_response_json <- function(httpResponse)
{
  result<- tryCatch(
    {
      return (fromJSON(content(httpResponse,"text")))
    },
    error= function(cond) {
      message(paste0("ERROR processing response:: ", cond))
      return (content(httpResponse,"text"))
    }
  )

  return (result)
}
