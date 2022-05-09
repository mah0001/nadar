#' Capture cover page of a PDF
#'
#' Capture cover page of a PDF file
#'
#' @return image file
#' @param file_path \strong{(required)} PDF file path
#' @param file_name_jpg \strong{(optional)} file name to store jpg. If empty, file name of jpg is same as pdf
#' @param file_dir_jpg \strong{(optional)} file directory to store jpg. If empty, file will be stored in same folder as pdf
#' @param dpi set image resolution in dpi, default is 72 dpi
#' @export
capture_pdf_cover <- function(file_path, file_name_jpg = NULL,
                              file_dir_jpg = NULL, dpi = 72) {

  if (!file.exists(file_path)){
    warning(paste("File not found: ", file_path))
    return (NULL)
  }

  # determine file name and directory for jpg
  if(is.null(file_name_jpg) & is.null(file_dir_jpg)){ # same name and directory as pdf
    jpg_path <- gsub(".pdf", ".jpg", file_path)
  } else { # change directory and/or file name
    pdf_directory <- dirname(file_path)
    pdf_name      <- basename(file_path)

    if(!is.null(file_name_jpg)){ # new file name
      if(!grepl(".jpg", file_name_jpg, fixed = TRUE)){
        file_name_jpg <- paste0(file_name_jpg, ".jpg")
      }
    } else {
      file_name_jpg <- gsub(".pdf", ".jpg", pdf_name)  # same as pdf name
    }

    if(!is.null(file_dir_jpg)){ # new directory
      if (!dir.exists(file_dir_jpg)){
        warning(paste("Dir does not exist: ", file_dir_jpg))
        return (NULL)
      }
    } else {
      file_dir_jpg <- pdf_directory # same as pdf directory
    }

    jpg_path <- paste0(file_dir_jpg, "/", file_name_jpg)
  }

  # Convert pdf to jpg and save jpg
  if(!file.exists(jpg_path)) {
    pdf_convert(file_path,
                format = "jpg",
                pages = 1,
                filenames = jpg_path,
                dpi = dpi,
                verbose = FALSE)
  }

  return(jpg_path)
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
  print(url)
  httpResponse <- GET(url, add_headers("X-API-KEY" = api_key), body=options, verbose(get_verbose()) )

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
      warning(paste0("ERROR processing response:: ", content(httpResponse,"text")))
      return (content(httpResponse,"text"))
    }
  )

  return (result)
}
