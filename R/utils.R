#' Capture cover page of a PDF
#'
#' Capture cover page of a PDF file
#'
#' @return image file
#' @param file_path \strong{(required)} PDF file path
#' @param dpi set image DPI, default is 72 dpi
#' @export
capture_pdf_cover <- function(file_path, dpi=72) {

  if (!file.exists(file_path)){
    warning(paste("file not found:: ", file_path))
    return (NULL)
  }

  jpg <- gsub(".pdf", ".jpg", file_path)

  if(!file.exists(jpg)) {
    pdf_convert(file_path, format = "jpg", pages = 1, filenames = jpg, dpi = dpi,  verbose = FALSE)
  }

  return (jpg)
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
