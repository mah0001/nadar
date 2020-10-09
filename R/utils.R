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
