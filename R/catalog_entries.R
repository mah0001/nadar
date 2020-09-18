#' List catalog entries
#'
#' List all entries in the catalog
#'
#' @return List of studies or a single study info
#' @param idno (Optional) Dataset IDNo
#' @export
catalog_list <- function(idno=NULL, api_key=NULL, api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  if(!is.null(idno)){
    endpoint=paste0(endpoint,'/',idno)
  }

  url=get_api_url('datasets')
  httpResponse <- GET(url, add_headers("X-API-KEY" = api_key), accept_json())
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
    stop(content(httpResponse, "text"), call. = FALSE)
  }

  output=fromJSON(content(httpResponse,"text"))
  #return (output)

  structure(
    list(
      content = output,
      api_url = url,
      status_code = httpResponse$status_code
    ),
    class = "nada_datasets"
  )
}


#' Delete a catalog study
#'
#' Delete a single entry from the catalog
#'
#' @return list
#' @param idno (Required) Dataset IDNo
#'
#' @export
catalog_delete <- function(idno, api_key=NULL, api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  url=get_api_url(paste0('datasets/', idno))
  httpResponse <- DELETE(url, add_headers("X-API-KEY" = api_key), accept_json())
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
    stop(content(httpResponse, "text"), call. = FALSE)
  }

  output=fromJSON(content(httpResponse,"text"))
  return (output)
}


#' Search catalog
#'
#' Search catalog
#'
#' @return list
#'
#' @export
catalog_search <- function(options=list()){
  return ("TODO")
}


#' Find a study by IDNO
#'
#' Find study by IDNO
#'
#' @return list
#'
#' @export
catalog_find_by_idno <- function(idno){
  return ("TODO")
}

#' @export
catalog_find_by_id <- function(id){
  return ("TODO")
}


