#' List all widgets
#'
#' List all widgets
#'
#' @return List of widgets
#' @param NULL
#' @export
widget_list <- function(api_key=NULL,
                        api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  endpoint=paste0('widgets/')
  url=get_api_url(endpoint)

  httpResponse <- GET(url, add_headers("X-API-KEY" = api_key), accept_json(), verbose(get_verbose()))
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


