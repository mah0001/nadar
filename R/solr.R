#' SOLR
#'
#' Solr index catalog entries
#'
#' @return NULL
#' @param start_row (required) row ID to start indexing from
#' @param limit Number of records to process
#'
#'
#'
#'
#' @export
nada_admin_solr_index_entries <- function(
                      start_row=0,
                      limit=10,
                      loop=FALSE,
                      api_key=NULL,
                      api_base_url=NULL
){

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  endpoint=paste0('solr/full_import_surveys/',start_row,'/',limit)
  url=nada_get_api_url(endpoint)

  httpResponse <- GET(url, add_headers("X-API-KEY" = api_key), accept_json(), verbose(nada_get_verbose()))
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  output=list(
    "status_code"=httpResponse$status_code,
    "response"= nada_http_response_json(httpResponse)
  )

  message(output$response)

  if (loop==TRUE && output$response$result$rows_processed>0){
    Sys.sleep(1)
    nada_admin_solr_index_entries(start_row=output$response$result$last_row_id,limit=limit,loop=loop)
  }

  return (output)
}


#' SOLR
#'
#' Solr index variables
#'
#' @return NULL
#' @param start_row (required) row ID to start indexing from
#' @param limit Number of records to process
#'
#'
#'
#'
#' @export
nada_admin_solr_index_variables <- function(
  start_row=0,
  limit=10,
  loop=FALSE,
  api_key=NULL,
  api_base_url=NULL
){

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  endpoint=paste0('solr/full_import_variables/',start_row,'/',limit)
  url=nada_get_api_url(endpoint)

  httpResponse <- GET(url, add_headers("X-API-KEY" = api_key), accept_json(), verbose(nada_get_verbose()))
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  output=list(
    "status_code"=httpResponse$status_code,
    "response"= nada_http_response_json(httpResponse)
  )


  message(paste0("\r\n========= ",output$response))

  if (loop==TRUE && output$response$result$rows_processed>0){
    Sys.sleep(1)
    nada_admin_solr_index_variables(start_row=output$response$result$last_row_id,limit=limit,loop=loop)
  }

  return (output)
}
