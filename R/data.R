#' publishTable
#'
#' Create a data table with data using CSV
#'
#' @return NULL
#' @param db_id (Required) database name
#' @param table_id (Required) Table name
#' @param metadata Table metadata
#' @export
data_api_publish_table <- function(db_id, table_id, table_metadata, csvfile,overwrite="no", api_key=NULL, api_base_url=NULL) {

  #define table
  table_def=data_api_create_table(db_id=db_id,table_id=table_id,metadata=table_metadata, api_key=api_key, api_base_url,api_base_url)

  #import csv
  csv_import=data_api_import_csv(db_id=db_id,table_id=table_id,csvfile=csvfile,overwrite=overwrite, api_key=api_key, api_base_url,api_base_url)

  return (
    list(
      'table_def'<-table_def,
      'csv_import'<-csv_import
    )
  )
}

#' createTable
#'
#' Create a table
#'
#' @return NULL
#' @param db_id (Required) database name
#' @param table_id (Required) Table name
#' @param metadata Table metadata
#' @export
data_api_create_table <- function(
  db_id,
  table_id,
  metadata,
  api_key=NULL,
  api_base_url=NULL){

  endpoint=paste0('tables/create_table/',db_id,'/',table_id)

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  url=get_api_url(endpoint)
  print(metadata)
  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key),
                       body = metadata,
                       content_type_json(),
                       encode="json",
                       accept_json(),
                       verbose(get_verbose())
  )

  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  result<- tryCatch(
    {
      output=list(
        "status_code"=httpResponse$status_code,
        "response"=fromJSON(content(httpResponse,"text"))
      )

      return (output)
    },
    error= function(cond) {
      message("ERROR processing response")
      message(cond)
      message(content(httpResponse,"text"))
    }
  )

  return (result)
}



#' dataApiImportCSV
#'
#' Import CSV
#'
#' @return NULL
#' @param db_id (Required) database name
#' @param table_id (Required) Table name
#' @param csvfile CSV file
#' @param overwrite (valid values = "yes" or "no")
#' @export
data_api_import_csv <- function(
  db_id,
  table_id,
  csvfile,
  overwrite="no",
  api_key=NULL,
  api_base_url=NULL){

  endpoint=paste0('tables/upload/',db_id,'/',table_id)

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  url=get_api_url(endpoint)

  csv_options=list(
    'db_id' = db_id,
    'table_id'=table_id,
    'file'=upload_file(csvfile),
    'overwrite'=overwrite
  )

  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key),body=csv_options)
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  result<- tryCatch(
    {
      output=list(
        "status_code"=httpResponse$status_code,
        "response"=fromJSON(content(httpResponse,"text"))
      )

      return (output)
    },
    error= function(cond) {
      message(paste0("ERROR processing response:: ", url))
      message(cond)
      return (content(httpResponse,"text"))
    }
  )

  return (result)
}







#' dataApiList
#'
#' List all tables
#'
#' @return NULL
#' @export
data_api_list_tables <- function(
  api_key=NULL,
  api_base_url=NULL){

  endpoint=paste0('tables/list/')

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  url=get_api_url(endpoint)
  httpResponse <- GET(url, add_headers("X-API-KEY" = api_key),
                      content_type_json(),
                      encode="json",
                      accept_json(),
                      verbose(get_verbose())
  )

  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  result<- tryCatch(
    {
      output=list(
        "status_code"=httpResponse$status_code,
        "response"=fromJSON(content(httpResponse,"text"))
      )

      return (output)
    },
    error= function(cond) {
      message("ERROR processing response")
      message(cond)
      message(content(httpResponse,"text"))
    }
  )

  return (result)
}








#' deleteTable
#'
#' delete a table
#'
#' @return NULL
#' @param db_id (Required) database name
#' @param table_id (Required) Table name
#' @export
data_api_delete_table <- function(
  db_id,
  table_id,
  api_key=NULL,
  api_base_url=NULL){

  endpoint=paste0('tables/delete/',db_id,'/',table_id)

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  url=get_api_url(endpoint)
  httpResponse <- DELETE(url, add_headers("X-API-KEY" = api_key),verbose(get_verbose()))

  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  result<- tryCatch(
    {
      output=list(
        "status_code"=httpResponse$status_code,
        "response"=fromJSON(content(httpResponse,"text"))
      )

      return (output)
    },
    error= function(cond) {
      message("ERROR processing response")
      message(cond)
      message(content(httpResponse,"text"))
    }
  )

  return (result)
}
