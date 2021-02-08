#' Publish table to Mongo DB
#'
#' Publish a table to Mongo DB
#'
#' @param tbl_id Table id
#' @param db_id  Database name
#' @param csv_file CSV file or zip file
#' @param tbl_metadata Table metadata
#' @param overwrite Overwrite if an entry with the same ID already exists? Valid values "yes", "no"
#' @examples
#' \dontrun{
#' publish_table_to_MongoDB()
#' }
publish_table_to_MongoDB <- function(tbl_id, db_id, csv_file, tbl_metadata, overwrite = "no"){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  # Create table
  url = get_api_url(paste0("tables/create_table/", db_id, "/", tbl_id))
  httpResponse1 <- POST(url,
                        add_headers("X-API-KEY" = api_key),
                        body=tbl_metadata,
                        encode="json",
                        accept_json(),
                        verbose(get_verbose()))

  if(httpResponse$status_code!=200){
    warning(paste0(" Failed - ",content(httpResponse, "text")))
  }else{
    print(content(httpResponse,"text"))
    print("Success")
  }

  # Upload csv/zip file
  csv_options=list('table_id'=tbl_id,
                   'file'=upload_file(csv_file),
                   'overwrite'=overwrite
  )

  url = get_api_url(paste0("tables/upload/", db_id, "/", tbl_id))
  httpResponse2 <- POST(url,
                        add_headers("X-API-KEY" = api_key),
                        body=csv_options)

  output=NULL

  if(httpResponse$status_code!=200){
    warning(paste0(" Failed - ",content(httpResponse, "text")))
  }else{
    print(content(httpResponse,"text"))
    print("Success")
  }
}
