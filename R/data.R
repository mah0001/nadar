#' publishTable
#'
#' Create a data table with data using CSV
#'
#' @return NULL
#' @param db_id (Required) database name
#' @param table_id (Required) Table name
#' @param metadata Table metadata
#' @export
nada_admin_data_table_publish <- function(db_id, table_id, table_metadata, csvfile,overwrite="no", api_key=NULL, api_base_url=NULL, sleep_seconds=1) {

  #define table
  table_def=nada_admin_data_table_create(db_id=db_id,table_id=table_id,metadata=table_metadata, api_key=api_key, api_base_url=api_base_url)

  #upload csv file
  upload_result=nada_admin_data_table_upload_csv(db_id=db_id,table_id=table_id,file=csvfile, api_key=api_key, api_base_url=api_base_url)

  #import csv using new batch import process
  csv_import=nada_admin_data_table_import_csv(db_id=db_id,table_id=table_id, api_key=api_key, api_base_url=api_base_url, sleep_seconds=sleep_seconds)

  return (
    list(
      'table_def'=table_def,
      'upload_result'=upload_result,
      'csv_import'=csv_import
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
nada_admin_data_table_create <- function(
  db_id,
  table_id,
  metadata,
  api_key=NULL,
  api_base_url=NULL){

  endpoint=paste0('tables/create_table/',db_id,'/',table_id)

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  url=nada_get_api_url(endpoint)
  print(metadata)
  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key),
                       body = metadata,
                       content_type_json(),
                       encode="json",
                       accept_json(),
                       verbose(nada_get_verbose())
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
#' Import CSV using batch import process
#'
#' @return NULL
#' @param db_id (Required) database name
#' @param table_id (Required) Table name
#' @param max_rows Number of rows to be processed in a batch
#' @param delimiter CSV delimiter: comma, tab, semi-colon, colon (default: ",")
#' @param api_key API key (optional if API key is set using nada_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using nada_set_api_url)
#' @param max_batches Maximum number of batches to process (safety limit)
#' @param sleep_seconds Sleep time in seconds between batch calls (default: 1)
#' @export
nada_admin_data_table_import_csv <- function(
  db_id,
  table_id,
  max_rows=NULL,
  delimiter=",",
  api_key=NULL,
  api_base_url=NULL,
  max_batches=1000,
  sleep_seconds=1){

  # Process the file in batches
  cat("Starting batch import...\n")
  
  batch_count <- 0
  all_results <- list()
  max_batches <- max_batches  # Safety limit to prevent infinite loops
  
  # Initial batch import
  batch_result <- nada_admin_data_table_batch_import_csv(
    db_id = db_id,
    table_id = table_id,
    max_rows = max_rows,
    delimiter = delimiter,
    api_key = api_key,
    api_base_url = api_base_url
  )
  
  batch_count <- batch_count + 1
  all_results[[batch_count]] <- batch_result
  
  # Check for errors (both HTTP status and JSON content)
  if(batch_result$status_code != 200 || 
     (is.list(batch_result$response) && 
      "status" %in% names(batch_result$response) && 
      batch_result$response$status != "success")) {
    warning("Batch import failed")
    return(batch_result)
  }
  
  # Check if there's more data to process
  response_data <- batch_result$response
  has_more <- if(is.list(response_data) && "progress" %in% names(response_data)) response_data$progress$has_more else FALSE
  
  # Safety check: Ensure we have valid response structure
  if(!is.list(response_data) || !"progress" %in% names(response_data)) {
    warning("Invalid response structure - stopping import")
    return(batch_result)
  }
  
  # Continue processing batches until complete
  while(has_more && batch_count < max_batches) {
    # Add sleep between batch calls to avoid overwhelming the API
    if(sleep_seconds > 0) {
      Sys.sleep(sleep_seconds)
    }
    
    batch_result <- nada_admin_data_table_batch_import_csv(
      db_id = db_id,
      table_id = table_id,
      max_rows = max_rows,
      delimiter = delimiter,
      api_key = api_key,
      api_base_url = api_base_url
    )
    
    batch_count <- batch_count + 1
    all_results[[batch_count]] <- batch_result
    
    # Check for errors (both HTTP status and JSON content)
    if(batch_result$status_code != 200 || 
       (is.list(batch_result$response) && 
        "status" %in% names(batch_result$response) && 
        batch_result$response$status != "success")) {
      warning(paste("Batch", batch_count, "failed"))
      break
    }
    
    # Validate response structure
    if(!is.list(batch_result$response) || !"progress" %in% names(batch_result$response)) {
      warning(paste("Invalid response structure in batch", batch_count, "- stopping import"))
      break
    }
    
    response_data <- batch_result$response
    has_more <- response_data$progress$has_more
    
    # Additional safety check: if no progress is made, break
    if("total_rows_processed" %in% names(response_data$progress)) {
      current_total <- response_data$progress$total_rows_processed
      if(batch_count > 1 && current_total == all_results[[batch_count-1]]$response$progress$total_rows_processed) {
        warning("No progress made in batch - stopping import to prevent infinite loop")
        break
      }
    }
    
    # Progress update every 10 batches
    if(batch_count %% 10 == 0) {
      cat("Processed", batch_count, "batches...\n")
    }
  }
  
  # Check if we hit the safety limit
  if(batch_count >= max_batches) {
    warning(paste("Reached maximum batch limit of", max_batches, "- import may be incomplete"))
  }
  
  # Summary
  cat("\nImport completed!\n")
  cat("Total batches processed:", batch_count, "\n")
  
  if("progress" %in% names(response_data)) {
    cat("Total rows processed:", response_data$progress$total_rows_processed, "\n")
    if(response_data$progress$has_more) {
      cat("Warning: Import may be incomplete - more data available\n")
    }
  }
  
  # Return the last result with summary information
  final_result <- batch_result
  final_result$summary <- list(
    total_batches = batch_count,
    all_results = all_results,
    max_batches_reached = (batch_count >= max_batches),
    import_complete = !response_data$progress$has_more
  )
  
  return(final_result)
}







#' dataApiList
#'
#' List all tables
#'
#' @return NULL
#' @export
nada_data_table_list <- function(
  api_key=NULL,
  api_base_url=NULL){

  endpoint=paste0('tables/list/')

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  url=nada_get_api_url(endpoint)
  httpResponse <- GET(url, add_headers("X-API-KEY" = api_key),
                      content_type_json(),
                      encode="json",
                      accept_json(),
                      verbose(nada_get_verbose())
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
nada_admin_data_table_delete <- function(
  db_id,
  table_id,
  api_key=NULL,
  api_base_url=NULL){

  endpoint=paste0('tables/delete/',db_id,'/',table_id)

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  url=nada_get_api_url(endpoint)
  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key),verbose(nada_get_verbose()))

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







#' Upload CSV or ZIP File
#'
#' nada_admin_data_table_upload_csv
#'
#' Upload a CSV or ZIP file
#'
#' @return NULL
#' @param db_id (Required) database name
#' @param table_id (Required) Table name
#' @param file (Required) CSV or ZIP file path
#' @param api_key API key (optional if API key is set using nada_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using nada_set_api_url)
#'
#' @examples
#'
#' nada_admin_data_table_upload_csv (
#'   file = "path/to/data.csv"
#' )
#'
#' nada_admin_data_table_upload_csv (
#'   file = "path/to/data.zip"
#' )
#'
#' @export
nada_admin_data_table_upload_csv <- function(
  db_id,
  table_id,
  file,
  api_key=NULL,
  api_base_url=NULL){

  endpoint=paste0('tables/upload/',db_id,'/',table_id)

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  # Validate file extension
  file_ext <- tolower(tools::file_ext(file))
  if (!file_ext %in% c("csv", "zip")) {
    stop("File must be a CSV or ZIP file")
  }

  # Check if file exists
  if (!file.exists(file)) {
    stop(paste("File does not exist:", file))
  }

  url=nada_get_api_url(endpoint)

  file_options=list(
    'db_id' = db_id,
    'table_id'=table_id,
    'file'=upload_file(file)
  )

  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key),body=file_options, encode="multipart")
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  result<- tryCatch(
    {
      output=list(
        "status_code"=httpResponse$status_code,
        "response"=nada_http_response_json(httpResponse)
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


#' Batch Import CSV
#'
#' Import uploaded CSV or ZIP file into the database using the updated API endpoint
#'
#' @return NULL
#' @param db_id (Required) database name
#' @param table_id (Required) Table name
#' @param max_rows Number of rows to be processed in a batch
#' @param delimiter CSV delimiter: comma, tab, semi-colon, colon (default: "comma")
#' @param api_key API key (optional if API key is set using nada_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using nada_set_api_url)
#'
#' @examples
#'
#' nada_admin_data_table_batch_import_csv (
#'   db_id = "example",
#'   table_id = "prices"
#' )
#'
#' nada_admin_data_table_batch_import_csv (
#'   db_id = "example",
#'   table_id = "prices",
#'   max_rows = 5000,
#'   delimiter = "semi-colon"
#' )
#'
#' @export
nada_admin_data_table_batch_import_csv <- function(
  db_id,
  table_id,
  max_rows=NULL,
  delimiter=",",
  api_key=NULL,
  api_base_url=NULL){

  endpoint='tables/import'

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  url=nada_get_api_url(endpoint)

  import_options=list(
    'db_id' = db_id,
    'table_id' = table_id
  )

  # Add optional parameters if provided
  if(!is.null(max_rows)){
    import_options$max_rows <- max_rows
  }
  
  if(!is.null(delimiter)){
    import_options$delimiter <- delimiter
  }

  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key),
                       body = import_options,
                       content_type_json(),
                       encode="json",
                       accept_json(),
                       verbose(nada_get_verbose())
  )

  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  result<- tryCatch(
    {
      response_data <- nada_http_response_json(httpResponse)
      
      output=list(
        "status_code"=httpResponse$status_code,
        "response"=response_data
      )

      # Check for API errors in JSON response
      if(is.list(response_data) && "status" %in% names(response_data) && response_data$status != "success") {
        warning(paste("API Error:", if("message" %in% names(response_data)) response_data$message else "Unknown error"))
        return(output)
      }

      # Print progress information if available
      if(is.list(response_data) && "progress" %in% names(response_data)) {
        progress <- response_data$progress
        cat("Import Progress:\n")
        cat("  Total rows processed:", progress$total_rows_processed, "\n")
        cat("  Last processed row:", progress$last_processed_row, "\n")
        cat("  Status:", progress$import_status, "\n")
        cat("  Has more data:", progress$has_more, "\n")
        
        if("batch" %in% names(response_data)) {
          batch <- response_data$batch
          cat("  Rows in this batch:", batch$rows_processed, "\n")
          cat("  Execution time:", batch$execution_time_formatted, "\n")
        }
        
        if(progress$has_more && "next" %in% names(response_data)) {
          cat("  Next endpoint:", response_data[["next"]]$endpoint, "\n")
        }
      }

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





#' Attach Data API to a study
#'
#' Attach dataset available via API to a study
#'
#' @return NULL
#' @param db_id (required) DB ID
#' @param table_id (required) Table ID
#' @param idno (required) Study unique identifier
#' @param dataset_title (required) Dataset title
#' @param api_key API key (optional if API key is set using nada_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using nada_set_api_url)
#'
#' @examples
#'
#' nada_admin_study_attach (
#'   db_id="example",
#'   table_id="prices",
#'   idno="survey-idno-test",
#'   dataset_title = "Dataset title"
#' )
#'
#' @export
nada_admin_study_attach <- function(
					db_id,
					table_id,
          idno,
					dataset_title,
					api_key=NULL,
					api_base_url=NULL){

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  options=list(
    db_id=db_id,
	table_id=table_id,	
	idno=idno,
	title=dataset_title
  )

  # Create url
  endpoint <- paste0('tables/attach_to_study')
  if(is.null(api_base_url)){
    url=nada_get_api_url(endpoint=endpoint)
  } else {
    url = paste0(api_base_url,"/",endpoint)
  }

  httpResponse <- POST(url, 
                       add_headers("X-API-KEY" = api_key), 
                       body=options,
                       content_type_json(),
                       encode="json",
                       accept_json(),
                       verbose(nada_get_verbose()))

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

