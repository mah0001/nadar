#' Create new collection
#'
#' @return NULL
#' @param repositoryid (Required) new collection ID
#' @param title Title of the collection
#' @param short_text Short description of the collection
#' @param long_text Long description of the collection
#' @param thumbnail Thumbnail file path
#' @param weight Weight
#' @param section Section
#' @param ispublished Set collection status - 1 = Published, 0 = Unpublished
#' @export
nada_admin_collection_create <- function(
  repositoryid = NULL,
  title = NULL,
  short_text = NULL,
  long_text = NULL,
  thumbnail = NULL,
  weight = 0,
  section = 0,
  ispublished = 0,
  api_key = NULL,
  api_base_url = NULL){

  endpoint='collections'

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  url=nada_get_api_url(endpoint)

  options = list(
    "repositoryid" = repositoryid,
    "title" = title,
    "short_text" = short_text,
    "long_text" = long_text,
    "weight" = weight,
    "section" = section,
    "ispublished" = ispublished
  )


  if (!is.null(thumbnail) && file.exists(thumbnail)){
    options[["thumbnail"]] = upload_file(thumbnail)
  }

  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key),
                       body=options,
                       verbose(nada_get_verbose()))

  output = NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  output=list(
    "status_code"=httpResponse$status_code,
    "response"=httpResponse
  )

  return (output)
}

#' update collection
#'
#' Update collection options - all fields are optional
#'
#' @return NULL
#' @param repositoryid (Required) Collection ID to be updated
#' @param title Title of the collection
#' @param short_text Short description of the collection
#' @param long_text Long description of the collection
#' @param thumbnail Thumbnail file path
#' @param weight Weight
#' @param section Section
#' @param ispublished Set collection status - 1 = Published, 0 = Unpublished
#' @export
nada_admin_collection_update <- function(
  repositoryid = NULL,
  title = NULL,
  short_text = NULL,
  long_text = NULL,
  thumbnail = NULL,
  weight = NULL,
  section = NULL,
  ispublished = NULL,
  api_key = NULL,
  api_base_url = NULL){

  endpoint='collections/update'

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  url=nada_get_api_url(endpoint)

  tmp = list(
    "repositoryid" = repositoryid,
    "title" = title,
    "short_text" = short_text,
    "long_text" = long_text,
    "weight" = weight,
    "section" = section,
    "ispublished" = ispublished
  )

  #remove null fields
  options=list()
  for (col in names(tmp)){
    if (!is.null(tmp[[col]])) {
      options[[col]]=tmp[[col]]
    }
  }

  if (!is.null(thumbnail) && file.exists(thumbnail)){
    options[["thumbnail"]] = upload_file(thumbnail)
  }

  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key),
                       body=options,
                       verbose(nada_get_verbose()))

  output = NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  output=list(
    "status_code"=httpResponse$status_code,
    "response"=httpResponse
  )

  return (output)
}


#' Get collections detail
#'
#' Provides functions to list available collections and retrieve detailed information
#' about a specific collection.
#'
#' @param api_key API key (optional if API key is set using nada_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using nada_set_api_url)
#' @param as_data_table Logical. If `TRUE`, converts the result to `data.table`. Defaults to `TRUE`.
#'
#' @return A `data.table`, a list (parsed JSON), or datatable depending on `as_data_table`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   collection <- nada_collection_list()
#'   print(collection)
#' }
nada_collection_list <- function(
    api_key = NULL,
    api_base_url = NULL,
    as_data_table = TRUE
) {

  # Construct API endpoint
  endpoint <- paste0("catalog/collections")

  if (is.null(api_base_url)) {
    url <- nada_get_api_url(endpoint = endpoint)
  } else {
    url <- paste0(api_base_url, endpoint)
  }

  # Send GET request
  httpResponse <- GET(url,
                      accept_json(),
                      add_headers("X-API-KEY" = api_key),
                      verbose(nada_get_verbose())
  )

  # Check for HTTP errors
  if (httr::http_error(httpResponse)) {

    cli::cli_abort(c(
      "x" = "HTTP error {.code {httr::status_code(httpResponse)}}",
      "!" = httr::content(httpResponse, "text")
    ))

  }

  # Parse content
  parsed <- httr::content(httpResponse, "parsed", type = "application/json")

  if (as_data_table) {

    dt <- data.table::rbindlist(
      lapply(parsed$collections, function(x) {
        data.table::data.table(
          id = x$id,
          repositoryid = x$repositoryid,
          title = x$title,
          file_name = x$file_name,
          thumbnail = x$thumbnail,
          short_text = x$short_text,
          long_text = x$long_text
        )
      }),
      fill = TRUE
    )

    return(dt)

  } else {
    return(parsed)
  }
}

#' @describeIn nada_collection_list Fetches details of a single collection by its repository ID.
#'
#' @param repoid Character. Repository or collection ID.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   collection <- nada_single_collection(repoid = "dhs")
#'   print(collection)
#' }
nada_single_collection <- function(
    api_key = NULL,
    api_base_url = NULL,
    repoid = NULL,
    as_data_table = TRUE
) {

  # repository/ collection id
  if (is.null(repoid)) {
    cli::cli_abort("Repository or collection ID is not provided")
  }

  # Construct API endpoint
  endpoint <- paste0("catalog/collections/", repoid)

  if (is.null(api_base_url)) {
    url <- nada_get_api_url(endpoint = endpoint)
  } else {
    url <- paste0(api_base_url, endpoint)
  }

  # Send GET request
  httpResponse <- GET(url,
                      accept_json(),
                      add_headers("X-API-KEY" = api_key),
                      verbose(nada_get_verbose())
  )

  # Check for HTTP errors
  if (httr::http_error(httpResponse)) {

    cli::cli_abort(c(
      "x" = "HTTP error {.code {httr::status_code(httpResponse)}}",
      "!" = httr::content(httpResponse, "text")
    ))

  }

  # Parse content
  parsed <- httr::content(httpResponse, "parsed", type = "application/json")

  if (as_data_table) {

    dt <- data.table::as.data.table(parsed)
    return(dt)

  } else {
    return(parsed)
  }
}

#' #' Get all collections
#' #'
#' #' Load a list of all nada_collection_list or get info for a single collection
#' #'
#' #' @return List of studies or a single study info
#' #' @param repositoryid (Optional) Collection IDNo
#' #' @export
#' nada_collection_list <- function(repositoryid=NULL, api_key=NULL, api_base_url=NULL){
#'
#'   endpoint='collections/'
#'
#'   if(!is.null(repositoryid)){
#'     endpoint=paste0(endpoint,'/',repositoryid)
#'   }
#'
#'   if(is.null(api_key)){
#'     api_key=nada_get_api_key();
#'   }
#'
#'   url=nada_get_api_url(endpoint)
#'   print(url)
#'   httpResponse <- GET(url, add_headers("X-API-KEY" = api_key), accept_json())
#'   output=NULL
#'
#'   if(httpResponse$status_code!=200){
#'     warning(content(httpResponse, "text"))
#'     stop(content(httpResponse, "text"), call. = FALSE)
#'   }
#'
#'   output=fromJSON(content(httpResponse,"text"))
#'   #return (output)
#'
#'   structure(
#'     list(
#'       content = output,
#'       api_url = url,
#'       status_code = httpResponse$status_code
#'     ),
#'     class = "nada_collections"
#'   )
#' }

options = list(
  "repositoryid" = "repositoryid",
  "title" = NULL,
  "short_text" = NULL,
  "long_text" = "long_text",
  "weight" = ""
)

data=list()
for (col in names(options)){
  if (!is.null(options[[col]])) {
    data[[col]]=options[[col]]
  }
}



#' Rename collection
#'
#' @return NULL
#' @param old_idno (Required) old collection IDNO
#' @param new_idno (Required) new collection IDNO
#' @export
nada_admin_collection_rename <- function(
  old_idno,
  new_idno,
  api_key = NULL,
  api_base_url = NULL){

  endpoint='collections/rename'

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  url=nada_get_api_url(endpoint)

  options = list(
    "old_repositoryid" = old_idno,
    "new_repositoryid" = new_idno
  )

  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key),
                       body=options,
                       verbose(nada_get_verbose()))

  output = NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  output=list(
    "status_code"=httpResponse$status_code,
    "response"=httpResponse
  )

  return (output)
}






#' Set owner and linked nada_collection_list for studies
#'
#' @return NULL
#' @param study_idno (Required) Study/dataset IDNo
#' @param owner_collection (Optional) Owner Collection IDNo
#' @param link_collections List of nada_collection_list to add to a study/dataset
#' @param mode (Required) Update or replace the linked nada_collection_list. Values 'update' or 'replace'. Default is 'update'
#' @export
nada_admin_study_attach_collections <- function(
  study_idno,
  owner_collection=NULL,
  link_collections=list(),
  mode="update",
  api_key = NULL,
  api_base_url = NULL){

  endpoint='datasets/collections'

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  url=nada_get_api_url(endpoint)

  print(url)

  options = list(
    "study_idno" = study_idno,
    "owner_collection" = owner_collection,
    "link_collections"=link_collections,
    "mode"=mode
  )

  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key),
                       body=options,
                       content_type_json(),
                       encode="json",
                       verbose(nada_get_verbose()))

  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
    stop(content(httpResponse, "text"), call. = FALSE)
  }

  output=fromJSON(content(httpResponse,"text"))

  structure(
    list(
      content = output,
      api_url = url,
      status_code = httpResponse$status_code
    ),
    class = "nada_datasets"
  )
}




#' Get owner and linked nada_collection_list by studies
#'
#' @return NULL
#' @param study_idno (Optional) Study/dataset IDNo
#' @param offset (Optional) Offset for pagination
#' @param limit (Optional) Number of rows to return
#' @export
nada_admin_study_get_collections <- function(
  study_idno=NULL,
  offset=0,
  limit=1000,
  api_key = NULL,
  api_base_url = NULL){

  endpoint='datasets/collections'

  if(!is.null(study_idno)){
    endpoint=paste0(endpoint,'/',study_idno)
  }

  if(!is.null(offset)){
    endpoint=paste0(endpoint,'?offset=',offset, '&limit=',limit)
  }


  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  url=nada_get_api_url(endpoint)

  httpResponse <- GET(url, add_headers("X-API-KEY" = api_key), accept_json())
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
    stop(content(httpResponse, "text"), call. = FALSE)
  }

  output=fromJSON(content(httpResponse,"text"))

  structure(
    list(
      content = output,
      api_url = url,
      status_code = httpResponse$status_code
    ),
    class = "dataset_collections"
  )
}




#' Delete a collection
#'
#' Delete a collection
#'
#' @return status
#' @param repositoryid (Required) Collection IDNo
#'
#' @export
nada_admin_collection_delete <- function(repositoryid, api_key=NULL, api_base_url=NULL){

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  url=nada_get_api_url(paste0('collections/delete/', repositoryid))
  httpResponse <- DELETE(url, add_headers("X-API-KEY" = api_key), accept_json())
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
    stop(content(httpResponse, "text"), call. = FALSE)
  }

  output=fromJSON(content(httpResponse,"text"))
  return (output)
}

