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
collection_add <- function(
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
    api_key=get_api_key();
  }

  url=get_api_url(endpoint)

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
                       verbose(get_verbose()))

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
collection_update <- function(
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
    api_key=get_api_key();
  }

  url=get_api_url(endpoint)

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
                       verbose(get_verbose()))

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


#' Get all collections
#'
#' Load a list of all collections or get info for a single collection
#'
#' @return List of studies or a single study info
#' @param repositoryid (Optional) Collection IDNo
#' @export
collections <- function(repositoryid=NULL, api_key=NULL, api_base_url=NULL){

  endpoint='collections/'

  if(!is.null(repositoryid)){
    endpoint=paste0(endpoint,'/',repositoryid)
  }

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  url=get_api_url(endpoint)
  print(url)
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
    class = "nada_collections"
  )
}

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
collection_rename <- function(
  old_idno,
  new_idno,
  api_key = NULL,
  api_base_url = NULL){

  endpoint='collections/rename'

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  url=get_api_url(endpoint)

  options = list(
    "old_repositoryid" = old_idno,
    "new_repositoryid" = new_idno
  )

  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key),
                       body=options,
                       verbose(get_verbose()))

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






#' Set owner and linked collections for studies
#'
#' @return NULL
#' @param study_idno (Required) Study/dataset IDNo
#' @param owner_collection (Optional) Owner Collection IDNo
#' @param link_collections List of collections to add to a study/dataset
#' @param mode (Required) Update or replace the linked collections. Values 'update' or 'replace'. Default is 'update'
#' @export
dataset_attach_collections <- function(
  study_idno,
  owner_collection=NULL,
  link_collections=list(),
  mode="update",
  api_key = NULL,
  api_base_url = NULL){

  endpoint='datasets/collections'

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  url=get_api_url(endpoint)

  options = list(
    "study_idno" = study_idno,
    "owner_collection" = owner_collection,
    "link_collections"=link_collections,
    "mode"=mode
  )

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
    class = "nada_datasets"
  )
}




#' Get owner and linked collections by studies
#'
#' @return NULL
#' @param study_idno (Optional) Study/dataset IDNo
#' @param offset (Optional) Offset for pagination
#' @param limit (Optional) Number of rows to return
#' @export
dataset_collections_list <- function(
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
    api_key=get_api_key();
  }

  url=get_api_url(endpoint)

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
collection_delete <- function(repositoryid, api_key=NULL, api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  url=get_api_url(paste0('collections/delete/', repositoryid))
  httpResponse <- DELETE(url, add_headers("X-API-KEY" = api_key), accept_json())
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
    stop(content(httpResponse, "text"), call. = FALSE)
  }

  output=fromJSON(content(httpResponse,"text"))
  return (output)
}

