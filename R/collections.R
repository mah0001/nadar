#' Returns a list of all collections in the catalog
#'
#' @export
list_collections <- function(api_key = NULL, api_base_url = NULL){
  if(is.null(api_key)){
    api_key = get_api_key();
  }

  url=get_api_url('collections')
  httpResponse <- GET(url,
                      add_headers("X-API-KEY" = api_key),
                      accept_json())
  output=NULL

  if(httpResponse$status_code != 200){
    warning(content(httpResponse, "text"))
    stop(content(httpResponse, "text"), call. = FALSE)
  }

  output <- fromJSON(content(httpResponse,"text"))
  return(output)
}

#' Returns information on a collection
#'
#' @param repository_id Collection's unique IDNo
#'
#' @export
get_collection_info <- function(repository_id, api_key = NULL, api_base_url = NULL){
  if(is.null(api_key)){
    api_key=get_api_key()
  }

  url = get_api_url(paste0('collections/', repository_id))
  httpResponse <- GET(url,
                      add_headers("X-API-KEY" = api_key),
                      accept_json())
  output=NULL

  if(httpResponse$status_code != 200){
    warning(content(httpResponse, "text"))
    stop(content(httpResponse, "text"), call. = FALSE)
  }

  output <- fromJSON(content(httpResponse, "text"))
  return(output)
}

#' Create new collection
#'
#' @param collectionid (required) Collection identifier containing numbers and letters only
#' @param title (required) Collection title
#' @param short_text (required) A short description for the collection
#' @param long_text (required) Detailed collection description. This field supports basic html and image tags.
#' @param thumbnail Thumbnail image - provide path/url
#' @param weight Provide weight to arrange display of collection
#' @param section (required) Section/Group. To see a list of sections available, see api endpoint collections/sections
#' @param ispublished (required) Set status to draft or published
#'
#' @export
create_collection <- function(
  collectionid,
  title,
  short_text,
  long_text,
  thumbnail=NULL,
  weight=0,
  section=2,
  ispublished=0,
  api_key=NULL,
  api_base_url=NULL){

  if(is.null(api_key)){
    api_key = get_api_key()
  }

  # Check whether all required arguments are provided

  options=list(
    "repositoryid" = collectionid,
    "title" = title,
    "short_text" = short_text,
    "long_text" = long_text,
    "weight" = weight,
    "section"= section,
    "ispublished" = ispublished
  )

  if (!is.null(thumbnail)){
    if (file.exists(thumbnail)){
      options[["thumbnail"]]=upload_file(thumbnail)
    }else{
      options[["thumbnail"]]=thumbnail
    }
  }

  url = get_api_url('collections')
  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body = options,
                       accept_json())
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


#' Rename a collection
#'
#' Change collection id of collection
#'
#' @param old_collectionid Old collection id
#' @param new_collectionid New collection id
#'
#' @export
rename_collection <- function(old_collectionid,
                              new_collectionid,
                              api_key = NULL,
                              api_base_url = NULL){
  if(is.null(api_key)){
    api_key = get_api_key()
  }

  options=list(
    "old_repositoryid" = old_collectionid,
    "new_repositoryid" = new_collectionid
  )

  url = get_api_url('collections/rename')
  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body = c(options),
                       content_type_json(),
                       encode = "json",
                       accept_json(),
                       verbose(get_api_verbose()))

  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }
}

#' Update collection
#'
#' Update collection by providing values for the fields that you want to update.
#' For updating the collection id, use the function rename_collection.
#'
#' @param collectionid (required) Collection identifier containing numbers and letters only
#' @param title (required) Collection title
#' @param short_text (required) A short description for the collection
#' @param long_text (required) Detailed collection description. This field supports basic html and image tags.
#' @param thumbnail Thumbnail image - provide path/url
#' @param weight Provide weight to arrange display of collection
#' @param section (required) Section/Group. To see a list of sections available, see api endpoint collections/sections
#' @param ispublished (required) Set status to draft or published
#'
#'@export
#'
# update_collection <- function()
# list_collections()
# get_collection_info(repository_id = "Ag")
# create_collection(collectionid = "test123_thumb",
#                   title = "Test123 collection title",
#                   short_text ="Test123 Short test description" ,
#                   long_text = "Test123 Long test description",
#                   ispublished = 1,
#                   thumbnail = thumb
# )
# thumbnail_upload(idno=idno,thumbnail = thumb)
# rename_collection(old_collectionid = "TEST123_THUMB",
#                   new_collectionid = "TEST123_THUMB_rename")
