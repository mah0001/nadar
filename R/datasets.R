#' GetDatasets
#'
#' Load a list of all studies/datasets or get info for a single dataset/study
#'
#' @return List of studies or a single study info
#' @param idno (Optional) Dataset IDNo
#' @param api_key API key (optional if API key is set using nada_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using nada_set_api_url)
#' @param offset Specify the number of rows to skip, default is 0
#' @param limit Specify number of rows to return, default is 50 rows. Note: if more than 500 rows are requested, several API calls are made and results are combined..
#' @export
#'
nada_admin_study_list <- function(idno=NULL,
                     offset=0,
                     limit=50,
                     api_key=NULL,
                     api_base_url=NULL){

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  # if only one dataset is requested
  if(!is.null(idno)){
    endpoint=paste0('datasets/','/',idno)
  } else { # if list of studies/datasets is requested
    if(limit < 500){ # up to 500 one API call
      endpoint <- paste0("datasets/", "?offset=", offset, "&limit=", limit)
    }else { # if more than 500 requested, multiple API calls
      endpoint=paste0("datasets/", "?offset=", offset, "&limit=500")
    }
  }

  # Create url
  if(is.null(api_base_url)){
    url <- nada_get_api_url(endpoint = endpoint)
  } else {
    url <- paste0(api_base_url,"/",endpoint)
  }

  httpResponse <- GET(url, add_headers("X-API-KEY" = api_key), accept_json(), verbose(nada_get_verbose()))
  output <- NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
    stop(content(httpResponse, "text"), call. = FALSE)
  }

  output <- fromJSON(content(httpResponse,"text"))

  # add more API calls if limit > 500
  print(limit)
  if(limit > 500){
    cur_datasets <- output$nada_study_list # adding result nada_study_list for each call
    num_entries_to_add <- min(limit, output$total) - 500 # number of entries to add (max of limit and available entries)

    df_column_names <- colnames(cur_datasets)

    while(output$found > 0 & num_entries_to_add > 0){ # while more entires to add
      offset <- offset + 500 # update offset
      endpoint <- paste0("datasets/", "?offset=", offset, "&limit=500")

      # Create URL
      if(is.null(api_base_url)){
        url <- nada_get_api_url(endpoint = endpoint)
      } else {
        url <- paste0(api_base_url,"/",endpoint)
      }

      # API call
      httpResponse <- GET(url, add_headers("X-API-KEY" = api_key), accept_json(), verbose(nada_get_verbose()))

      if(httpResponse$status_code!=200){
        warning(content(httpResponse, "text"))
        stop(content(httpResponse, "text"), call. = FALSE)
      }

      output <- fromJSON(content(httpResponse,"text"))

      if (output$found==0){
        warning("exiting....")
        break;
      }

      output_ds <- subset(output$nada_study_list, select = df_column_names)
      cur_datasets <- rbind(cur_datasets, output_ds) # combine results
      num_entries_to_add <- num_entries_to_add - 500 # update number of entries to add
    }

    output$nada_study_list <- cur_datasets
  }

  structure(
    list(
      content = output,
      api_url = url,
      status_code = httpResponse$status_code
    ),
    class = "nada_datasets"
  )
}



#' ImportDDI
#'
#' Import a DDI file
#'
#' @return NULL
#' @param api_key API key (optional if API key is set using nada_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using nada_set_api_url)
#' @param xml_file (Required) DDI/XML file path
#' @param repositoryid Collection ID that owns the study
#' @param overwrite Overwrite if a study with the same ID already exists? Valid values "yes", "no"
#' @param access_policy Select the access policy suitable for your data. Valid values - "open" "direct" "public" "licensed" "enclave" "remote" "other"
#' @param data_remote_url Link to the website where the data is available. Required if access_policy is set to 'remote'.
#' @param rdf_file RDF file path
#' @param published Set status for study - 0 = Draft, 1 = Published
#' @param verbose Show verbose output - True, False
#' @export
nada_admin_study_import_ddi <- function(api_key=NULL,
                       api_base_url=NULL,
                      xml_file=NULL,
                      rdf_file=NULL,
                      repositoryid=NULL,
                      overwrite='no',
                      access_policy=NULL,
                      data_remote_url=NULL,
                      published=NULL){


  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  # Create url
  endpoint='datasets/import_ddi'
  if(is.null(api_base_url)){
    url=nada_get_api_url(endpoint=endpoint)
  } else {
    url = paste0(api_base_url,"/",endpoint)
  }

  options=list(
    "file"=upload_file(xml_file),
    "overwrite"=overwrite,
    "published"=published,
    "repositoryid"=repositoryid,
    "access_policy"=access_policy,
    "data_remote_url"=data_remote_url
  )

  if (!is.null(rdf_file) && file.exists(rdf_file)){
    options[["rdf"]]=upload_file(rdf_file)
  }

  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key),body=options, accept_json(), verbose(nada_get_verbose()))

  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  output=list(
    "status_code"=httpResponse$status_code,
    "response"=nada_http_response_json(httpResponse)
  )

  return (output)
}

#' Create new study
#'
#' Create a new study
#'
#' @return NULL
#' @param type (required) Type of study - survey, geospatial, table, document, timeseries
#' @param idno (required) Study unique identifier
#' @param repositoryid Collection ID that owns the study
#' @param access_policy Select the access policy suitable for your data. Valid values - "open" "direct" "public" "licensed" "enclave" "remote" "other"
#' @param data_remote_url Link to the website where the data is available. Required if access_policy is set to 'remote'.
#' @param published Set status for study - 0 = Draft, 1 = Published
#' @param overwrite Overwrite if a study with the same ID already exists? Valid values "yes", "no"
#' @param metadata \strong{(required)} Metadata list depending on the type of study
#' @param api_key API key (optional if API key is set using nada_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using nada_set_api_url)
#'
#' @examples
#'
#' metadata=list(
#' doc_desc=list(
#' "idno"="doc-idno",
#' "producers"=list(
#' list(
#'     "name"="name here",
#'     "abbr"="abbreviation"
#'   )
#' )
#' ),
#'
#' study_desc=list(
#'   "title_statement"= list(
#'     "idno"= "survey-idno-test",
#'     "title"= "string",
#'     "sub_title"= "string",
#'     "alternate_title"= "string",
#'     "translated_title"= "string"
#'   ),
#'   "study_info"=list(
#'     "nation"=list(
#'       list(
#'         "name"="Test",
#'         "abbreviation"="tst")
#'     )
#'   )
#' )
#' )
#'
#' nada_admin_study_create (
#'   idno="survey-idno-test",
#'   type="survey",
#'   published = 1,
#'   overwrite = "yes",
#'   metadata = metadata,
#'   additional = NULL
#' )
#'
#'
#'
#'
#' @export
nada_admin_study_create <- function(
                   type,
                   idno,
                   metadata,
                   repositoryid=NULL,
                   access_policy=NULL,
                   data_remote_url=NULL,
                   published=NULL,
                   overwrite=NULL,
                   thumbnail=NULL,
                   api_key=NULL,
                   api_base_url=NULL){

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  options=list(
    "idno"=idno,
    "repositoryid"=repositoryid,
    "access_policy"=access_policy,
    "data_remote_url"=data_remote_url,
    "published"=published,
    "overwrite"=overwrite
  )

  options= c(options,metadata)

  # Create url
  endpoint <- paste0('datasets/create/',type,'/',idno)
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

  thumbnail_result=NULL

  #upload thumbnail
  if(!is.null(thumbnail) && file.exists(thumbnail)) {
    thumbnail_result=nada_admin_thumbnail_upload(idno=idno,thumbnail = thumbnail)
  }

  #set default thumbnail
  if(!is.null(thumbnail) && thumbnail == 'default'){
    thumbnail_result= nada_admin_thumbnail_delete(idno=idno)
  }

  output=list(
    "status_code"=httpResponse$status_code,
    "response"= nada_http_response_json(httpResponse),
    "thumbnail"=thumbnail_result
  )

  return (output)
}

#' Upload thumbnail for a study
#'
#' Upload thumbnail for a study
#'
#' @return NULL
#' @param idno (required) Study unique identifier
#' @param thumbnail \strong{(required)} Path to the thumbnail file
#' @param api_key API key (optional if API key is set using nada_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using nada_set_api_url)
#'
#' @examples
#'
#' nada_admin_thumbnail_upload (
#'   idno="survey-idno-test",
#'   thumbnail = "/thumbnails/thumbnail-idno-test.png"
#' )
#'
#' @export
nada_admin_thumbnail_upload <- function(
                   idno,
                   thumbnail,
                   api_key=NULL,
                   api_base_url=NULL){

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  options=list(
    file=upload_file(thumbnail)
  )

  # Create url
  endpoint <- paste0('datasets/thumbnail/',idno)
  if(is.null(api_base_url)){
    url=nada_get_api_url(endpoint=endpoint)
  } else {
    url = paste0(api_base_url,"/",endpoint)
  }

  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key), body=options, verbose(nada_get_verbose()))

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

#' Delete thumbnail for a study
#'
#' Delete thumbnail for a study
#'
#' @return NULL
#' @param idno (required) Study unique identifier
#' @param api_key API key (optional if API key is set using nada_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using nada_set_api_url)
#'
#' @examples
#'
#' nada_admin_thumbnail_delete (
#'   idno="survey-idno-test"
#' )
#'
#' @export
nada_admin_thumbnail_delete <- function(idno,
                             api_key=NULL,
                             api_base_url=NULL){

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  options=list(
    thumbnail=''
  )

  # Create url
  endpoint <- paste0('datasets/',idno)
  if(is.null(api_base_url)){
    url=nada_get_api_url(endpoint=endpoint)
  } else {
    url = paste0(api_base_url,"/",endpoint)
  }

  httpResponse <- PUT(url,
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




#' Find a project by IDNO
#'
#' Find a project by IDNO
#'
#' @return NULL
#' @param idno (required) Study unique identifier
#' @param api_key API key (optional if API key is set using nada_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using nada_set_api_url)
#'
#' @examples
#'
#' nada_study_get_by_idno (
#'   idno="survey-idno-test"
#' )
#'
#' @export
nada_study_get_by_idno <- function(
                    idno,
                    api_key=NULL,
                    api_base_url=NULL){

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  # Create url
  endpoint <- paste0('datasets/',idno)
  if(is.null(api_base_url)){
    url=nada_get_api_url(endpoint=endpoint)
  } else {
    url = paste0(api_base_url,"/",endpoint)
  }

  httpResponse <- GET(url, add_headers("X-API-KEY" = api_key))

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

#' Set various options for dataset
#'
#' Set various options for dataset, such as access policy, project publish status, tags and aliases, owner and linked nada_collection_list and links to the data, study website and indicators website.
#'
#' @return NULL
#' @param idno (required) Study unique identifier
#' @param api_key API key (optional if API key is set using nada_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using nada_set_api_url)
#' @param access_policy Select the access policy suitable for your data. Valid values - "direct", "public", "licensed", "data_enclave", "remote", "data_na", "open"
#' @param data_remote_url Link to the website where the data is available. Required if access_policy is set to "remote".
#' @param published Set status for study - 0 = Draft, 1 = Published
#' @param tags Tag or vector of multiple tags for study (string)
#' @param aliases Alias or vector of multiple aliases for study (string)
#' @param owner_collection Collection that owns the dataset (repositoryid (string) of existing collection)
#' @param linked_collections Display in other nada_collection_list (repositoryid (string) of existing collection or vector with multiple nada_collection_list)
#' @param link_study URL for study website (string must include http:// or https://)
#' @param link_indicator URL to the indicators website (string must include http:// or https://)
#'
#' @param verbose Show verbose output - True, False
#' @examples
#'
#' nada_admin_study_get_options (
#'   idno="survey-idno-test",
#'   access_policy = "licensed",
#'   tags = "ihsn",
#'   link_study = "http://www.studypage.org",
#'   link_indicator = "http://www.indicatorpage.org"
#' )
#'
#' @export
nada_admin_study_get_options <- function(
  idno,
  api_key=NULL,
  api_base_url=NULL,
  access_policy=NULL,
  data_remote_url=NULL,
  published=NULL,
  tags=NULL,
  aliases=NULL,
  owner_collection=NULL,
  linked_collections=NULL,
  link_study=NULL,
  link_indicator=NULL){

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  # Check whether access policy is a valid string
  if(!is.null(access_policy)){
      valid_access_types <- c("direct","public","licensed","data_enclave","remote","data_na","open")
      if(!(access_policy %in% valid_access_types)){
        stop(paste("access_policy should be one of the valid types - valid types are", paste(valid_access_types, collapse = ", ")))
      }
  }
  # Check whether published is valid value (0 or 1)
  if(!is.null(published)){
    if(!(published %in% c(0, 1))){
      stop(paste("published should be either 0 (draft) or 1 (published)"))
    }
  }
  # Check if owner and linked nada_collection_list exist
  if(!is.null(owner_collection)){
    existing_collections <- nada_collection_list()$content$nada_collection_list[,"repositoryid"]
    if(!(owner_collection %in% existing_collections)){
        stop("owner_collection is not an existing collection - to proceed first create the collection")
    }
  }
  if(!is.null(linked_collections)){
    existing_collections <- nada_collection_list()$content$nada_collection_list[,"repositoryid"]
    if(!(all(linked_collections %in% existing_collections))){
      stop("linked_collections contains nada_collection_list that are not an existing collection - to proceed first create the collection")
    }
  }

  # Check whether aliases and tags are strings
  if(!is.null(aliases)){
    if(!(all(is.character(aliases)))){
      stop("all aliases must be of type string")
    }
  }
  if(!is.null(tags)){
    if(!(all(is.character(tags)))){
      stop("all tags must be of type string")
    }
  }

  # Create list of tags, aliases and linked collections
  if(!is.null(tags)){tags = as.list(tags)}
  if(!is.null(aliases)){aliases = as.list(aliases)}
  if(!is.null(aliases))linked_collections = as.list(linked_collections)

  options=list(
    "access_policy"=access_policy,
    "data_remote_url"=data_remote_url,
    "published"=published,
    "tags"=tags,
    "aliases"=aliases,
    "owner_collection"=owner_collection,
    "linked_collections"=linked_collections,
    "link_study"=link_study,
    "link_indicator"=link_indicator
  )

  # Create url
  endpoint <- paste0('datasets/options/',idno)
  if(is.null(api_base_url)){
    url=nada_get_api_url(endpoint=endpoint)
  } else {
    url = paste0(api_base_url,"/",endpoint)
  }

  httpResponse <- POST(url,
                      add_headers("X-API-KEY" = api_key),
                      body = options,
                      encode = "json",
                      accept_json())
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  output=list(
    "status_code"=httpResponse$status_code,
    "response"=fromJSON(content(httpResponse,"text"))
  )

  return(output)
}



#' Attach related studies
#'
#' Attach related studies
#'
#' @return NULL
#' @param idno (required) Study unique identifier
#' @param related_datasets \strong{(required)} list of related studies IDNOs
#' @param api_key API key (optional)
#' @param api_base_url API base endpoint (optional)
#'
#' @examples
#'
#' nada_admin_study_attach_related (
#'   idno="survey-idno-test",
#'   related_datasets = c("idno-1", "idno-2", "idno-3")
#' )
#'
#' @export
nada_admin_study_attach_related <- function(
                   idno,
                   related_datasets,
                   api_key=NULL,
                   api_base_url=NULL){

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  options=list(
    idno=idno,
	related_datasets=related_datasets
  )

  # Create url
  endpoint <- paste0('datasets/related_datasets')
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


#' Delete a study
#'
#' Delete a single entry from the catalog
#'
#' @return list
#' @param idno (Required) Dataset IDNo
#'
#' @export
nada_admin_study_delete <- function(idno, api_key=NULL, api_base_url=NULL){

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  url=nada_get_api_url(paste0('datasets/', idno))
  httpResponse <- DELETE(url, add_headers("X-API-KEY" = api_key), accept_json())
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
    stop(content(httpResponse, "text"), call. = FALSE)
  }

  output=fromJSON(content(httpResponse,"text"))
  return (output)
}
