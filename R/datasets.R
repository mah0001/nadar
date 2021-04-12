#' GetDatasets
#'
#' Load a list of all datasets or get info for a single dataset/study
#'
#' @return List of studies or a single study info
#' @param idno (Optional) Dataset IDNo
#' @export
datasets <- function(idno=NULL, offset=NULL, limit=50, api_key=NULL, api_base_url=NULL){

  endpoint='datasets/'

  if(!is.null(idno)){
    endpoint=paste0(endpoint,'/',idno)
  }

  if(!is.null(offset)){
    endpoint=paste0(endpoint,'?offset=',offset, '&limit=',limit)
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
    class = "nada_datasets"
  )
}





#' ImportDDI
#'
#' Import a DDI file
#'
#' @return NULL
#' @param xml_file (Required) DDI/XML file path
#' @param repositoryid Collection ID that owns the study
#' @param overwrite Overwrite if a study with the same ID already exists? Valid values "yes", "no"
#' @param access_policy Select the access policy suitable for your data. Valid values - "open" "direct" "public" "licensed" "enclave" "remote" "other"
#' @param data_remote_url Link to the website where the data is available. Required if access_policy is set to 'remote'.
#' @param rdf_file RDF file path
#' @param published Set status for study - 0 = Draft, 1 = Published
#' @param verbose Show verbose output - True, False
#' @export
import_ddi <- function(
                      xml_file=NULL,
                      rdf_file=NULL,
                      repositoryid=NULL,
                      overwrite='no',
                      access_policy=NULL,
                      data_remote_url=NULL,
                      published=NULL,
                      api_key=NULL,
                      api_base_url=NULL){

  endpoint='datasets/import_ddi'

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  url=get_api_url(endpoint)

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

  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key),body=options, accept_json(), verbose(get_verbose()))

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
#' create (
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
create <- function(
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
    api_key=get_api_key();
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

  url=get_api_url(paste0('datasets/create/',type,'/',idno))
  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body=options,
                       content_type_json(),
                       encode="json",
                       accept_json(),
                       verbose(get_verbose()))

  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  thumbnail_result=NULL

  #upload thumbnail
  if(!is.null(thumbnail) && file.exists(thumbnail)) {
    thumbnail_result=thumbnail_upload(idno=idno,thumbnail = thumbnail)
  }

  #set default thumbnail
  if(!is.null(thumbnail) && thumbnail == 'default'){
    thumbnail_result= thumbnail_delete(idno=idno)
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
#'
#' @examples
#'
#' upload_thumbnail (
#'   idno="survey-idno-test",
#'   thumbnail = "/thumbnails/thumbnail-idno-test.png"
#' )
#'
#' @export
thumbnail_upload <- function(
                   idno,
                   thumbnail,
                   api_key=NULL,
                   api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  options=list(
    file=upload_file(thumbnail)
  )

  url=get_api_url(paste0('datasets/thumbnail/',idno))
  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key), body=options, verbose(get_verbose()))

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
#'
#' @examples
#'
#' delete_thumbnail (
#'   idno="survey-idno-test"
#' )
#'
#' @export
thumbnail_delete <- function(idno, api_key=NULL,api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  options=list(
    thumbnail=''
  )

  url=get_api_url(paste0('datasets/',idno))
  httpResponse <- PUT(url,
                      add_headers("X-API-KEY" = api_key),
                      body=options,
                      content_type_json(),
                      encode="json",
                      accept_json(),
                      verbose(get_verbose()))

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
#'
#' @examples
#'
#' find_by_idno (
#'   idno="survey-idno-test"
#' )
#'
#' @export
find_by_idno <- function(
                    idno,
                    api_key=NULL,
                    api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  url=get_api_url(paste0('datasets/',idno))
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
