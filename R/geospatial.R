

#' Create new geospatial study
#'
#' Create a new geospatial study
#'
#' @return NULL
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
#' metadata= list(
#' metadata_maintenance=list(
#' "update_frequency"="daily",
#' "note"="Maintenane note",
#' "contact"=list(
#' list(
#'     "person_name"="name here",
#'     "role"="role"
#'   )
#' )
#' )
#'
#' dataset_description=list(
#' "file_identifier"="Unique IDNO",
#' "language"="en",
#' "contact"= list(
#'     "person_name"= "name",
#'     "role"= "string",
#'   ),
#'   "identification_info"=list(
#'      "title"="title goes here",
#'   )
#' )
#' )
#'
#' geospatial_add (
#'   idno="survey-idno-test",
#'   published = 1,
#'   overwrite = "yes",
#'   metadata_maintenance = list(),
#'   dataset_description = list()
#'   additional = list()
#' )
#'
#'
#'
#'
#' @export
geospatial_add <- function(
  idno,
  metadata,
  repositoryid=NULL,
  access_policy=NULL,
  data_remote_url=NULL,
  published=0,
  overwrite="no",
  thumbnail=NULL,
  api_key=NULL,
  api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  result = create(type="geospatial",
                  idno=idno,
                  repositoryid=repositoryid,
                  access_policy=access_policy,
                  data_remote_url = data_remote_url,
                  published = published,
                  overwrite= overwrite,
                  metadata=metadata,
                  thumbnail=thumbnail
  )

  return (result)
}





#' Import geospatial xml file
#'
#' Import geospatial xml (ISO19139)
#'
#' @return NULL
#' @param repositoryid Collection ID that owns the study
#' @param access_policy Select the access policy suitable for your data. Valid values - "open" "direct" "public" "licensed" "enclave" "remote" "other"
#' @param data_remote_url Link to the website where the data is available. Required if access_policy is set to 'remote'.
#' @param published Set status for study - 0 = Draft, 1 = Published
#' @param overwrite Overwrite if a study with the same ID already exists? Valid values "yes", "no"
#' @param file \strong{(required)} File or URL of ISO19139 XML file
#'
#' @examples
#'
#'
#' geospatial_import (
#'   published = 1,
#'   overwrite = "yes",
#'   file = "http://example.com/file/file-iso13913.xml",
#' )
#'
#'
#'
#'
#' @export
geospatial_import <- function(
  file_uri,
  repositoryid=NULL,
  access_policy=NULL,
  data_remote_url=NULL,
  published=0,
  overwrite="no",
  api_key=NULL,
  api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }


  options=list(
    repositoryid=repositoryid,
    access_policy=access_policy,
    data_remote_url=data_remote_url,
    published=published,
    overwrite=overwrite
  )

  if (file.exists(file_uri)){
    options$file=upload_file(file_uri)
  }
  else if(is_valid_url(file_uri)){
    options$file=file_uri
  }

  url=get_api_url('datasets/import_geospatial')
  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body=options,
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

