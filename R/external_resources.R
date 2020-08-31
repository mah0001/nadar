#library(curl)
#library(httr)
#library(jsonlite)

#' List External Resources
#'
#' List external resources for a study
#'
#' @return List of external resources
#' @param dataset_idno Study IDNo
#' @export
resources <- function(dataset_idno, api_key=NULL, api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  endpoint=paste0('datasets/',dataset_idno,'/resources')
  url=get_api_url(endpoint)

  httpResponse <- GET(url, add_headers("X-API-KEY" = api_key), accept_json(), verbose(get_verbose()))
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }else{
    output=fromJSON(content(httpResponse,"text"))
  }

  return (output)
}


#' ImportRDF
#'
#' Import an RDF file
#'
#' @return NULL
#' @param dataset_idno Study IDNo
#' @param rdf_file RDF file path
#' @export
import_rdf <- function(
                      dataset_idno,
                      rdf_file,
                      api_key=NULL,
                      api_base_url=NULL
                      ){

  endpoint=paste0('datasets/',dataset_idno,'/resources/import_rdf')

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  url=get_api_url(endpoint)

  options=list(
    "file"=upload_file(rdf_file)
  )

  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key),body=options, accept_json(), verbose(get_verbose()))
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }else{
    output=fromJSON(content(httpResponse,"text"))
  }

  return (output)
}




#' Upload external resources
#'
#' Upload an external resource file
#'
#' @return NULL
#' @param dataset_idno Study IDNo
#' @param resource_id (Optional) External resource ID
#' @param file External resource file to be uploaded
#' @export
resource_upload <- function(
                      dataset_idno,
                      resource_id=NULL,
                      file,
                      api_key=NULL,
                      api_base_url=NULL){

  endpoint=paste0('datasets/',dataset_idno,'/files')

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  url=get_api_url(endpoint)

  options=list(
    "file"=upload_file(file)
  )

  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key),body=options, accept_json(), verbose(get_verbose()))
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }else{
    output=fromJSON(content(httpResponse,"text"))
  }

  return (output)
}



#' Download resource file
#'
#' Download resource file
#'
#' @return file
#' @param dataset_idno Study IDNo
#' @param resource_id Resource ID
#' @export
resource_download <- function(dataset_idno, resource_id,api_key=NULL, api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  endpoint=paste0('datasets/',dataset_idno,'/resources/download/',resource_id)
  url=get_api_url(endpoint)

  httpResponse <- GET(url, add_headers("X-API-KEY" = api_key), accept_json(), verbose(get_verbose()))
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))

    return (httpResponse)
  }

  #get downloaded file name
  file_name=get_disposition_filename(httpResponse)

  #save downloaded file
  #writeBin(resource$content, resource$file_name)

  return (
    list(
      "file_name"=file_name,
      "content" = content(httpResponse,"raw")
      )
    )
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
#'
#' doc_desc=list(
#' "idno"="doc-idno",
#' "producers"=list(
#' list(
#'     "name"="name here",
#'     "abbr"="abbreviation"
#'   )
#' )
#' )
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
#'
#' create (
#'   idno="survey-idno-test",
#'   type="survey",
#'   published = 1,
#'   overwrite = "yes",
#'   doc_desc = doc_desc,
#'   study_desc = study_desc,
#'   data_files=NULL,
#'   variables=NULL,
#'   variable_groups = NULL,
#'   additional = NULL
#' )
#'
#'
#'
#'
#' @export
resource_create <- function(
                      idno,
                      dctype,
                      title,
                      dcformat=NULL,
                      author=NULL,
                      dcdate=NULL,
                      country=NULL,
                      language=NULL,
                      contributor=NULL,
                      publisher=NULL,
                      rights=NULL,
                      description=NULL,
                      abstract=NULL,
                      toc=NULL,
                      file_path=NULL,
                      api_key=NULL,
                      api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  options=list(
    idno=idno,
    dctype=dctype,
    dcformat=dcformat,
    title=title,
    author=author,
    dcdate=dcdate,
    country=country,
    language=language,
    contributor=contributor,
    publisher=publisher,
    rights=rights,
    description=description,
    abstract=abstract,
    toc=toc
  )

  if (file.exists(file_path)){
    options$file=upload_file(file_path)
  }

  url=get_api_url(paste0('datasets/',idno,'/resources'))
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


