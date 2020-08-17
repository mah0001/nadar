#' GetDatasets
#'
#' Load a list of all datasets or get info for a single dataset/study
#'
#' @return List of studies or a single study info
#' @param idno (Optional) Dataset IDNo
#' @export
datasets <- function(api_key=NULL, api_base_url=NULL, idno=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  if(!is.null(idno)){
    endpoint=paste0(endpoint,'/',idno)
  }

  url=get_api_url('datasets')
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
import_ddi <- function(api_key=NULL,
                      api_base_url=NULL,
                      xml_file=NULL,
                      rdf_file=NULL,
                      repositoryid=NULL,
                      overwrite='no',
                      access_policy=NULL,
                      data_remote_url=NULL,
                      published=NULL){

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
    options["rdf"]=upload_file(rdf_file)
  }

  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key),body=options, accept_json(), verbose(get_verbose()))
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }else{
    output=fromJSON(content(httpResponse,"text"))
  }

  return (output)
}




#' Create Survey
#'
#' Create a new survey
#'
#' @return NULL
#' @param type (required) DDI/XML file path
#' @param idno (required) Survey unique identifier
#' @param repositoryid Collection ID that owns the study
#' @param access_policy Select the access policy suitable for your data. Valid values - "open" "direct" "public" "licensed" "enclave" "remote" "other"
#' @param data_remote_url Link to the website where the data is available. Required if access_policy is set to 'remote'.
#' @param published Set status for study - 0 = Draft, 1 = Published
#' @param overwrite Overwrite if a study with the same ID already exists? Valid values "yes", "no"
#' @param doc_desc Document description
#' @param study_desc (Required) Study description
#' @param data_files Data files
#' @param variables List of variables and metadata
#' @param variable_groups List of variable groups
#' @param additional Additional metadata
#'
#'
#' @examples
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
#' createSurvey (
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
#' @param metadata Metadata array
#' @export
create_survey <- function(api_key=NULL,
                      api_base_url=NULL,
                      idno,
                      repositoryid=NULL,
                      access_policy=NULL,
                      data_remote_url=NULL,
                      published=NULL,
                      overwrite=NULL,
                      doc_desc,
                      study_desc,
                      data_files=NULL,
                      variables=NULL,
                      variable_groups=NULL,
                      additional=NULL){

  endpoint=paste0('datasets/create/survey/',idno)

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  url=get_api_url(endpoint)

  metadata=list(
    "idno"=idno,
    "published"=published,
    "access_policy"=access_policy,
    "data_remote_url"=data_remote_url,
    "overwrite"=overwrite,
    "doc_desc"=doc_desc,
    "study_desc"=study_desc,
    "data_files"=data_files,
    "variables"=variables,
    "variable_groups"=variable_groups,
    "additional"=additional
  )

  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key), body=metadata, content_type_json(), encode="json", accept_json(), verbose(get_verbose()))

  output=NULL

  if(httpResponse$status_code!=200){
    #warning(content(httpResponse, "text"))
    stop(content(httpResponse, "text"), call. = FALSE)
  }else{
    output=fromJSON(content(httpResponse,"text"))
  }

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
#' createSurvey (
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
create <- function(api_key=NULL,
                         api_base_url=NULL,
                         type,
                         idno,
                         repositoryid=NULL,
                         access_policy=NULL,
                         data_remote_url=NULL,
                         published=NULL,
                         overwrite=NULL,
                         metadata){

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
  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key), body=metadata, content_type_json(), encode="json", accept_json(), verbose(get_verbose()))

  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }else{
    output=fromJSON(content(httpResponse,"text"))
  }

  return (output)
}

