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
    thumbnail_result=upload_thumbnail(idno=idno,thumbnail = thumbnail)
  }

  output=list(
    "status_code"=httpResponse$status_code,
    "response"=fromJSON(content(httpResponse,"text")),
    "thumbnail"=thumbnail_result
  )

  return (output)
}





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
#' create_geospatial (
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
create_geospatial <- function(
                   idno,
                   metadata,
                   repositoryid=NULL,
                   access_policy=NULL,
                   data_remote_url=NULL,
                   published=0,
                   overwrite="no",
                   thumbnail=NULL,
                   api_key=NULL,
                   api_base_url=NULL
                   ){

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






#' Create new document
#'
#' Create a new document
#'
#' @return NULL
#' @param idno (required) Study unique identifier
#' @param repositoryid Collection ID that owns the study
#' @param access_policy Select the access policy suitable for your data. Valid values - "open" "direct" "public" "licensed" "enclave" "remote" "other"
#' @param data_remote_url Link to the website where the data is available. Required if access_policy is set to 'remote'.
#' @param published Set status for study - 0 = Draft, 1 = Published
#' @param overwrite Overwrite if a study with the same ID already exists? Valid values "yes", "no"
#' @param metadata \strong{(required)} Metadata list
#'
#' @examples
#'
#' metadata=list(
#' "repositoryid"= NULL,
#' "published"= 1,
#' "overwrite"= "no",
#' "metadata_information"= list(
#' "title"= "string",
#' "idno"= "string",
#' "producers"= list(
#'  list(
#'       "name"= "string",
#'       "abbr"= "string",
#'       "affiliation"= "string",
#'       "role"= "string"
#'     )
#'   ),
#'   "production_date"= "string",
#'   "version"= "string"
#' ),
#' "document_description"= list(
#'   "title_statement"= list(
#'     "idno"= "document-unique-id",
#'     "title"= "document title",
#'     "sub_title"= "string",
#'     "alternate_title"= "string",
#'     "abbreviated_title"= "string"
#'   ),
#'   "type"= "article",
#'   "description"= "string",
#'   "toc"= "string",
#'   "toc_structured"= list(
#'     list(
#'       "id"= "string",
#'       "parent_id"= "string",
#'       "name"= "string"
#'     )
#'   ),
#'   "abstract"= "string",
#'   "notes"= list(
#'     list(
#'       "note"= "string"
#'     )
#'   ),
#'   "scope"= "string",
#'   "ref_country"= list(
#'     list(
#'       "name"= "country name",
#'       "code"= "string"
#'     ),
#'     list(
#'       "name"= "country name 2",
#'       "code"= "string"
#'     )
#'   ),
#'   "spatial_coverage"= "string",
#'   "temporal_coverage"= "string",
#'   "date_created"= "string",
#'   "date_available"= "string",
#'   "date_modified"= "string",
#'   "date_published"= "string",
#'   "id_numbers"= list(
#'     "type"= "string",
#'     "value"= "string"
#'   ),
#'   "publication_frequency"= "string",
#'   "languages"= list(
#'     list(
#'       "name"= "string",
#'       "code"= "string"
#'     )
#'   ),
#'   "license"= list(
#'     list(
#'       "name"= "string",
#'       "uri"= "string"
#'     )
#' ),
#'   "bibliographic_citation"= "string",
#'   "chapter"= "string",
#'   "edition"= "string",
#'   "institution"= "string",
#'   "journal"= "string",
#'   "volume"= "string",
#'   "issue"= "string",
#'   "pages"= "string",
#'   "series"= "string",
#'   "creator"= "string",
#'   "authors"= list(
#'     list(
#'       "first_name"= "string",
#'       "initial"= "string",
#'       "last_name"= "string",
#'       "affiliation"= "string"
#'     )
#'   ),
#'   "editors"= list(
#'     list(
#'       "first_name"= "string",
#'       "initial"= "string",
#'       "last_name"= "string",
#'       "affiliation"= "string"
#'     )
#'   ),
#'   "translators"= list(
#'     list(
#'       "first_name"= "string",
#'       "initial"= "string",
#'       "last_name"= "string",
#'       "affiliation"= "string"
#'     )
#'   ),
#'   "contributors"= list(
#'     list(
#'       "first_name"= "string",
#'       "initial"= "string",
#'       "last_name"= "string",
#'       "affiliation"= "string"
#'     )
#'   ),
#'   "publisher"= "string",
#'   "publisher_address"= "string",
#'   "rights"= "string",
#'   "copyright"= "string",
#'   "usage_terms"= "string",
#'   "security_classification"= "string",
#'   "access_restrictions"= "string",
#'   "sources"= list(
#'     "data_source"= list(),
#'     "source_origin"= "string",
#'     "source_char"= "string",
#'     "source_doc"= "string"
#'   ),
#'   "keywords"= list(
#'     list(
#'       "name"= "string",
#'       "vocabulary"= "string",
#'       "uri"= "string"
#'     )
#'   ),
#'   "themes"= list(
#'     list(
#'       "name"= "string",
#'       "vocabulary"= "string",
#'       "uri"= "string"
#'     )
#'   ),
#'   "topics"= list(
#'     list(
#'       "id"= "string",
#'       "name"= "string",
#'       "parent_id"= "string",
#'       "vocabulary"= "string",
#'       "uri"= "string"
#'     )
#'   ),
#'   "disciplines"= list(
#'     list(
#'       "name"= "string",
#'       "vocabulary"= "string",
#'       "uri"= "string"
#'     )
#'   ),
#'   "audience"= "string",
#'   "mandate"= "string",
#'   "pricing"= "string",
#'   "relations"= list(
#'     list(
#'       "name"= "string",
#'       "type"= "isPartOf"
#'     )
#'   ),
#'   "lda_topics"= list(
#'     list(
#'       "model_info"= list(
#'         list(
#'           "source"= "string",
#'           "author"= "string",
#'           "version"= "string",
#'           "model_id"= "string",
#'           "nb_topics"= 0,
#'           "description"= "string",
#'           "corpus"= "string",
#'           "uri"= "string"
#'         )
#'       ),
#'       "topic_description"= list(
#'         list(
#'           "topic_id"= 1,
#'           "topic_score"= .01,
#'           "topic_label"= "label",
#'           "topic_words"= list(
#'             list(
#'               "word"= "string"
#'             )
#'           )
#'         )
#'       )
#'     )
#'   )
#' ),
#' "tags"= list(
#'   list(
#'     "tag"= "string"
#'   )
#' ),
#' "files"= list(
#'   list(
#'     "file_uri"= "string",
#'     "format"= "string",
#'     "location"= "string",
#'     "note"= "string"
#'   )
#' )
#' )
#'
#' create_document (
#'   idno="document-idno",
#'   published = 1,
#'   overwrite = "yes",
#'   metadata = metadata,
#'   thumbnail ="images/thumbnail.jpg"
#' )
#'
#'
#'
#'
#' @export
create_document <- function(idno,
                            metadata,
                            repositoryid=NULL,
                            access_policy=NULL,
                            data_remote_url=NULL,
                            published=0,
                            overwrite="no",
                            thumbnail=NULL,
                            api_key=NULL,
                            api_base_url=NULL
                            ){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  result = create(type="document",
                  idno=idno,
                  repositoryid=repositoryid,
                  access_policy=access_policy,
                  data_remote_url = data_remote_url,
                  published = published,
                  overwrite= overwrite,
                  metadata=metadata
  )

  return (result)
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
upload_thumbnail <- function(
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
  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key), body=options)

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
