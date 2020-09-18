
#' Create Microdata
#'
#' Create a new Microdata study
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
#'  )
#')
#'
#' microdata_add (
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
microdata_add <- function(
  idno,
  repositoryid="central",
  access_policy=NULL,
  data_remote_url=NULL,
  published=1,
  overwrite="no",
  metadata=NULL,
  thumbnail=NULL,
  api_key=NULL,
  api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  result = create(type="survey",
                  idno=idno,
                  repositoryid=repositoryid,
                  access_policy=access_policy,
                  data_remote_url = data_remote_url,
                  published = published,
                  overwrite= overwrite,
                  metadata=metadata,
                  thumbnail=thumbnail)
  return (result)
}

