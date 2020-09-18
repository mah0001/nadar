
#' Create timeseries database
#'
#' Create a new timeseries database
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
#' "published"= 0,
#' "overwrite"= "no",
#' "database_description"= list(
#'   "title_statement"= list(
#'     "idno"= "string",
#'     "title"= "string",
#'     "sub_title"= "string",
#'     "alternate_title"= "string",
#'     "translated_title"= "string"
#'   ),
#'   "authoring_entity"= list(
#'     list(
#'       "name"= "string",
#'       "role"= "string",
#'       "affiliation"= "string",
#'       "abbreviation"= null,
#'       "email"= null
#'     )
#'     ),
#'   "abstract"= "string",
#'   "url"= "string",
#'   "type"= "string",
#'   "doi"= "string",
#'   "date_created"= "string",
#'   "date_published"= "string",
#'   "version"= list(
#'     list(
#'       "version"= "string",
#'       "date"= "string",
#'       "responsibility"= "string",
#'       "notes"= "string"
#'     )
#'     ),
#'   "update_frequency"= "string",
#'   "update_schedule"= list(
#'     list(
#'       "update"= "string"
#'     )
#'     ),
#'   "time_coverage"= list(
#'     list(
#'       "start"= "string",
#'       "end"= "string"
#'     )
#'     ),
#'   "time_coverage_note"= "string",
#'   "periodicity"= list(
#'     list(
#'       "period"= "string"
#'     )
#'     ),
#'   "themes"= list(
#'     list(
#'       "name"= "string",
#'       "vocabulary"= "string",
#'       "uri"= "string"
#'     )
#'     ),
#'   "topics"= list(
#'     list(
#'       "id"= "string",
#'       "name"= "string",
#'       "parent_id"= "string",
#'       "vocabulary"= "string",
#'       "uri"= "string"
#'     )
#'     ),
#'   "keywords"= list(
#'     list(
#'       "name"= "string",
#'       "vocabulary"= "string",
#'       "uri"= "string"
#'     )
#'     ),
#'   "geographic_units"= list(
#'     list(
#'       "name"= "string",
#'       "code"= "string",
#'       "type"= "string"
#'     )
#'     ),
#'   "geographic_coverage_note"= "string",
#'   "bbox"= list(
#'     list(
#'       "west"= "string",
#'       "east"= "string",
#'       "south"= "string",
#'       "north"= "string"
#'     )
#'     ),
#'   "geographic_granularity"= "string",
#'   "geographic_area_count"= "string",
#'   "sponsors"= list(
#'     list(
#'       "name"= "string",
#'       "abbreviation"= "string",
#'       "role"= "string",
#'       "grant"= "string",
#'       "uri"= "string"
#'     )
#'     ),
#'   "acknowledgments"= list(
#'     list(
#'       "name"= "string",
#'       "affiliation"= "string",
#'       "role"= "string",
#'       "uri"= "string"
#'     )
#'     ),
#'   "contacts"= list(
#'     list(
#'       "name"= "string",
#'       "role"= "string",
#'       "affiliation"= "string",
#'       "email"= "string",
#'       "telephone"= "string",
#'       "uri"= "string"
#'     )
#'     ),
#'   "links"= list(
#'     list(
#'       "uri"= "string",
#'       "description"= "string"
#'     )
#'     ),
#'   "languages"= list(
#'     list(
#'       "name"= "string",
#'       "code"= "string"
#'     )
#'     ),
#'   "access_options"= list(
#'     list(
#'       "type"= "string",
#'       "uri"= "string",
#'       "note"= "string"
#'     )
#'     ),
#'   "license"= list(
#'     list(
#'       "type"= "string",
#'       "uri"= "string",
#'       "note"= "string"
#'     )
#'     ),
#'   "citation"= "string",
#'   "notes"= list(
#'     list(
#'       "note"= "string"
#'     )
#'     ),
#'   "disclaimer"= "string",
#'   "copyright"= "string"
#' ),
#' "additional"= list()
#' )
#'
#' timeseries_database_add (
#'   idno="document-idno",
#'   published = 1,
#'   overwrite = "yes",
#'   metadata = metadata
#' )
#'
#'
#'
#'
#' @export
timeseries_database_add <- function(idno,
                            published=0,
                            overwrite="no",
                            metadata=NULL,
                            api_key=NULL,
                            api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key()
  }

  result = create(type="timeseries-db",
                  idno=idno,
                  repositoryid=repositoryid,
                  access_policy=access_policy,
                  data_remote_url = data_remote_url,
                  published = published,
                  overwrite= overwrite,
                  metadata=metadata)

  return (result)
}
