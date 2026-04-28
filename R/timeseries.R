
#' Create timeseries indicator
#'
#' Create a new timeseries indicator
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
#'   "metadata_creation"= list(
#'     "producers"= list(
#'       list(
#'         "name"= "string",
#'         "abbr"= "string",
#'         "affiliation"= "string",
#'         "role"= "string"
#'       )
#'     ),
#'     "prod_date"= "string",
#'     "version"= "string"
#'   ),
#'   "series_description"= list(
#'     "idno"= "unique=series-idno",
#'     "name"= "Series name",
#'     "database_id"= "db-idno",
#'     "aliases"= list(
#'       list(
#'         "alias"= "string"
#'       )
#'     ),
#'     "measurement_unit"= "string",
#'     "periodicity"= "string",
#'     "base_period"= "string",
#'     "definition_short"= "string",
#'     "definition_long"= "string",
#'     "definition_references"= list(
#'       list(
#'         "source"= "string",
#'         "uri"= "string",
#'         "note"= "string"
#'       )
#'     ),
#'     "statistical_concept"= "string",
#'     "concepts"= list(
#'       list(
#'         "name"= "string",
#'         "definition"= "string",
#'         "uri"= "string"
#'       )
#'     ),
#'     "methodology"= "string",
#'     "imputation"= "string",
#'     "quality_checks"= "string",
#'     "quality_note"= "string",
#'     "series_break"= "string",
#'     "limitation"= "string",
#'     "themes"= list(
#'       list(
#'         "name"= "string",
#'         "vocabulary"= "string",
#'         "uri"= "string"
#'       )
#'     ),
#'     "topics"= list(
#'       list(
#'         "id"= "string",
#'         "name"= "string",
#'         "parent_id"= "string",
#'         "vocabulary"= "string",
#'         "uri"= "string"
#'       )
#'     ),
#'     "disciplines"= list(
#'       list(
#'         "name"= "string",
#'         "vocabulary"= "string",
#'         "uri"= "string"
#'       )
#'     ),
#'     "relevance"= "string",
#'     "time_periods"= list(
#'       list(
#'         "start"= "string",
#'         "end"= "string"
#'       )
#'     ),
#'     "geographic_units"= list(
#'       list(
#'         "name"= "string",
#'         "code"= "string",
#'         "type"= "string"
#'       )
#'     ),
#'     "aggregation_method"= "string",
#'     "license"= list(
#'       "name"= "string",
#'       "uri"= "string"
#'     ),
#'     "confidentiality"= "string",
#'     "confidentiality_status"= "string",
#'     "confidentiality_note"= "string",
#'     "links"= list(
#'       list(
#'         "type"= "string",
#'         "description"= "string",
#'         "uri"= "string"
#'       )
#'     ),
#'     "api_documentation"= list(
#'       "description"= "string",
#'       "uri"= "string"
#'     ),
#'     "source"= "string",
#'     "source_note"= "string",
#'     "keywords"= list(
#'       list(
#'         "name"= "string",
#'         "vocabulary"= "string",
#'         "uri"= "string"
#'       )
#'     ),
#'     "acronyms"= list(
#'       list(
#'         "acronym"= "string",
#'         "expansion"= "string",
#'         "occurrence"= 0
#'       )
#'     ),
#'     "notes"= list(
#'       list(
#'         "note"= "string"
#'       )
#'     ),
#'     "related_indicators"= list(
#'       list(
#'         "code"= "string",
#'         "label"= "string",
#'         "uri"= "string"
#'       )
#'     ),
#'     "compliance"= list(
#'       list(
#'         "standard"= "string",
#'         "organization"= "string",
#'         "uri"= "string"
#'       )
#'     ),
#'     "lda_topics"= list(
#'       list(
#'         "model_info"= list(
#'           list(
#'             "source"= "string",
#'             "author"= "string",
#'             "version"= "string",
#'             "model_id"= "string",
#'             "nb_topics"= 0,
#'             "description"= "string",
#'             "corpus"= "string",
#'             "uri"= "string"
#'           )
#'         ),
#'         "topic_description"= list(
#'           list(
#'             "topic_id"= 1,
#'             "topic_score"= 0.0002,
#'             "topic_label"= "string",
#'             "topic_words"= list(
#'               list(
#'                 "word"= "string",
#'                 "word_weight"= 0
#'               )
#'             )
#'           )
#'         )
#'       )
#'     ),
#'     "word_vectors"= list(
#'       list(
#'         "id"= "string",
#'         "description"= "string",
#'         "date"= "string",
#'         "vector"= list(0.526272732298821,0.587750827893615)
#'       )
#'     ),
#'     "series_groups"= list(
#'       list(
#'         "name"= "string",
#'         "version"= "string",
#'         "uri"= "string"
#'       )
#'     )
#'   ),
#'   "additional"= list()
#' )
#'
#' nada_admin_timeseries_add (
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
nada_admin_timeseries_add <- function(idno,
                            repositoryid="central",
                            access_policy=NULL,
                            data_remote_url=NULL,
                            published=0,
                            overwrite="no",
                            metadata=NULL,
                            thumbnail=NULL,
                            api_key=NULL,
                            api_base_url=NULL){

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  result = nada_admin_study_create(type="timeseries",
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


#' Attach a timeseries study to a global DSD
#'
#' Wraps POST /api/admin/timeseries/data/{idno}/attach-dsd.
#'
#' @param idno Study idno.
#' @param dsd_idno DSD idno (`data_structures.idno`).
#' @param dsd_id Optional numeric DSD id. When supplied, both id and idno are sent.
#' @param api_key Optional API key.
#' @param api_base_url Optional API base URL.
#'
#' @return list with `status_code` and `response`.
#' @export
nada_admin_timeseries_attach_dsd <- function(idno,
                                             dsd_idno = NULL,
                                             dsd_id = NULL,
                                             api_key = NULL,
                                             api_base_url = NULL) {
  if (is.null(idno) || !nzchar(as.character(idno))) {
    stop("`idno` (study idno) is required.")
  }
  if ((is.null(dsd_idno) || !nzchar(as.character(dsd_idno))) && is.null(dsd_id)) {
    stop("Provide at least one of `dsd_idno` or `dsd_id`.")
  }
  if (is.null(api_key)) api_key <- nada_get_api_key()

  endpoint <- paste0(
    "admin/timeseries/data/",
    utils::URLencode(idno, reserved = TRUE),
    "/attach-dsd"
  )
  url <- if (is.null(api_base_url)) {
    nada_get_api_url(endpoint = endpoint)
  } else {
    paste0(api_base_url, "/", endpoint)
  }

  body <- list()
  if (!is.null(dsd_idno) && nzchar(as.character(dsd_idno))) {
    body$data_structure_reference <- as.character(dsd_idno)
  }
  if (!is.null(dsd_id)) {
    body$data_structure_id <- as.integer(dsd_id)
  }

  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body = body,
                       content_type_json(),
                       encode = "json",
                       accept_json(),
                       verbose(nada_get_verbose()))

  list(
    status_code = httpResponse$status_code,
    response    = nada_http_response_json(httpResponse)
  )
}

#' Detach a timeseries study from a global DSD
#'
#' Wraps POST /api/admin/timeseries/data/{idno}/attach-dsd with `{detach: true}`.
#'
#' @param idno Study idno.
#' @param api_key Optional API key.
#' @param api_base_url Optional API base URL.
#'
#' @return list with `status_code` and `response`.
#' @export
nada_admin_timeseries_detach_dsd <- function(idno,
                                             api_key = NULL,
                                             api_base_url = NULL) {
  if (is.null(idno) || !nzchar(as.character(idno))) {
    stop("`idno` (study idno) is required.")
  }
  if (is.null(api_key)) api_key <- nada_get_api_key()

  endpoint <- paste0(
    "admin/timeseries/data/",
    utils::URLencode(idno, reserved = TRUE),
    "/attach-dsd"
  )
  url <- if (is.null(api_base_url)) {
    nada_get_api_url(endpoint = endpoint)
  } else {
    paste0(api_base_url, "/", endpoint)
  }

  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body = list(detach = TRUE),
                       content_type_json(),
                       encode = "json",
                       accept_json(),
                       verbose(nada_get_verbose()))

  list(
    status_code = httpResponse$status_code,
    response    = nada_http_response_json(httpResponse)
  )
}
