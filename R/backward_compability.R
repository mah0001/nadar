#' (deprecated - use add_document) Create new document
#'
#' Deprecated function, available for backward compability
#'
#' Create a new document
#'
#' @return NULL
#' @param idno \strong{(required)} Entry unique identifier
#' @param repositoryid Collection ID that owns the study
#' @param access_policy Select the access policy suitable for your data. Valid values - "open" "direct" "public" "licensed" "enclave" "remote" "other"
#' @param data_remote_url Link to the website where the data is available. Required if access_policy is set to 'remote'.
#' @param published Set status for entry - 0 = Draft, 1 = Published
#' @param overwrite Overwrite if a entry with the same ID already exists? Valid values "yes", "no"
#' @param metadata \strong{(required)} Metadata list
#'
#' @export
document_add <- function(idno,
                         metadata,
                         repositoryid = NULL,
                         access_policy = NULL,
                         data_remote_url = NULL,
                         published = 0,
                         overwrite = "no",
                         thumbnail = NULL,
                         api_key = NULL,
                         api_base_url = NULL
){
  # Deprecated function
  .Deprecated("add_document")
  add_document(idno = idno,
               metadata = metadata,
               repositoryid = repositoryid,
               access_policy = access_policy,
               data_remote_url = data_remote_url,
               published = published,
               overwrite = overwrite,
               thumbnail = thumbnail,
               api_key  = api_key,
               api_base_url = api_base_url)
}

