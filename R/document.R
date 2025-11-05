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
#' @export
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
#' nada_admin_document_add(
#'   idno="document-idno",
#'   published = 1,
#'   overwrite = "yes",
#'   metadata = metadata,
#'   thumbnail ="images/thumbnail.jpg"
#' )
#'
nada_admin_document_add <- function(idno,
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
    api_key=nada_get_api_key();
  }

  files=list()

  #change file_uri value to keep only file basename
  if(!is.null(metadata$files)){
    files=metadata$files
    for(i in seq_along(metadata$files)){
      if (file.exists(metadata$files[[i]]$file_uri)){
        metadata$files[[i]]$file_uri=basename(metadata$files[[i]]$file_uri)
      }
    }
  }

  result = nada_admin_study_create(type="document",
                  idno=idno,
                  repositoryid = repositoryid,
                  access_policy = access_policy,
                  data_remote_url = data_remote_url,
                  published = published,
                  overwrite = overwrite,
                  metadata = metadata,
                  thumbnail = thumbnail
  )

  if(result$status_code==200){
    if(!is.null(files)){
      for(f in files){
        if(file.exists(f$file_uri) || nada_is_valid_url(f$file_uri) ){
          resource_result=nada_admin_resource_add(idno=idno,
                                                 dctype="Document [doc/oth]",
                                                 title=basename(f$file_uri),
                                                 file_path=f$file_uri,
                                                 overwrite="yes")
          result$resources[[basename(f$file_uri)]] <- resource_result
        } else{
          warning(paste("File not found:",f$file_uri))
        }
      }
    }
  }
  return (result)
}
