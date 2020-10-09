#' Create new table
#'
#' Create a new table
#'
#' @return NULL
#' @param idno (required) Study unique identifier
#' @param repositoryid Collection ID that owns the study
#' @param access_policy Select the access policy suitable for your data. Valid values - "open" "direct" "public" "licensed" "enclave" "remote" "other"
#' @param data_remote_url Link to the website where the data is available. Required if access_policy is set to 'remote'.
#' @param published Set status for study - 0 = Draft, 1 = Published
#' @param overwrite Overwrite if a study with the same ID already exists? Valid values "yes", "no"
#' @param metadata \strong{(required)} Metadata using the Table Schema
#'
#' @examples
#'
#' metadata=list(
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
#' "table_description"= list(
#'   "title_statement"= list(
#'     "idno"= "table-unique-id",
#'     "title"= "table title",
#'     "sub_title"= "string",
#'     "alternate_title"= "string",
#'     "abbreviated_title"= "string"
#'   ),
#' ),
#' "files"= list(
#'   list(
#'     "file_uri"= "http://example.com/files/file.xls",
#'     "format"= "application/excel",
#'     "location"= "sheet1",
#'     "note"= "some note"
#'   )
#' )
#' )
#'
#' table_add (
#'   idno="table-idno",
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
table_add <- function(idno,
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

  files=list()

  #change file_uri value to file basename
  if(!is.null(metadata$files)){
    files=metadata$files
    for(i in seq_along(metadata$files)){
      if (file.exists(metadata$files[[i]]$file_uri)){
        metadata$files[[i]]$file_uri=basename(metadata$files[[i]]$file_uri)
      }
    }
  }

  result = create(type= "table",
                  idno= idno,
                  repositoryid= repositoryid,
                  access_policy= access_policy,
                  data_remote_url= data_remote_url,
                  published= published,
                  overwrite= overwrite,
                  metadata= metadata,
                  thumbnail= thumbnail
  )

  if(result$status_code==200){
    if(!is.null(files)){
      for(f in files){
        if(file.exists(f$file_uri) || is_valid_url(f$file_uri)){
          resource_result=external_resources_add(idno=idno,
                                          dctype="Document [doc/oth]",
                                          title=basename(f$file_uri),
                                          file_path=f$file_uri,
                                          overwrite="yes"
          )
          result$resources[[basename(f$file_uri)]]=resource_result
        } else{
          warning(paste("File not found:",f$file_uri))
        }
      }
    }
  }

  return (result)
}
