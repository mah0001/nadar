#' Create entry in catalog
#'
#' @param type \strong{(required)} Entry type - "document", "script", "table",
#' @param idno \strong{(required)} Study unique identifier
#' @param repositoryid Collection ID that owns the entry
#' @param access_policy Select the access policy suitable for your data. Valid values - "open",
#' "direct", "public", "licensed", "enclave", "remote", "other"
#' @param data_remote_url Link to the website where the data is available. Required if access_policy is set to 'remote'.
#' @param published Set status for entry - 0 = Draft, 1 = Published
#' @param overwrite Overwrite if a entry with the same ID already exists? Valid values "yes", "no"
#' @param metadata \strong{(required)} Metadata list depending on the type of study
#' @param api_key (optoinal) API key
#' @param api_base_url (optional) API URL
#'
#' @return result
#' @export
create_entry <- function(
  type,
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

  if (type %in% c("document", "script", "table")) {
    #change file_uri value to keep only file basename
    if(!is.null(metadata$files)){
      files=metadata$files
      #  files=metadata$project_desc$scripts #scripts
      #  files=metadata$files # document / tables
      for(i in seq_along(metadata$files)){
        if (file.exists(metadata$files[[i]]$file_uri)){
          metadata$files[[i]]$file_uri=basename(metadata$files[[i]]$file_uri)
        }
      }
    }
  }

  result = create(type=type,
                  idno=idno,
                  repositoryid=repositoryid,
                  access_policy=access_policy,
                  data_remote_url = data_remote_url,
                  published = published,
                  overwrite= overwrite,
                  metadata=metadata,
                  thumbnail=thumbnail
  )

  if (type %in% c("document", "script", "table")) {
    if(result$status_code==200){
      if(!is.null(files)){
        for(f in files){
          if(file.exists(f$file_uri) || is_valid_url(f$file_uri) ){
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
  }

  return (result)
}
