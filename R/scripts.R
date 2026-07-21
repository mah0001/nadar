#' Create new script
#'
#' Create a new script
#'
#' @return NULL
#' @param idno (required) Study unique identifier
#' @param repositoryid Collection ID that owns the study
#' @param access_policy Select the access policy suitable for your data. Valid values - "open" "direct" "public" "licensed" "enclave" "remote" "other"
#' @param data_remote_url Link to the website where the data is available. Required if access_policy is set to 'remote'.
#' @param published Set status for study - 0 = Draft, 1 = Published
#' @param overwrite Overwrite if a study with the same ID already exists? Valid values "yes", "no"
#' @param metadata \strong{(required)} Metadata using the Script Schema
#'
#' @examples
#'
#'
#' nada_admin_script_add (
#'   idno="script-idno",
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
nada_admin_script_add <- function(idno,
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
    api_key=nada_get_api_key();
  }

  files=list()

  #change file_name value to file basename
  if(!is.null(metadata$project_desc$scripts)){
    files=metadata$project_desc$scripts
    for(i in seq_along(metadata$project_desc$scripts)){
      metadata$project_desc$scripts[[i]]$file_name=basename(metadata$project_desc$scripts[[i]]$file_name)
    }
  }

  result = nada_admin_study_create(type= "script",
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
      print ("script files were found, processing....")
      for(f in files){
        if(file.exists(f$file_name)){
          resource_result=nada_admin_resource_add(idno=idno,
                                                 dctype="Document [doc/oth]",
                                                 title=basename(f$file_name),
                                                 file_path=f$file_name,
                                                 overwrite="yes"
          )
          result$resources[[basename(f$file_name)]]=resource_result
        } else{
          warning(paste("File not found:",f$file_name))
        }
      }
    }
  }

  return (result)
}
