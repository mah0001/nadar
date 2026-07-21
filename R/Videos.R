#' Create new video
#'
#' Create a new video
#'
#' @return NULL
#' @param idno (required) Unique identifier
#' @param repositoryid Owner Collection ID
#' @param access_policy Select the access policy suitable for your data. Valid values - "open" "direct" "public" "licensed" "enclave" "remote" "other"
#' @param data_remote_url Link to the website where the data is available. Required if access_policy is set to 'remote'.
#' @param published Set status for study - 0 = Draft, 1 = Published
#' @param overwrite Overwrite if a study with the same ID already exists? Valid values "yes", "no"
#' @param metadata \strong{(required)} Metadata using video schema
#'
#' @examples
#'
#' metadata=list(
#'    "repositoryid"= "central",
#'    "published"= "1",
#'    "overwrite"= "no",
#'    "video_description"= list(
#'      "idno"= "video-idno",
#'      "title"= "Lorem ipsum",
#'      "description"= "Description of video",
#'      "video_provider"= "youtube",
#'      "video_url"= "https://www.youtube.com/watch?v=7X8II6J-6mU",
#'      "embed_url"= "https://www.youtube.com/embed/7X8II6J-6mU",
#'      "country"= "Country",
#'      "language"= "english",
#'      "contributor"= "Contributor name",
#'      "publisher"= "Publisher",
#'      "rights"= "Rights",
#'      "author"= "Author name",
#'      "date_created"= "2020-01-01",
#'      "date_published"= "2021-08-17"
#'    ),
#'    "tags"= list(
#'      list(
#'        "tag"= "demo"
#'      )
#'    )
#')
#'
#'
#'  nada_admin_video_add (
#'   idno="video-idno",
#'   published = 1,
#'   overwrite = "yes",
#'   metadata = metadata,
#'   thumbnail ="path-to-thumbnail-file.jpg"
#' )
#'
#'
#'
#'
#' @export
nada_admin_video_add <- function(idno,
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


  result = nada_admin_study_create(type= "video",
                  idno= idno,
                  repositoryid= repositoryid,
                  access_policy= access_policy,
                  data_remote_url= data_remote_url,
                  published= published,
                  overwrite= overwrite,
                  metadata= metadata,
                  thumbnail=thumbnail
  )



  return (result)
}
