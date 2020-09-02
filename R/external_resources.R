#library(curl)
#library(httr)
#library(jsonlite)

#' List External Resources
#'
#' List external resources for a study
#'
#' @return List of external resources
#' @param dataset_idno Study IDNo
#' @export
list_resources <- function(dataset_idno, api_key=NULL, api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  endpoint=paste0('datasets/',dataset_idno,'/resources')
  url=get_api_url(endpoint)

  httpResponse <- GET(url, add_headers("X-API-KEY" = api_key), accept_json(), verbose(get_verbose()))
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }else{
    output=fromJSON(content(httpResponse,"text"))
  }

  return (output)
}


#' import_rdf
#'
#' Import an RDF file
#'
#' @return NULL
#' @param dataset_idno Study IDNo
#' @param rdf_file RDF file path
#' @export
import_rdf <- function(
                      dataset_idno,
                      rdf_file,
                      api_key=NULL,
                      api_base_url=NULL
                      ){

  endpoint=paste0('datasets/',dataset_idno,'/resources/import_rdf')

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  url=get_api_url(endpoint)

  options=list(
    "file"=upload_file(rdf_file)
  )

  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key),body=options, accept_json(), verbose(get_verbose()))
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }else{
    output=fromJSON(content(httpResponse,"text"))
  }

  return (output)
}




#' Upload external resources
#'
#' Upload an external resource file
#'
#' @return NULL
#' @param dataset_idno Study IDNo
#' @param resource_id (Optional) External resource ID
#' @param file External resource file to be uploaded
#' @export
upload_resource <- function(
                      dataset_idno,
                      resource_id=NULL,
                      file,
                      api_key=NULL,
                      api_base_url=NULL){

  endpoint=paste0('datasets/',dataset_idno,'/files')

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  url=get_api_url(endpoint)

  options=list(
    "file"=upload_file(file)
  )

  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key),body=options, accept_json(), verbose(get_verbose()))
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }else{
    output=fromJSON(content(httpResponse,"text"))
  }

  return (output)
}



#' Download resource file
#'
#' Download resource file
#'
#' @return file
#' @param dataset_idno Study IDNo
#' @param resource_id Resource ID
#' @export
download_resource <- function(dataset_idno, resource_id,api_key=NULL, api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  endpoint=paste0('datasets/',dataset_idno,'/resources/download/',resource_id)
  url=get_api_url(endpoint)

  httpResponse <- GET(url, add_headers("X-API-KEY" = api_key), accept_json(), verbose(get_verbose()))
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))

    return (httpResponse)
  }

  #get downloaded file name
  file_name=get_disposition_filename(httpResponse)

  #save downloaded file
  #writeBin(resource$content, resource$file_name)

  return (
    list(
      "file_name"=file_name,
      "content" = content(httpResponse,"raw")
      )
    )
}





#' Create new resource
#'
#' Create a new resource
#'
#' @return NULL
#' @param idno \strong{(required)} Study IDNO
#' @param dctype Resource document type
#' @param title Resource title
#' @param dcformat Resource file format
#' @param author Author name
#' @param dcdate Date using YYYY-MM-DD format
#' @param country Country name
#' @param language Language or Language code
#' @param contributor Contributor name
#' @param publisher Publisher name
#' @param rights Rights
#' @param description Resource detailed description
#' @param abstract  Resource abstract
#' @param toc Table of contents
#' @param file_path File path for uploading
#' @param overwrite Overwrite if resource already exists? Accepted values "yes", "no"
#'
#'
#'
#'
#' @export
create_resource <- function(
                      idno,
                      dctype,
                      title,
                      dcformat=NULL,
                      author=NULL,
                      dcdate=NULL,
                      country=NULL,
                      language=NULL,
                      contributor=NULL,
                      publisher=NULL,
                      rights=NULL,
                      description=NULL,
                      abstract=NULL,
                      toc=NULL,
                      file_path=NULL,
                      overwrite="no",
                      api_key=NULL,
                      api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  options=list(
    idno=idno,
    dctype=dctype,
    dcformat=dcformat,
    title=title,
    author=author,
    dcdate=dcdate,
    country=country,
    language=language,
    contributor=contributor,
    publisher=publisher,
    rights=rights,
    description=description,
    abstract=abstract,
    toc=toc,
    overwrite=overwrite
  )

  if (file.exists(file_path)){
    options$file=upload_file(file_path)
  }

  url=get_api_url(paste0('datasets/',idno,'/resources'))
  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body=options,
                       accept_json(),
                       verbose(get_verbose()))

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


