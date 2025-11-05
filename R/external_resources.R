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
nada_resource_list <- function(dataset_idno, api_key=NULL, api_base_url=NULL){

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  endpoint=paste0('datasets/',dataset_idno,'/resources')
  url=nada_get_api_url(endpoint)

  httpResponse <- GET(url, add_headers("X-API-KEY" = api_key), accept_json(), verbose(nada_get_verbose()))
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
#' @param skip_uploads TRUE/FALSE - If TRUE, won't upload files
#' @param overwrite yes/no - Overwrite existing resources
#' @export
nada_admin_resource_import <- function(
                      dataset_idno,
                      rdf_file,
                      skip_uploads=FALSE,
                      overwrite="no",
                      api_key=NULL,
                      api_base_url=NULL
                      ){

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  resources <- nada_rdf_to_list(rdf_file)
  base_folder=dirname(rdf_file)

  for(i in 1:length(resources)) {

    if(nada_is_valid_url(resources[[i]]$filename)){
      resource_file=resources[[i]]$filename
    }else {
      resource_file=paste0(base_folder,"/",resources[[i]]$filename)

      # If file not found in location provided in rdf (incl. sub-folder), look in base_folder itself
      if(skip_uploads==FALSE && resources[[i]]$is_url==FALSE && file.exists(resource_file) == FALSE) {
        resource_file = paste0(base_folder, "/", basename(resource_file))

        if (!file.exists(resource_file)){
          warning(paste0("Resource file not found: ",resource_file))
        }
      }
    }

    print(paste0("PROCESSING file.....",resource_file))

    res_response <- nada_admin_resource_add(
        idno = dataset_idno,
        dctype = resources[[i]]$dctype,
        dcformat = resources[[i]]$dcformat,
        title = resources[[i]]$title,
        author = resources[[i]]$creator,
        dcdate = resources[[i]]$date,
        country = resources[[i]]$spatial,
        language = resources[[i]]$language,
        contributor = resources[[i]]$contributor,
        publisher = resources[[i]]$publisher,
        rights = resources[[i]]$rights,
        description = resources[[i]]$label,
        abstract = resources[[i]]$abstract,
        toc = resources[[i]]$toc,
        file_path = resource_file,
        overwrite = overwrite)
  }
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
nada_admin_resource_upload <- function(
                      dataset_idno,
                      resource_id=NULL,
                      file,
                      api_key=NULL,
                      api_base_url=NULL){

  endpoint=paste0('datasets/',dataset_idno,'/files')

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  url=nada_get_api_url(endpoint)

  options=list(
    "file"=upload_file(file)
  )

  httpResponse <- POST(url, add_headers("X-API-KEY" = api_key),body=options, accept_json(), verbose(nada_get_verbose()))
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
nada_resource_download <- function(dataset_idno, resource_id,api_key=NULL, api_base_url=NULL){

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  endpoint=paste0('datasets/',dataset_idno,'/resources/download/',resource_id)
  url=nada_get_api_url(endpoint)

  httpResponse <- GET(url, add_headers("X-API-KEY" = api_key), accept_json(), verbose(nada_get_verbose()))
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
nada_admin_resource_add <- function(
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
    api_key=nada_get_api_key();
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
  else if(nada_is_valid_url(file_path)){
    options[['filename']]=file_path
  }

  url=nada_get_api_url(paste0('datasets/',idno,'/resources'))
  print(url)
  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body=options,
                       accept_json(),
                       verbose(nada_get_verbose()))

  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  output=list(
    "status_code"=httpResponse$status_code,
    "response"=nada_http_response_json(httpResponse)
  )

  return (output)
}


#' Delete External Resources
#'
#' Delete external resources for a study
#'
#' @return List with status_code and response
#' @param dataset_idno Study IDNo
#' @param resource_id Resource ID
#' @export
nada_admin_resource_delete <- function(dataset_idno, resource_id, api_key=NULL, api_base_url=NULL){

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  endpoint=paste0('datasets/',dataset_idno,'/resources/',resource_id)
  url=nada_get_api_url(endpoint)

  httpResponse <- DELETE(url, add_headers("X-API-KEY" = api_key), accept_json(), verbose(nada_get_verbose()))
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  output=list(
    "status_code"=httpResponse$status_code,
    "response"= nada_http_response_json(httpResponse)
  )

  return (output)
}



#' Delete all external resources for a study
#'
#' Delete all external resources for a study
#'
#' @return List with status_code and response
#' @param dataset_idno Study IDNo
#' @export
nada_admin_resource_delete_all <- function(dataset_idno, api_key=NULL, api_base_url=NULL){

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  endpoint=paste0('datasets/',dataset_idno,'/resources/delete_all')
  url=nada_get_api_url(endpoint)

  httpResponse <- DELETE(url, add_headers("X-API-KEY" = api_key), accept_json(), verbose(nada_get_verbose()))
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }else{
    output=fromJSON(content(httpResponse,"text"))
  }

  return (output)
}



#' Convert RDF/XML to List
#'
#' Convert RDF/XML to list
#'
#' @return List of resource metadata extracted from RDF file
#' @param rdf_file Path to RDF xml file
#' @export
nada_rdf_to_list <- function(rdf_file) {

  rdf <- xmlParse(rdf_file)
  rdf_l <- xmlToList(rdf)

  output=list()

  for(i in 1:length(rdf_l)) {
    for(j in 1:length(rdf_l[[i]])) {
      rdf_l[[i]][j] = trimws(rdf_l[[i]][j], which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
    }

    filepath = gsub("\\\\", "/", rdf_l[[i]]$.attrs)

    resource<-list(
      dctype = rdf_l[[i]]$type,
      dcformat = rdf_l[[i]]$format,
      title = rdf_l[[i]]$title,
      author = rdf_l[[i]]$creator,
      dcdate = rdf_l[[i]]$date,
      country = rdf_l[[i]]$spatial,
      language = rdf_l[[i]]$language,
      contributor = rdf_l[[i]]$contributor,
      publisher = rdf_l[[i]]$publisher,
      rights = rdf_l[[i]]$rights,
      description = rdf_l[[i]]$label,
      abstract = rdf_l[[i]]$abstract,
      toc = rdf_l[[i]]$toc,
      filename = filepath
    )

    if (nada_is_valid_url(filepath)){
      resource[['is_url']]=TRUE
    }else{
      resource[['is_url']]=FALSE
    }

    output[[length(output) + 1]] <- resource
  }

  return (output)
}
