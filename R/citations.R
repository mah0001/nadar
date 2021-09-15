Citations_create – create new citation [you can add an overwrite option to update/replace an existing citation]
Citations_update – update a citation
Citations – list all citations
Citations_delete – delete a citation
Citations_attach – to attach a citation to one or more studies
Citations_info – get detailed metadata for a single citation

library(nadar)

# link to API documentation
# http://training.ihsn.org/api-documentation/catalog-admin/

# Set API key and URL
my_keys <- read.csv("C:/Users/wb460271/OneDrive - WBG/Documents/TB_NADA_API_keys.csv", header=T, stringsAsFactors=F)

set_api_key(my_keys[2, "Key"])
set_api_url(my_keys[2, "URL"])

# View URL
get_api_url()


#' GetCitations
#'
#' Search citations or get list of all citations
#'
#' @return List of studies or a single study info
#' @param keyword (Optional) Dataset IDNo (string)
#' @param from (Optional) Start year (integer)
#' @param to (Optional) End year (integer)
#' @param api_key API key (optional if API key is set using set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using set_api_url)
#' @export
citations <- function(keyword=NULL,
                     from=1900,
                     to=2100,
                     api_key=NULL,
                     api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  options=list(
    "q"=keyword,
    "from"=from,
    "to"=to
  )

  # Create url
  endpoint <- 'citations/'
  if(is.null(api_base_url)){
    url=get_api_url(endpoint=endpoint)
  } else {
    url = paste0(api_base_url,"/",endpoint)
  }

  httpResponse <- GET(url,
                      add_headers("X-API-KEY" = api_key),
                      body=options,
                      accept_json())
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
    stop(content(httpResponse, "text"), call. = FALSE)
  }

  output=fromJSON(content(httpResponse,"text"))
  #return (output)

  structure(
    list(
      content = output,
      api_url = url,
      status_code = httpResponse$status_code
    ),
    class = "nada_datasets"
  )
}

# Test citations()
#res <- citations()
#res <- citations(keyword = "Botswana")
#dim(res$content$citations)

#' CitationInfo
#'
#' Get detailed metadata on a single citation
#'
#' @param uuid (required) Unique user defined id (string) (is actually assigned id)
#' @param api_key API key (optional if API key is set using set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using set_api_url)
#' @return Detailed metadata on single citation
#' @export
citation_info <- function(uuid=NULL,
                      api_key=NULL,
                      api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  # Create url
  endpoint <- paste0('citations/', uuid)
  if(is.null(api_base_url)){
    url=get_api_url(endpoint=endpoint)
  } else {
    url = paste0(api_base_url,"/",endpoint)
  }

  httpResponse <- GET(url,
                      add_headers("X-API-KEY" = api_key),
                      encode="json",
                      accept_json())

  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
    stop(content(httpResponse, "text"), call. = FALSE)
  }

  output=fromJSON(content(httpResponse,"text"))
  #return (output)

  structure(
    list(
      content = output,
      api_url = url,
      status_code = httpResponse$status_code
    ),
    class = "nada_datasets"
  )
}

# Test citationinfo
citation_info("2")

#' Create citation
#'
#' Create a new citation
#'
#' @param uuid (required) Unique user defined id (string)
#' @param ctype (required) Type of citation - valid values: "book", "book-section", "report",
#' "anthology-editor", "anthology-translator", "corporate-author", "journal", "working-paper",
#' "conference-paper", "magazine", "newspaper", "website", "website-doc", "thesis"
#' @param title (Collection ID that owns the study)required) Title (string)
#' @param api_key API key (optional if API key is set using set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using set_api_url)
#' @return NULL
#'
#' @examples

#' @export
citation_create <- function(
  id,
  ctype,
  uuid,
  title,
  api_key=NULL,
  api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  options=list(
    "uuid"=uuid,
    "ctype"=ctype,
    "title"=title
  )

  #options= c(options,metadata)

  # Create url
  endpoint <- 'citations/'
  if(is.null(api_base_url)){
    url=get_api_url(endpoint=endpoint)
  } else {
    url = paste0(api_base_url,"/",endpoint)
  }

  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body=options,
                       #content_type_json(),
                       encode="json",
                       accept_json(),
                       verbose(get_verbose()))

  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  output=list(
    "status_code"=httpResponse$status_code,
    fromJSON(content(httpResponse,"text"))
    #"response"= nada_http_response_json(httpResponse),
  )

  return(output)
}

#' Delete citation
#'
#' Delete citation
#'
#' @param uuid (required) Unique user defined id (string)
#' @param api_key API key (optional if API key is set using set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using set_api_url)
#' @return NULL
#'
#' @examples
#'
#' citation_delete(
#'   uuid="citation_id"
#' )
#'
#' @export
citation_delete <- function(uuid,
                             api_key=NULL,
                             api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  # Create url
  endpoint <- paste0('citations/delete/',uuid)
  if(is.null(api_base_url)){
    url=get_api_url(endpoint=endpoint)
  } else {
    url = paste0(api_base_url,"/",endpoint)
  }

  httpResponse <- PUT(url,
                      add_headers("X-API-KEY" = api_key),
                      encode="json",
                      accept_json(),
                      verbose(get_verbose()))

  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  output=list(
    "status_code"=httpResponse$status_code#,
    #"response"=fromJSON(content(httpResponse,"text"))
  )

  return(output)
}
