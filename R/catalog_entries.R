#' Search catalog
#'
#' Search catalog
#'
#' @param options - Search options:
#' \itemize{
#'   \item sk - Keywords
#'   \item from - Year from
#'   \item to | Year to
#'   \item country | Country names or codes. To search for multiple countries, use pipe (|) to separate them e.g. Albania|Turkey
#'   \item collection - Filter by one or more nada_collection_list. e.g. `collection1, collection2`
#'   \item created - Filter by date of creation. Use date format YYYY-MM-DD. For example, `2020/04/01` returns rows created on and after the date. To specify a date range, use 2020/04/01-2020/04/15
#'   \item dtype - Data access types (`open`, `direct`, `public`, `licensed`, `remote`). For multiple values, use comma e.g. `open,direct`
#' }
#' @param page Page
#' @param ps Page size (number of rows per page)
#' @param sort_by Sort results by field, support fields are `title`,`year`,`nation`
#' @param sort_order Sort results order, valid options are `asc`, `desc`
#'
#' @examples
#'
#' #example - keywords search
#'
#' nada_study_search (
#'   options=list(
#'     sk="health survey"
#'   ),
#'   ps=1000
#' )
#'
#' #example - keywords search + filter by country and collection
#'
#' nada_study_search (
#'   options=list(
#'     sk="health survey",
#'     country="albania|afghanistan",
#'     collection="dhs"
#'   ),
#'   ps=1000
#' )
#'
#'
#'
#'
#' @return list
#'
#' @export
nada_study_search <- function(
                     options = list(
                       sk = NULL,
                       from = NULL,
                       to = NULL,
                       country = NULL,
                       collection = NULL,
                       created = NULL,
                       dtype = NULL
                     ),
                     page = 0,
                     ps = 50,
                     sort_by = NULL,
                     sort_order = NULL,
                     api_key = NULL,
                     api_base_url = NULL){

  MAX_SINGLE_REQUEST <- 100
  if (page < 0) {
    stop("page must be >= 0", call. = FALSE)
  }
  if (ps <= 0) {
    stop("ps must be > 0", call. = FALSE)
  }
  if (!is.null(sort_order) && !sort_order %in% c("asc", "desc")) {
    stop("sort_order must be 'asc' or 'desc'", call. = FALSE)
  }

  if (is.null(api_key)) {
    api_key <- nada_get_api_key()
  }

  params <- c()
  for (param in names(options)) {
    if (!is.null(options[[param]])) {
      params <- append(params, paste0(param, '=', URLencode(options[[param]], reserved = TRUE)))
    }
  }

  if (!is.null(sort_by)) {
    params <- append(params, paste0("sort_by=", sort_by))
  }
  if (!is.null(sort_order)) {
    params <- append(params, paste0("sort_order=", sort_order))
  }

  params <- paste(params, collapse = "&")

  if (ps <= MAX_SINGLE_REQUEST) {
    endpoint <- paste0("catalog", "?page=", page, "&ps=", ps, '&', params)
  } else {
    endpoint <- paste0("catalog", "?page=", page, "&ps=", MAX_SINGLE_REQUEST, '&', params)
  }

  handle_api_error <- function(response, context = "") {
    if (response$status_code != 200) {
      error_msg <- content(response, "text")
      warning(paste(context, error_msg))
      stop(paste(context, error_msg), call. = FALSE)
    }
  }

  if (is.null(api_base_url)) {
    url <- nada_get_api_url(endpoint = endpoint)
  } else {
    url <- paste0(api_base_url, "/", endpoint)
  }

  httpResponse <- GET(url, add_headers("X-API-KEY" = api_key), accept_json(), verbose(nada_get_verbose()))
  handle_api_error(httpResponse, "Initial API call failed:")

  output <- fromJSON(content(httpResponse, "text"))

  total_found <- as.integer(output$result$found)
  rows_returned <- nrow(output$result$rows)
  found_pages <- ceiling(total_found / MAX_SINGLE_REQUEST)
  ps_used <- if (ps > MAX_SINGLE_REQUEST) MAX_SINGLE_REQUEST else ps
  pages_fetched <- 1

  if (ps > MAX_SINGLE_REQUEST) {
    all_datasets <- output$result$rows
    num_entries_to_add <- min(ps, total_found) - MAX_SINGLE_REQUEST

    if (total_found <= MAX_SINGLE_REQUEST) {
      num_entries_to_add <- 0
    }

    df_column_names <- colnames(all_datasets)
    current_page <- 1

    while (num_entries_to_add > 0 && current_page <= found_pages) {
      endpoint <- paste0("catalog", "?page=", current_page, "&ps=", MAX_SINGLE_REQUEST, '&', params)

      if (is.null(api_base_url)) {
        url <- nada_get_api_url(endpoint = endpoint)
      } else {
        url <- paste0(api_base_url, "/", endpoint)
      }

      httpResponse <- GET(url, add_headers("X-API-KEY" = api_key), accept_json(), verbose(nada_get_verbose()))
      handle_api_error(httpResponse, paste("Pagination API call failed (page", current_page, "):"))

      page_output <- fromJSON(content(httpResponse, "text"))

      if (is.null(page_output$result$rows) || nrow(page_output$result$rows) == 0) {
        break
      }

      page_datasets <- subset(page_output$result$rows, select = df_column_names)
      all_datasets <- rbind(all_datasets, page_datasets)
      num_entries_to_add <- num_entries_to_add - nrow(page_datasets)
      current_page <- current_page + 1
      pages_fetched <- pages_fetched + 1
    }

    output$result$rows <- all_datasets
    rows_returned <- nrow(all_datasets)
  }

  structure(
    list(
      content = output,
      api_url = url,
      status_code = httpResponse$status_code,
      search_info = list(
        rows_found = total_found,
        rows_returned = rows_returned,
        ps_requested = ps,
        ps_used = ps_used,
        found_pages = found_pages,
        pages_fetched = pages_fetched
      )
    ),
    class = "nada_catalog_search"
  )
}


#' Download DDI metadata
#'
#' Download DDI/XML by Study IDNO or Direct URL
#'
#' @param idno Study IDNo (ignored if ddi_url is provided)
#' @param output_file Optional output file path to save DDI content
#' @param ddi_url Optional direct URL to download DDI from
#' @param api_key API key
#' @param api_base_url Base URL for the API
#' @return DDI XML content as character string (if no output_file) or file path (if output_file provided)
#' @export
nada_study_download_ddi <- function(idno, output_file = NULL, ddi_url = NULL, api_key = NULL, api_base_url = NULL) {

  if (is.null(api_key)) {
    api_key <- nada_get_api_key()
  }

  if (!is.null(ddi_url)) {
    url <- ddi_url
  } else {
    endpoint <- paste0("catalog/ddi/", idno)

    if (is.null(api_base_url)) {
      url <- nada_get_api_url(endpoint = endpoint)
    } else {
      url <- paste0(api_base_url, "/", endpoint)
    }
  }

  if (is.null(output_file)) {
    output_file <- tempfile(fileext = ".xml")
  }

  headers <- if (!is.null(api_key)) c("X-API-KEY" = api_key) else NULL
  
  download.file(url, output_file,
                method = "curl",
                headers = headers,
                quiet = !nada_get_verbose())

  result <- list(
    file_path = output_file,
    api_url = url,
    status_code = 200,
    idno = if (!is.null(ddi_url)) NULL else idno
  )

  structure(result, class = "nada_ddi_download")
}


#' Download RDF/XML by Study IDNO or Direct URL
#'
#' @param idno Study IDNo (ignored if rdf_url is provided)
#' @param output_file Optional output file path to save RDF content
#' @param rdf_url Optional direct URL to download RDF from
#' @param api_key API key
#' @param api_base_url Base URL for the API
#' @return RDF XML content as character string (if no output_file) or file path (if output_file provided)
#' @export
nada_study_download_rdf <- function(idno, output_file = NULL, rdf_url = NULL, api_key = NULL, api_base_url = NULL) {

  if (is.null(api_key)) {
    api_key <- nada_get_api_key()
  }

  if (!is.null(rdf_url)) {
    url <- rdf_url
  } else {
    endpoint <- paste0("catalog/rdf/", idno)

    if (is.null(api_base_url)) {
      url <- nada_get_api_url(endpoint = endpoint)
    } else {
      url <- paste0(api_base_url, "/", endpoint)
    }
  }

  if (is.null(output_file)) {
    output_file <- tempfile(fileext = ".rdf")
  }

  headers <- if (!is.null(api_key)) c("X-API-KEY" = api_key) else NULL
  
  download.file(url, output_file,
                method = "curl",
                headers = headers,
                quiet = !nada_get_verbose())

  result <- list(
    file_path = output_file,
    api_url = url,
    status_code = 200,
    idno = if (!is.null(rdf_url)) NULL else idno
  )

  structure(result, class = "nada_rdf_download")
}





#' Find a study by IDNO
#'
#' Find study by IDNO
#'
#' @return list
#'
#' @export
nada_study_get_by_idno <- function(idno){
  return ("TODO")
}

#' Find a study by ID
#'
#' Find study by ID
#'
#' @return list
#'
#' @export
nada_study_get_by_id <- function(id){
  return ("TODO")
}


#' Replace study IDNO
#'
#' Replace Study IDNO
#'
#' @return list
#'
#' @export
nada_admin_study_replace_idno <- function(old_idno,new_idno,api_key=NULL,api_base_url=NULL){

  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  options=list(
    old_idno=old_idno,
    new_idno=new_idno
  )

  url=nada_get_api_url('datasets/replace_idno')
  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body=options,
                       content_type_json(),
                       encode="json",
                       verbose(nada_get_verbose()))

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


#' Get study metadata as JSON
#'
#' Get study metadata as JSON
#'
#' @return Metadata as JSON
#' @param idno Dataset IDNo
#' @param is_legacy TRUE | FALSE - if using NADA < 5.3, use legacy as TRUE
#' @export
nada_admin_study_get_json <- function(idno,is_legacy=FALSE, api_key=NULL,api_base_url=NULL){

    if(is.null(api_key)){
      api_key=nada_get_api_key();
    }


    if (is_legacy==FALSE){
      json_metadata<-nadar::nada_http_get(paste0('catalog/json/',idno))

      if (is.null(json_metadata$response)){
        stop(paste("Failed to get study metadata:",idno))
      }


      #remove id, sid
      if (!is.null(json_metadata$data_files)){
        json_metadata$data_files <- subset(json_metadata$data_files, select = -c(id, sid))
      }

      return (json_metadata$response)
    }


    # for older/legacy versions of NADA
    result<-nadar::nada_http_get(paste0('catalog/',idno))

    #hold json metadata for the study
    json_study=list()

    if (is.null(result$response$dataset$metadata)){
      stop(paste("Failed to get study metadata:","IDNO") )
    }else{
      json_study=result$response$dataset$metadata
    }

    # get data files
    data_files<-nadar::nada_http_get(paste0('catalog/data_files/',idno))

    if (!is.null(data_files$response$datafiles)){
      json_study$data_files=data_files$response$datafiles

      #remove id, sid
      json_study$data_files <- subset(json_study$data_files, select = -c(id, sid))
    }

    # read variables
    variables<-nadar::nada_http_get(paste0('catalog/variables/',idno))

    if (!is.null(variables$response$variables)){

      json_study$variables=list()

      for(i in 1:nrow(variables$response$variables)){
        vid=variables$response$variables$vid[i]

        #get variable metadata
        variable<-nadar::nada_http_get(paste0('catalog/variable/',idno,"/",vid))

        if (!is.null(variable$response$variable$metadata)){
          json_study$variables<-list.append(json_study$variables, variable$response$variable$metadata)
        }
      }

      # remove var_format column as the field has changed
      if (any(names(json_study$variables) == 'var_format')){
        json_study$variables<-subset(json_study$variables, select=-c(var_format))
      }

    }

    return (json_study)
}



#' Write study metadata as JSON
#'
#' Write study metadata as JSON
#'
#' @return None
#' @param idno Dataset IDNo
#' @param output_file Path to the output file
#' @param is_legacy TRUE | FALSE - if using NADA < 5.3, use legacy as TRUE
#' @export
nada_admin_study_write_json<-function(idno,output_file,is_legacy=FALSE,api_key=NULL, api_base_url=NULL){
  json_metadata=nada_admin_study_get_json(idno,api_key=api_key, is_legacy=is_legacy, api_base_url=api_base_url)
  write(jsonlite::toJSON(json_metadata,auto_unbox=TRUE), output_file)
}
