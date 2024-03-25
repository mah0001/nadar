#' List catalog entries
#'
#' List all entries in the catalog
#'
#' @return List of studies or a single study info
#' @param idno (Optional) Dataset IDNo
#' @export
catalog_list <- function(idno=NULL, api_key=NULL, api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  if(!is.null(idno)){
    endpoint=paste0(endpoint,'/',idno)
  }

  url=get_api_url('datasets')
  httpResponse <- GET(url, add_headers("X-API-KEY" = api_key), accept_json())
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


#' Delete a catalog study
#'
#' Delete a single entry from the catalog
#'
#' @return list
#' @param idno (Required) Dataset IDNo
#'
#' @export
catalog_delete <- function(idno, api_key=NULL, api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  url=get_api_url(paste0('datasets/', idno))
  httpResponse <- DELETE(url, add_headers("X-API-KEY" = api_key), accept_json())
  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
    stop(content(httpResponse, "text"), call. = FALSE)
  }

  output=fromJSON(content(httpResponse,"text"))
  return (output)
}


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
#'   \item collection - Filter by one or more collections. e.g. `collection1, collection2`
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
#' catalog_search (
#'   options=list(
#'     sk="health survey"
#'   ),
#'   ps=1000
#' )
#'
#' #example - keywords search + filter by country and collection
#'
#' catalog_search (
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
catalog_search <- function(
                     options=list(
                       sk=NULL,
                       from=NULL,
                       to=NULL,
                       country=NULL,
                       collection=NULL,
                       created=NULL,
                       dtype=NULL
                     ),
                     page=0,
                     ps=50,
                     sort_by=NULL,
                     sort_order=NULL,
                     api_key=NULL,
                     api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  params<-c()
  for (param in names(options)) {
    if (!is.null(options[[param]])){
      params<-append(params,paste0(param,'=',URLencode(options[[param]],reserved = TRUE)))
    }
  }

  params=paste(params, collapse="&");

  print(params)

  if(ps <= 100){ # up to 500 one API call
    endpoint <- paste0("catalog", "?page=", page, "&ps=", ps, '&',params)
  }else { # if more than 500 requested, multiple API calls
    endpoint=paste0("catalog", "?page=", page, "&ps=100", '&',params)
  }

  # Create url
  if(is.null(api_base_url)){
    url <- get_api_url(endpoint = endpoint)
  } else {
    url <- paste0(api_base_url,"/",endpoint)
  }

  httpResponse <- GET(url, add_headers("X-API-KEY" = api_key), accept_json(), verbose(get_verbose()))
  output <- NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
    stop(content(httpResponse, "text"), call. = FALSE)
  }

  output <- fromJSON(content(httpResponse,"text"))

  # add more API calls if limit > 500
  if(ps > 100){
    cur_datasets <- output$result$rows # adding result datasets for each call
    num_entries_to_add <- min(ps, as.integer(output$result$found)) - 100 # number of entries to add (max of limit and available entries)

    df_column_names <- colnames(cur_datasets)

    if (page==0){
      page=1
    }

    while(output$result$found > 0 & num_entries_to_add > 0){ # while more entires to add
      page <- page + 1 # update offset
      endpoint <- paste0("catalog", "?page=", page, "&ps=100",'&',params)

      # Create URL
      if(is.null(api_base_url)){
        url <- get_api_url(endpoint = endpoint)
      } else {
        url <- paste0(api_base_url,"/",endpoint)
      }

      # API call
      httpResponse <- GET(url, add_headers("X-API-KEY" = api_key), accept_json(), verbose(get_verbose()))

      if(httpResponse$status_code!=200){
        warning(content(httpResponse, "text"))
        stop(content(httpResponse, "text"), call. = FALSE)
      }

      output <- fromJSON(content(httpResponse,"text"))

      output_ds <- subset(output$result$rows, select = df_column_names)
      cur_datasets <- rbind(cur_datasets, output_ds) # combine results
      num_entries_to_add <- num_entries_to_add - 100 # update number of entries to add
    }

    output$datasets <- cur_datasets
  }

  structure(
    list(
      content = output,
      api_url = url,
      status_code = httpResponse$status_code
    ),
    class = "nada_catalog_search"
  )
}


#' Find a study by IDNO
#'
#' Find study by IDNO
#'
#' @return list
#'
#' @export
catalog_find_by_idno <- function(idno){
  return ("TODO")
}

#' @export
catalog_find_by_id <- function(id){
  return ("TODO")
}


#' Replace study IDNO
#'
#' Replace Study IDNO
#'
#' @return list
#'
#' @export
replace_idno <- function(old_idno,new_idno,api_key=NULL,api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  options=list(
    old_idno=old_idno,
    new_idno=new_idno
  )

  url=get_api_url('datasets/replace_idno')
  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body=options,
                       content_type_json(),
                       encode="json",
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


#' Get study metadata as JSON
#'
#' Get study metadata as JSON
#'
#' @return Metadata as JSON
#' @param idno Dataset IDNo
#' @param is_legacy TRUE | FALSE - if using NADA < 5.3, use legacy as TRUE
#' @export
study_json <- function(idno,is_legacy=FALSE, api_key=NULL,api_base_url=NULL){

    if(is.null(api_key)){
      api_key=get_api_key();
    }


    if (is_legacy==FALSE){
      json_metadata<-nadar::nada_http_get(paste0('catalog/json/',idno))

      if (is.null(json_metadata$response)){
        stop(paste("Failed to get study metadata:",idno))
      }


      #remove id, sid
      if (!is.null(json_study$data_files)){
        json_study$data_files <- subset(metadata$data_files, select = -c(id, sid))
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
write_study_json<-function(idno,output_file,is_legacy=FALSE,api_key=NULL, api_base_url=NULL){
  json_metadata=study_json(idno,api_key=api_key, is_legacy=is_legacy, api_base_url=api_base_url)
  write(jsonlite::toJSON(json_metadata,auto_unbox=TRUE), output_file)
}
