# MongoDB index management for data tables (wraps /api/tables/indexes* endpoints)


# Build the full URL for a tables endpoint, honouring an explicit api_base_url
nada_data_table_index_url <- function(endpoint, api_base_url = NULL) {
  if (is.null(api_base_url)) {
    nada_get_api_url(endpoint)
  } else {
    paste0(sub("/+$", "", api_base_url), "/", endpoint)
  }
}

# Encode an id for use as a single URL path segment
nada_data_table_index_segment <- function(x, name) {
  if (is.null(x) || length(x) != 1 || is.na(x) || !nzchar(as.character(x))) {
    stop(paste0("`", name, "` is required."))
  }
  utils::URLencode(as.character(x), reserved = TRUE)
}

# Normalise index fields (character vector or comma-separated string) to the
# comma-separated string the API expects
nada_data_table_index_fields <- function(index_fields) {
  if (is.null(index_fields) || length(index_fields) == 0) {
    stop("`index_fields` is required.")
  }
  fields <- trimws(unlist(strsplit(as.character(index_fields), ",", fixed = TRUE)))
  fields <- fields[nzchar(fields)]
  if (length(fields) == 0) {
    stop("`index_fields` must contain at least one field name.")
  }
  paste(fields, collapse = ",")
}

# Wrap an httr response in the package's usual list(status_code, response)
nada_data_table_index_result <- function(httpResponse) {
  if (httpResponse$status_code != 200) {
    warning(content(httpResponse, "text"))
  }
  list(
    status_code = httpResponse$status_code,
    response    = nada_http_response_json(httpResponse)
  )
}


#' List indexes on a data table
#'
#' Returns the MongoDB indexes defined on a data table's collection.
#'
#' Wraps \code{GET /api/tables/indexes/{db_id}/{table_id}}.
#'
#' @param db_id (Required) Database ID
#' @param table_id (Required) Table ID
#' @param api_key API key (optional if API key is set using nada_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using nada_set_api_url)
#' @return List with \code{status_code} and \code{response}.
#'   \code{response$result} is a named list: index name -> key pattern
#'   (field -> 1, or field -> "text" for text indexes).
#'
#' @examples
#' \dontrun{
#' idx <- nada_admin_data_table_index_list("wdi", "population")
#' idx$response$result
#' }
#' @export
nada_admin_data_table_index_list <- function(db_id,
                                             table_id,
                                             api_key = NULL,
                                             api_base_url = NULL) {
  endpoint <- paste0("tables/indexes/",
                     nada_data_table_index_segment(db_id, "db_id"), "/",
                     nada_data_table_index_segment(table_id, "table_id"))
  if (is.null(api_key)) api_key <- nada_get_api_key()

  httpResponse <- GET(nada_data_table_index_url(endpoint, api_base_url),
                      add_headers("X-API-KEY" = api_key),
                      accept_json(),
                      verbose(nada_get_verbose()))

  nada_data_table_index_result(httpResponse)
}


#' Create an index on a data table
#'
#' Creates an ascending MongoDB index on one or more fields of a data table.
#' Several fields make one compound index, in the order given. The server
#' checks each field exists in the data (case-sensitive) and names the index
#' from the field names, e.g. \code{REF_AREA_TIME_PERIOD_1}.
#'
#' Wraps \code{POST /api/tables/indexes/{db_id}/{table_id}}.
#'
#' @param db_id (Required) Database ID
#' @param table_id (Required) Table ID
#' @param index_fields (Required) Field names, either as a character vector
#'   (\code{c("REF_AREA", "TIME_PERIOD")}) or a comma-separated string
#'   (\code{"REF_AREA,TIME_PERIOD"}). Field names are case-sensitive.
#' @param api_key API key (optional if API key is set using nada_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using nada_set_api_url)
#' @return List with \code{status_code} and \code{response}. On success
#'   \code{response$result} is the name of the created index.
#'
#' @examples
#' \dontrun{
#' # single-field index
#' nada_admin_data_table_index_create("wdi", "population", "REF_AREA")
#'
#' # compound index
#' nada_admin_data_table_index_create("wdi", "population",
#'                                    c("REF_AREA", "TIME_PERIOD"))
#' }
#' @seealso \code{\link{nada_admin_data_table_text_index_create}},
#'   \code{\link{nada_admin_data_table_index_list}}
#' @export
nada_admin_data_table_index_create <- function(db_id,
                                               table_id,
                                               index_fields,
                                               api_key = NULL,
                                               api_base_url = NULL) {
  endpoint <- paste0("tables/indexes/",
                     nada_data_table_index_segment(db_id, "db_id"), "/",
                     nada_data_table_index_segment(table_id, "table_id"))
  body <- list(index_fields = nada_data_table_index_fields(index_fields))
  if (is.null(api_key)) api_key <- nada_get_api_key()

  httpResponse <- POST(nada_data_table_index_url(endpoint, api_base_url),
                       add_headers("X-API-KEY" = api_key),
                       body = body,
                       encode = "json",
                       accept_json(),
                       verbose(nada_get_verbose()))

  nada_data_table_index_result(httpResponse)
}


#' Create a text index on a data table
#'
#' Creates a MongoDB text index on one or more fields of a data table, so the
#' fields can be used in full-text search. A MongoDB collection can hold only
#' one text index, so list every field you want searchable in the same call.
#' The server checks each field exists in the data (case-sensitive) and names
#' the index \code{text_<field1>_<field2>...}.
#'
#' Wraps \code{POST /api/tables/text_index/{db_id}/{table_id}}.
#'
#' @inheritParams nada_admin_data_table_index_create
#' @return List with \code{status_code} and \code{response}. On success
#'   \code{response$result} is the name of the created index.
#'
#' @examples
#' \dontrun{
#' nada_admin_data_table_text_index_create("wdi", "indicators",
#'                                         c("name", "description"))
#' }
#' @export
nada_admin_data_table_text_index_create <- function(db_id,
                                                    table_id,
                                                    index_fields,
                                                    api_key = NULL,
                                                    api_base_url = NULL) {
  endpoint <- paste0("tables/text_index/",
                     nada_data_table_index_segment(db_id, "db_id"), "/",
                     nada_data_table_index_segment(table_id, "table_id"))
  body <- list(index_fields = nada_data_table_index_fields(index_fields))
  if (is.null(api_key)) api_key <- nada_get_api_key()

  httpResponse <- POST(nada_data_table_index_url(endpoint, api_base_url),
                       add_headers("X-API-KEY" = api_key),
                       body = body,
                       encode = "json",
                       accept_json(),
                       verbose(nada_get_verbose()))

  nada_data_table_index_result(httpResponse)
}


#' Delete an index from a data table
#'
#' Drops one MongoDB index by name. Use
#' \code{\link{nada_admin_data_table_index_list}} to find index names.
#'
#' Wraps \code{DELETE /api/tables/indexes/{db_id}/{table_id}/{index_name}}.
#'
#' @param db_id (Required) Database ID
#' @param table_id (Required) Table ID
#' @param index_name (Required) Name of the index to drop, e.g.
#'   \code{"REF_AREA_TIME_PERIOD_1"}.
#' @param api_key API key (optional if API key is set using nada_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using nada_set_api_url)
#' @return List with \code{status_code} and \code{response}.
#'
#' @examples
#' \dontrun{
#' nada_admin_data_table_index_delete("wdi", "population", "REF_AREA_1")
#' }
#' @export
nada_admin_data_table_index_delete <- function(db_id,
                                               table_id,
                                               index_name,
                                               api_key = NULL,
                                               api_base_url = NULL) {
  if (identical(as.character(index_name), "_id_")) {
    stop("The `_id_` index cannot be dropped.")
  }
  if (identical(as.character(index_name), "all")) {
    # /tables/indexes/{db}/{table}/all is routed to the delete-all endpoint
    stop("An index named `all` cannot be dropped by name; use nada_admin_data_table_index_delete_all().")
  }
  endpoint <- paste0("tables/indexes/",
                     nada_data_table_index_segment(db_id, "db_id"), "/",
                     nada_data_table_index_segment(table_id, "table_id"), "/",
                     nada_data_table_index_segment(index_name, "index_name"))
  if (is.null(api_key)) api_key <- nada_get_api_key()

  httpResponse <- DELETE(nada_data_table_index_url(endpoint, api_base_url),
                         add_headers("X-API-KEY" = api_key),
                         accept_json(),
                         verbose(nada_get_verbose()))

  nada_data_table_index_result(httpResponse)
}


#' Delete all indexes from a data table
#'
#' Drops every MongoDB index on a data table except the built-in \code{_id_}
#' index.
#'
#' Wraps \code{POST /api/tables/indexes/{db_id}/{table_id}/all}.
#'
#' @param db_id (Required) Database ID
#' @param table_id (Required) Table ID
#' @param api_key API key (optional if API key is set using nada_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using nada_set_api_url)
#' @return List with \code{status_code} and \code{response}.
#'   \code{response$result} has \code{indexes_dropped} and
#'   \code{indexes_remaining}.
#'
#' @examples
#' \dontrun{
#' nada_admin_data_table_index_delete_all("wdi", "population")
#' }
#' @export
nada_admin_data_table_index_delete_all <- function(db_id,
                                                   table_id,
                                                   api_key = NULL,
                                                   api_base_url = NULL) {
  endpoint <- paste0("tables/indexes/",
                     nada_data_table_index_segment(db_id, "db_id"), "/",
                     nada_data_table_index_segment(table_id, "table_id"), "/all")
  if (is.null(api_key)) api_key <- nada_get_api_key()

  httpResponse <- POST(nada_data_table_index_url(endpoint, api_base_url),
                       add_headers("X-API-KEY" = api_key),
                       accept_json(),
                       verbose(nada_get_verbose()))

  nada_data_table_index_result(httpResponse)
}


#' Create indexes on the table dictionary collection
#'
#' One-time setup that creates the indexes NADA uses on its internal
#' \code{table_dictionary} collection (field lookups and text search across
#' table fields). Safe to re-run; existing indexes are kept.
#'
#' Wraps \code{POST /api/tables/create_dictionary_indexes}.
#'
#' @param api_key API key (optional if API key is set using nada_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using nada_set_api_url)
#' @return List with \code{status_code} and \code{response}.
#'
#' @examples
#' \dontrun{
#' nada_admin_data_table_dictionary_indexes_create()
#' }
#' @export
nada_admin_data_table_dictionary_indexes_create <- function(api_key = NULL,
                                                            api_base_url = NULL) {
  if (is.null(api_key)) api_key <- nada_get_api_key()

  httpResponse <- POST(nada_data_table_index_url("tables/create_dictionary_indexes", api_base_url),
                       add_headers("X-API-KEY" = api_key),
                       accept_json(),
                       verbose(nada_get_verbose()))

  nada_data_table_index_result(httpResponse)
}
