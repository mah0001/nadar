#' List codelists
#'
#' Endpoint: GET /api/admin/codelists
#' Without `page`, returns the full catalogue; with `page`, returns one page plus total/page/per_page.
#'
#' @param page Optional 1-based page number.
#' @param per_page Page size (default 50, max 200 server-side). Used only when `page` is set.
#' @param search Optional search term filtering name/idno/agency/version/description.
#' @param with_counts When TRUE (default), include item_count and group_count for each codelist.
#' @param api_key,api_base_url See `nada_admin_dsd_list`.
#'
#' @return list with status_code and response
#' @export
nada_admin_codelist_list <- function(page = NULL, per_page = NULL, search = NULL,
                                     with_counts = TRUE,
                                     api_key = NULL, api_base_url = NULL) {
  if (is.null(api_key)) api_key <- nada_get_api_key()
  url <- if (is.null(api_base_url)) nada_get_api_url("admin/codelists") else paste0(api_base_url, "/admin/codelists")

  qp <- list(with_counts = ifelse(isTRUE(with_counts), "1", "0"))
  if (!is.null(page))     qp$page     <- page
  if (!is.null(per_page)) qp$per_page <- per_page
  if (!is.null(search))   qp$search   <- search

  httpResponse <- GET(url,
                      add_headers("X-API-KEY" = api_key),
                      accept_json(),
                      query = qp,
                      verbose(nada_get_verbose()))

  list(status_code = httpResponse$status_code, response = nada_http_response_json(httpResponse))
}

#' Get a codelist (by id, idno, or name)
#'
#' Endpoint (one of):
#' - GET /api/admin/codelists/item/{id}
#' - GET /api/admin/codelists/by_idno/{idno}
#' - GET /api/admin/codelists/by_name/{name}?agency=&version=
#'
#' The response includes nested `items` and `groups`.
#'
#' @param id Numeric id (mutually exclusive with idno/name).
#' @param idno Catalogue idno (mutually exclusive with id/name).
#' @param name Maintainable name, requires `agency`/`version` to disambiguate (defaults NADA/1.0).
#' @param agency Used with `name` (default "NADA").
#' @param version Used with `name` (default "1.0").
#' @param api_key,api_base_url See `nada_admin_dsd_list`.
#'
#' @return list with status_code and response
#' @export
nada_admin_codelist_get <- function(id = NULL, idno = NULL, name = NULL,
                                    agency = "NADA", version = "1.0",
                                    api_key = NULL, api_base_url = NULL) {
  n_given <- sum(!is.null(id), !is.null(idno), !is.null(name))
  if (n_given != 1) stop("Provide exactly one of `id`, `idno`, or `name`.")
  if (is.null(api_key)) api_key <- nada_get_api_key()

  qp <- list()
  if (!is.null(id)) {
    endpoint <- paste0("admin/codelists/item/", id)
  } else if (!is.null(idno)) {
    endpoint <- paste0("admin/codelists/by_idno/", utils::URLencode(idno, reserved = TRUE))
  } else {
    endpoint <- paste0("admin/codelists/by_name/", utils::URLencode(name, reserved = TRUE))
    qp$agency  <- agency
    qp$version <- version
  }
  url <- if (is.null(api_base_url)) nada_get_api_url(endpoint) else paste0(api_base_url, "/", endpoint)

  httpResponse <- GET(url,
                      add_headers("X-API-KEY" = api_key),
                      accept_json(),
                      query = qp,
                      verbose(nada_get_verbose()))

  list(status_code = httpResponse$status_code, response = nada_http_response_json(httpResponse))
}

#' Create a codelist (row only, no items)
#'
#' Endpoint: POST /api/admin/codelists
#' Use `nada_admin_codelist_item_add()` to insert items after creation,
#' or `nada_admin_dsd_import_json()` to create a codelist together with a DSD import.
#'
#' @param name Maintainable name (required).
#' @param idno Catalogue idno.
#' @param agency Agency (default "NADA").
#' @param version Version (default "1.0").
#' @param description Description.
#' @param api_key,api_base_url See `nada_admin_dsd_list`.
#'
#' @return list with status_code and response
#' @export
nada_admin_codelist_create <- function(name, idno = NULL, agency = "NADA", version = "1.0",
                                       description = NULL,
                                       api_key = NULL, api_base_url = NULL) {
  if (is.null(api_key)) api_key <- nada_get_api_key()
  url <- if (is.null(api_base_url)) nada_get_api_url("admin/codelists") else paste0(api_base_url, "/admin/codelists")

  body <- list(name = name, agency = agency, version = version,
               idno = idno, description = description)
  body <- body[!vapply(body, is.null, logical(1))]

  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body = body,
                       content_type_json(),
                       encode = "json",
                       accept_json(),
                       verbose(nada_get_verbose()))

  list(status_code = httpResponse$status_code, response = nada_http_response_json(httpResponse))
}

#' Update a codelist
#'
#' Endpoint: PUT /api/admin/codelists/item/{id}
#'
#' @param id Numeric id.
#' @param fields Named list of fields to update (name, idno, agency, version, description).
#' @param api_key,api_base_url See `nada_admin_dsd_list`.
#'
#' @return list with status_code and response
#' @export
nada_admin_codelist_update <- function(id, fields, api_key = NULL, api_base_url = NULL) {
  if (is.null(api_key)) api_key <- nada_get_api_key()
  if (!is.list(fields) || length(fields) == 0) stop("`fields` must be a non-empty named list.")
  endpoint <- paste0("admin/codelists/item/", id)
  url <- if (is.null(api_base_url)) nada_get_api_url(endpoint) else paste0(api_base_url, "/", endpoint)

  httpResponse <- PUT(url,
                      add_headers("X-API-KEY" = api_key),
                      body = fields,
                      content_type_json(),
                      encode = "json",
                      accept_json(),
                      verbose(nada_get_verbose()))

  list(status_code = httpResponse$status_code, response = nada_http_response_json(httpResponse))
}

#' Delete a codelist
#'
#' Endpoint: DELETE /api/admin/codelists/item/{id}
#'
#' @param id Numeric id.
#' @param api_key,api_base_url See `nada_admin_dsd_list`.
#'
#' @return list with status_code and response
#' @export
nada_admin_codelist_delete <- function(id, api_key = NULL, api_base_url = NULL) {
  if (is.null(api_key)) api_key <- nada_get_api_key()
  endpoint <- paste0("admin/codelists/item/", id)
  url <- if (is.null(api_base_url)) nada_get_api_url(endpoint) else paste0(api_base_url, "/", endpoint)

  httpResponse <- DELETE(url,
                         add_headers("X-API-KEY" = api_key),
                         accept_json(),
                         verbose(nada_get_verbose()))

  list(status_code = httpResponse$status_code, response = nada_http_response_json(httpResponse))
}

#' List items of a codelist
#'
#' Endpoint: GET /api/admin/codelists/item/{codelist_id}/items
#'
#' @param codelist_id Numeric codelist id.
#' @param view One of "full" (default, full rows) or "flat" (list of `{value,label}`).
#' @param page Optional 1-based page (only with `view = "flat"`).
#' @param per_page Page size when paging (default 50 server-side).
#' @param search Optional search filter on code/title (only with `view = "flat"`).
#' @param with_translations When view = "full", include translations (default TRUE).
#' @param api_key,api_base_url See `nada_admin_dsd_list`.
#'
#' @return list with status_code and response
#' @export
nada_admin_codelist_items_list <- function(codelist_id, view = "full",
                                           page = NULL, per_page = NULL, search = NULL,
                                           with_translations = TRUE,
                                           api_key = NULL, api_base_url = NULL) {
  if (is.null(api_key)) api_key <- nada_get_api_key()
  endpoint <- paste0("admin/codelists/item/", codelist_id, "/items")
  url <- if (is.null(api_base_url)) nada_get_api_url(endpoint) else paste0(api_base_url, "/", endpoint)

  qp <- list()
  if (identical(view, "flat")) {
    qp$view <- "flat"
    if (!is.null(page))     qp$page     <- page
    if (!is.null(per_page)) qp$per_page <- per_page
    if (!is.null(search))   qp$search   <- search
  } else {
    qp$with_translations <- ifelse(isTRUE(with_translations), "1", "0")
  }

  httpResponse <- GET(url,
                      add_headers("X-API-KEY" = api_key),
                      accept_json(),
                      query = qp,
                      verbose(nada_get_verbose()))

  list(status_code = httpResponse$status_code, response = nada_http_response_json(httpResponse))
}

#' Add an item to a codelist
#'
#' Endpoint: POST /api/admin/codelists/item/{codelist_id}/items
#'
#' @param codelist_id Numeric codelist id.
#' @param code Item code (required).
#' @param title Display label.
#' @param parent_id Parent codelist_item id (for hierarchies).
#' @param sort_order Sort order within the codelist.
#' @param api_key,api_base_url See `nada_admin_dsd_list`.
#'
#' @return list with status_code and response
#' @export
nada_admin_codelist_item_add <- function(codelist_id, code, title = NULL,
                                         parent_id = NULL, sort_order = NULL,
                                         api_key = NULL, api_base_url = NULL) {
  if (is.null(api_key)) api_key <- nada_get_api_key()
  endpoint <- paste0("admin/codelists/item/", codelist_id, "/items")
  url <- if (is.null(api_base_url)) nada_get_api_url(endpoint) else paste0(api_base_url, "/", endpoint)

  body <- list(code = code, title = title, parent_id = parent_id, sort_order = sort_order)
  body <- body[!vapply(body, is.null, logical(1))]

  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body = body,
                       content_type_json(),
                       encode = "json",
                       accept_json(),
                       verbose(nada_get_verbose()))

  list(status_code = httpResponse$status_code, response = nada_http_response_json(httpResponse))
}

#' Import one codelist and its items from JSON
#'
#' Wraps POST /api/admin/codelists/import_json. Body fields `overwrite` and `dry_run`
#' are sent in the JSON body only (no query parameters).
#'
#' @param payload Named list with `codelist` (nested object with idno/name/items, etc.),
#'   optional `overwrite` and `dry_run` booleans, OR a legacy flat list accepted by the API
#'   when it is unambiguous.
#' @param file Path to a JSON file (mutually exclusive with `payload`).
#' @param dry_run If not `NULL`, sets body field `dry_run`.
#' @param overwrite If not `NULL`, sets body field `overwrite` (replace items on matched list).
#' @param api_key,api_base_url See `nada_admin_dsd_list`.
#'
#' @return list with status_code and response
#' @export
nada_admin_codelist_import_json <- function(payload = NULL, file = NULL,
                                            dry_run = NULL, overwrite = NULL,
                                            api_key = NULL, api_base_url = NULL) {
  if (is.null(payload) && is.null(file)) {
    stop("Provide either `payload` (list) or `file` (path to a JSON file).")
  }
  if (!is.null(payload) && !is.null(file)) {
    stop("Provide only one of `payload` or `file`, not both.")
  }
  if (is.null(api_key)) api_key <- nada_get_api_key()

  endpoint <- "admin/codelists/import_json"
  url <- if (is.null(api_base_url)) nada_get_api_url(endpoint) else paste0(api_base_url, "/", endpoint)

  if (!is.null(file)) {
    if (!file.exists(file)) stop(paste0("File not found: ", file))
    body_raw <- readChar(file, file.info(file)$size, useBytes = TRUE)
    body <- jsonlite::fromJSON(body_raw, simplifyVector = FALSE)
    if (!is.list(body)) stop("JSON root must be an object (list).")
  } else {
    body <- payload
  }
  if (!is.null(dry_run)) body$dry_run <- isTRUE(dry_run)
  if (!is.null(overwrite)) body$overwrite <- isTRUE(overwrite)

  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body = body,
                       content_type_json(),
                       encode = "json",
                       accept_json(),
                       verbose(nada_get_verbose()))

  list(status_code = httpResponse$status_code, response = nada_http_response_json(httpResponse))
}
