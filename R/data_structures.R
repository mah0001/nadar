#' List data structures (DSDs)
#'
#' Returns all global DSDs in the catalogue.
#' Endpoint: GET /api/admin/data_structures
#'
#' @param api_key Optional API key; defaults to the value set via `nada_set_api_key()`.
#' @param api_base_url Optional API base URL; defaults to the value set via `nada_set_api_url()`.
#'
#' @return list with status_code and response
#' @export
nada_admin_dsd_list <- function(api_key = NULL, api_base_url = NULL) {
  if (is.null(api_key)) api_key <- nada_get_api_key()
  url <- if (is.null(api_base_url)) nada_get_api_url("admin/data_structures") else paste0(api_base_url, "/admin/data_structures")

  httpResponse <- GET(url,
                      add_headers("X-API-KEY" = api_key),
                      accept_json(),
                      verbose(nada_get_verbose()))

  list(status_code = httpResponse$status_code, response = nada_http_response_json(httpResponse))
}

#' Create a data structure (row only)
#'
#' Creates only the data_structures row (no components, no codelists).
#' Use `nada_admin_dsd_import_json()` to create a DSD with components and codelists in one call.
#' Endpoint: POST /api/admin/data_structures/create
#'
#' @param idno Catalogue idno (required). Used by studies as data_structure_reference.
#' @param name Maintainable name (required). Unique with agency + version.
#' @param agency Agency (default "NADA").
#' @param version Version (default "1.0").
#' @param title Title.
#' @param description Description.
#' @param status Status.
#' @param notes Notes.
#' @param metadata Named list stored as JSON metadata on the structure.
#' @param overwrite When TRUE, overwrite an existing non-locked DSD matched by
#'   idno or identity (agency+name+version).
#' @param api_key,api_base_url See `nada_admin_dsd_list`.
#'
#' @return list with status_code and response
#' @export
nada_admin_dsd_create <- function(idno, name, agency = "NADA", version = "1.0",
                                  title = NULL, description = NULL, status = NULL,
                                  notes = NULL, metadata = NULL, overwrite = FALSE,
                                  api_key = NULL, api_base_url = NULL) {
  if (is.null(api_key)) api_key <- nada_get_api_key()
  url <- if (is.null(api_base_url)) nada_get_api_url("admin/data_structures/create") else paste0(api_base_url, "/admin/data_structures/create")

  body <- list(idno = idno, name = name, agency = agency, version = version,
               title = title, description = description, status = status,
               notes = notes, metadata = metadata)
  body <- body[!vapply(body, is.null, logical(1))]

  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body = body,
                       content_type_json(),
                       encode = "json",
                       query = list(overwrite = ifelse(isTRUE(overwrite), "1", "0")),
                       accept_json(),
                       verbose(nada_get_verbose()))

  list(status_code = httpResponse$status_code, response = nada_http_response_json(httpResponse))
}

#' Delete a data structure
#'
#' Endpoint: POST /api/admin/data_structures/delete/{id_or_idno}
#'
#' @param id_or_idno Numeric id or catalogue idno of the data structure.
#' @param api_key,api_base_url See `nada_admin_dsd_list`.
#'
#' @return list with status_code and response
#' @export
nada_admin_dsd_delete <- function(id_or_idno, api_key = NULL, api_base_url = NULL) {
  if (is.null(api_key)) api_key <- nada_get_api_key()
  endpoint <- paste0("admin/data_structures/delete/", utils::URLencode(as.character(id_or_idno), reserved = TRUE))
  url <- if (is.null(api_base_url)) nada_get_api_url(endpoint) else paste0(api_base_url, "/", endpoint)

  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       accept_json(),
                       verbose(nada_get_verbose()))

  list(status_code = httpResponse$status_code, response = nada_http_response_json(httpResponse))
}

#' Import a full DSD (structure + components + codelists) from JSON
#'
#' Wraps POST /api/admin/data_structures/import_json.
#' Body shape is documented in application/schemas/data-structure-schema.json.
#'
#' Each coded component binds codelists via a single nested object, e.g.:
#' \preformatted{
#' list(
#'   structure = list(idno = "NADA_DSD_PRICES_1.0", name = "DSD_PRICES",
#'                    agency = "NADA", version = "1.0"),
#'   components = list(
#'     list(name = "REF_AREA", column_type = "geography", data_type = "string",
#'          codelist = list(idno = "NADA_CL_AREA_1.0", name = "CL_AREA",
#'                          items = list(list(code = "AA", label = "Area A")))),
#'     list(name = "TIME_PERIOD", column_type = "time_period",
#'          time_period_format = "YYYY-MM"),
#'     list(name = "OBS_VALUE", column_type = "observation_value", data_type = "double")
#'   )
#' )
#' }
#' Reuse an existing codelist with \code{codelist = list(idno = "...")} (no items).
#'
#' @param payload Named list matching data-structure-schema.json, OR
#' @param file Path to a JSON file with the payload (mutually exclusive with `payload`).
#' @param dry_run When TRUE, the API validates only and does not persist (HTTP 200).
#' @param overwrite_codelists When TRUE, replace items on any matched existing codelist.
#' @param api_key,api_base_url See `nada_admin_dsd_list`.
#'
#' @return list with status_code and response (parsed JSON summary from the importer)
#' @export
nada_admin_dsd_import_json <- function(payload = NULL, file = NULL,
                                       dry_run = FALSE, overwrite_codelists = FALSE,
                                       api_key = NULL, api_base_url = NULL) {
  if (is.null(payload) && is.null(file)) {
    stop("Provide either `payload` (list) or `file` (path to a JSON file).")
  }
  if (!is.null(payload) && !is.null(file)) {
    stop("Provide only one of `payload` or `file`, not both.")
  }
  if (is.null(api_key)) api_key <- nada_get_api_key()

  endpoint <- "admin/data_structures/import_json"
  url <- if (is.null(api_base_url)) nada_get_api_url(endpoint) else paste0(api_base_url, "/", endpoint)

  qp <- list()
  if (isTRUE(dry_run))             qp$dry_run             <- "1"
  if (isTRUE(overwrite_codelists)) qp$overwrite_codelists <- "1"

  if (!is.null(file)) {
    if (!file.exists(file)) stop(paste0("File not found: ", file))
    body_raw <- readChar(file, file.info(file)$size, useBytes = TRUE)
    httpResponse <- POST(url,
                         add_headers("X-API-KEY" = api_key),
                         body = body_raw,
                         content_type_json(),
                         accept_json(),
                         query = qp,
                         verbose(nada_get_verbose()))
  } else {
    httpResponse <- POST(url,
                         add_headers("X-API-KEY" = api_key),
                         body = payload,
                         content_type_json(),
                         encode = "json",
                         accept_json(),
                         query = qp,
                         verbose(nada_get_verbose()))
  }

  list(status_code = httpResponse$status_code, response = nada_http_response_json(httpResponse))
}

#' Import a DSD from an SDMX-ML XML file
#'
#' Wraps POST /api/admin/data_structures/import.
#'
#' @param file Path to the SDMX structure XML file.
#' @param overwrite_codelists When TRUE, replace items on matched existing codelists.
#' @param dsd_id When multiple DSDs are present in the XML, the SDMX id to import.
#' @param api_key,api_base_url See `nada_admin_dsd_list`.
#'
#' @return list with status_code and response
#' @export
nada_admin_dsd_import_xml <- function(file, overwrite_codelists = FALSE, dsd_id = NULL,
                                      api_key = NULL, api_base_url = NULL) {
  if (!file.exists(file)) stop(paste0("File not found: ", file))
  if (is.null(api_key)) api_key <- nada_get_api_key()

  endpoint <- "admin/data_structures/import"
  url <- if (is.null(api_base_url)) nada_get_api_url(endpoint) else paste0(api_base_url, "/", endpoint)

  body <- list(file = httr::upload_file(file))
  if (isTRUE(overwrite_codelists)) body$overwrite_codelists <- "1"
  if (!is.null(dsd_id) && nzchar(as.character(dsd_id))) body$dsd_id <- as.character(dsd_id)

  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body = body,
                       encode = "multipart",
                       accept_json(),
                       verbose(nada_get_verbose()))

  list(status_code = httpResponse$status_code, response = nada_http_response_json(httpResponse))
}
