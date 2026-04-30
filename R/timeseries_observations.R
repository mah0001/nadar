#' Import observations from a CSV file into MongoDB
#'
#' Wraps POST /api/admin/timeseries/data/import (multipart body includes `idno` and `file`).
#'
#' The study must be linked to a global DSD, either beforehand (see
#' `nada_admin_timeseries_attach_dsd`) or in the same call by passing `dsd_idno`.
#' The server:
#'   - Resolves the DSD from `surveys.data_structure_id`.
#'   - Ensures Mongo indexes (including unique `key_hash` by default).
#'   - Reads the CSV, applies the optional header `mapping`, and rejects any
#'     post-mapping header that is not a DSD component.
#'   - Builds observation documents and bulk-inserts; empty cells become NULL.
#'
#' HTTP responses:
#'   - 201 `{ status, result: { inserted, lines_read } }`.
#'   - 409 on duplicate `key_hash` for any row.
#'   - 400 on validation / unknown mapped field / missing DSD link.
#'
#' @param idno Study idno (`surveys.idno`).
#' @param file Path to the CSV file to upload.
#' @param delimiter CSV field delimiter. Defaults to ",". Use "\\t" for tab.
#' @param mapping Named list mapping CSV header -> DSD component name, e.g.
#'   `list(country = "REF_AREA", year = "TIME_PERIOD", value = "OBS_VALUE")`.
#'   Headers absent from the mapping are used as-is.
#' @param ensure_unique_index When TRUE (default), the unique `key_hash` index
#'   is created before insert. Set FALSE to allow duplicates through.
#' @param dsd_idno Optional catalogue DSD idno; when set, the API links this DSD
#'   to the study (same effect as `nada_admin_timeseries_attach_dsd`) before import.
#' @param api_key Optional API key; defaults to `nada_get_api_key()`.
#' @param api_base_url Optional API base URL; defaults to `nada_get_api_url()`.
#'
#' @return list with `status_code` and `response`.
#'
#' @examples
#' \dontrun{
#'   nada_admin_timeseries_import_csv(
#'     idno    = "prices-study",
#'     file    = "observations.csv",
#'     mapping = list(country = "REF_AREA",
#'                    year    = "TIME_PERIOD",
#'                    value   = "OBS_VALUE")
#'   )
#' }
#'
#' @export
nada_admin_timeseries_import_csv <- function(idno,
                                             file,
                                             delimiter = ",",
                                             mapping = NULL,
                                             ensure_unique_index = TRUE,
                                             dsd_idno = NULL,
                                             api_key = NULL,
                                             api_base_url = NULL) {
  if (is.null(idno) || !nzchar(as.character(idno))) {
    stop("`idno` (study idno) is required.")
  }
  if (!file.exists(file)) {
    stop(paste0("File not found: ", file))
  }
  if (is.null(api_key)) api_key <- nada_get_api_key()

  endpoint <- "admin/timeseries/data/import"
  url <- if (is.null(api_base_url)) {
    nada_get_api_url(endpoint)
  } else {
    paste0(api_base_url, "/", endpoint)
  }

  body <- list(
    idno      = as.character(idno),
    file      = httr::upload_file(file),
    delimiter = delimiter
  )
  if (!is.null(mapping)) {
    if (!is.list(mapping)) stop("`mapping` must be a named list.")
    body$mapping <- jsonlite::toJSON(mapping, auto_unbox = TRUE)
  }
  body$ensure_unique_index <- if (isTRUE(ensure_unique_index)) "1" else "0"
  if (!is.null(dsd_idno) && nzchar(as.character(dsd_idno))) {
    body$dsd_idno <- as.character(dsd_idno)
  }

  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       body = body,
                       encode = "multipart",
                       accept_json(),
                       verbose(nada_get_verbose()))

  if (!(httpResponse$status_code %in% c(200, 201))) {
    warning(content(httpResponse, "text"))
  }

  list(
    status_code = httpResponse$status_code,
    response    = nada_http_response_json(httpResponse)
  )
}


