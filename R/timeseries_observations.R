#' Import observations from a CSV file into MongoDB
#'
#' Wraps POST /api/admin/timeseries/data/{idno}/import (multipart: idno, file or upload_id; URL path uses the same idno).
#'
#' Send the CSV either as `file` or as the `upload_id` of a completed resumable
#' upload (see `nada_resumable_upload`). Files larger than the web server's
#' request body limit must use `upload_id`; `nada_admin_timeseries_import_csv_resumable`
#' does both steps.
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
#' @param file Path to the CSV file to upload. Provide exactly one of `file` or `upload_id`.
#' @param upload_id ID of a completed resumable upload of the CSV (from `nada_resumable_upload`).
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
                                             file = NULL,
                                             delimiter = ",",
                                             mapping = NULL,
                                             ensure_unique_index = TRUE,
                                             dsd_idno = NULL,
                                             upload_id = NULL,
                                             api_key = NULL,
                                             api_base_url = NULL) {
  if (is.null(idno) || !nzchar(as.character(idno))) {
    stop("`idno` (study idno) is required.")
  }
  has_file   <- !is.null(file) && nzchar(as.character(file))
  has_upload <- !is.null(upload_id) && nzchar(as.character(upload_id))
  if (has_file == has_upload) {
    stop("Provide exactly one of `file` or `upload_id`.")
  }
  if (has_file && !file.exists(file)) {
    stop(paste0("File not found: ", file))
  }
  if (is.null(api_key)) api_key <- nada_get_api_key()

  endpoint <- paste0(
    "admin/timeseries/data/",
    utils::URLencode(as.character(idno), reserved = TRUE),
    "/import"
  )
  url <- if (is.null(api_base_url)) {
    nada_get_api_url(endpoint)
  } else {
    paste0(api_base_url, "/", endpoint)
  }

  body <- list(
    idno      = as.character(idno),
    delimiter = delimiter
  )
  if (has_file) {
    body$file <- httr::upload_file(file)
  } else {
    body$upload_id <- as.character(upload_id)
  }
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


#' Import observations from a large CSV file via resumable upload
#'
#' Uploads the CSV in chunks with `nada_resumable_upload`, then imports it with
#' `nada_admin_timeseries_import_csv` using the returned `upload_id`. Use this for
#' files larger than the web server's request body limit.
#'
#' @inheritParams nada_admin_timeseries_import_csv
#' @param file Path to the CSV file to upload.
#' @param chunk_size Optional chunk size in bytes (default from server limits).
#' @param progress_callback Optional progress function; see `nada_resumable_upload`.
#'
#' @return list with `status_code`, `response`, and `upload_id`.
#'
#' @examples
#' \dontrun{
#'   nada_admin_timeseries_import_csv_resumable(
#'     idno     = "prices-study",
#'     file     = "observations.csv",
#'     dsd_idno = "DSD_PRICES"
#'   )
#' }
#'
#' @export
nada_admin_timeseries_import_csv_resumable <- function(idno,
                                                       file,
                                                       delimiter = ",",
                                                       mapping = NULL,
                                                       ensure_unique_index = TRUE,
                                                       dsd_idno = NULL,
                                                       chunk_size = NULL,
                                                       api_key = NULL,
                                                       api_base_url = NULL,
                                                       progress_callback = NULL) {
  if (is.null(idno) || !nzchar(as.character(idno))) {
    stop("`idno` (study idno) is required.")
  }
  if (!file.exists(file)) {
    stop(paste0("File not found: ", file))
  }
  if (tolower(tools::file_ext(file)) != "csv") {
    stop("File must be a CSV file.")
  }
  if (is.null(api_key)) api_key <- nada_get_api_key()

  upload_result <- nada_resumable_upload(
    file = file,
    metadata = list(source = "nadar", allowed_types = "csv", idno = as.character(idno)),
    chunk_size = chunk_size,
    api_key = api_key,
    api_base_url = api_base_url,
    progress_callback = progress_callback
  )

  result <- nada_admin_timeseries_import_csv(
    idno = idno,
    upload_id = upload_result$upload_id,
    delimiter = delimiter,
    mapping = mapping,
    ensure_unique_index = ensure_unique_index,
    dsd_idno = dsd_idno,
    api_key = api_key,
    api_base_url = api_base_url
  )
  result$upload_id <- upload_result$upload_id
  result
}


#' Delete all observations for a timeseries study
#'
#' Wraps POST /api/admin/timeseries/data/{idno}/clear-data.
#'
#' Removes the study's observations from MongoDB, its value-count summaries and
#' its canonical import CSV. The link to the DSD is kept. Use it before
#' re-running `nada_admin_timeseries_import_csv` for the same study, which
#' otherwise fails with 409 on duplicate rows.
#'
#' HTTP responses:
#'   - 200 `{ status, result: { cleared } }`.
#'   - 400 when the study does not exist or has no DSD link.
#'
#' @param idno Study idno (`surveys.idno`).
#' @param api_key Optional API key; defaults to `nada_get_api_key()`.
#' @param api_base_url Optional API base URL; defaults to `nada_get_api_url()`.
#'
#' @return list with `status_code` and `response`.
#'
#' @examples
#' \dontrun{
#'   nada_admin_timeseries_clear_data(idno = "prices-study")
#'   nada_admin_timeseries_import_csv(idno = "prices-study", file = "observations.csv")
#' }
#'
#' @export
nada_admin_timeseries_clear_data <- function(idno,
                                             api_key = NULL,
                                             api_base_url = NULL) {
  if (is.null(idno) || !nzchar(as.character(idno))) {
    stop("`idno` (study idno) is required.")
  }
  if (is.null(api_key)) api_key <- nada_get_api_key()

  endpoint <- paste0(
    "admin/timeseries/data/",
    utils::URLencode(as.character(idno), reserved = TRUE),
    "/clear-data"
  )
  url <- if (is.null(api_base_url)) {
    nada_get_api_url(endpoint)
  } else {
    paste0(api_base_url, "/", endpoint)
  }

  httpResponse <- POST(url,
                       add_headers("X-API-KEY" = api_key),
                       accept_json(),
                       verbose(nada_get_verbose()))

  if (httpResponse$status_code != 200) {
    warning(content(httpResponse, "text"))
  }

  list(
    status_code = httpResponse$status_code,
    response    = nada_http_response_json(httpResponse)
  )
}
