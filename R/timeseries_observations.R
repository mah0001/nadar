#' Import observations from a CSV file into MongoDB
#'
#' Runs the server's chunked import, so files of any size finish without hitting
#' web-server request timeouts:
#'   1. POST /api/admin/timeseries/data/upload (multipart: idno, file or upload_id)
#'      validates the CSV header against the DSD and stages the file. Existing
#'      observations are not changed by this step.
#'   2. POST /api/admin/timeseries/data/{idno}/import-process is called repeatedly.
#'      Each call works for about 30 seconds (replace existing rows,
#'      insert CSV rows from the saved position, finalize summaries) until the
#'      server reports `has_more = FALSE`.
#'
#' Send the CSV either as `file` or as the `upload_id` of a completed resumable
#' upload (see `nada_resumable_upload`). Files larger than the web server's
#' request body limit must use `upload_id`; `nada_admin_timeseries_import_csv_resumable`
#' does both steps.
#'
#' The study must be linked to a global DSD, either beforehand (see
#' `nada_admin_timeseries_attach_dsd`) or in the same call by passing `dsd_idno`.
#' All existing observations for the study are replaced by the CSV rows.
#'
#' Process calls answered with 409 (another call is still working on the same
#' study), 502/503/504 or a network error are retried after a pause; progress is
#' saved on the server after every batch, so retrying is safe. When verbose
#' mode is on, progress is printed after each call.
#'
#' @param idno Study idno (`surveys.idno`).
#' @param file Path to the CSV file to upload. Provide exactly one of `file` or `upload_id`.
#' @param upload_id ID of a completed resumable upload of the CSV (from `nada_resumable_upload`).
#' @param delimiter CSV field delimiter. Defaults to ",". Use "\\t" for tab.
#' @param mapping Named list mapping CSV header -> DSD component name, e.g.
#'   `list(country = "REF_AREA", year = "TIME_PERIOD", value = "OBS_VALUE")`.
#'   Headers absent from the mapping are used as-is.
#' @param ensure_unique_index When TRUE (default), the unique `key_hash` index
#'   is created before insert, so duplicate rows fail the import. Set FALSE to
#'   allow duplicates through.
#' @param dsd_idno Optional catalogue DSD idno; when set, the API links this DSD
#'   to the study (same effect as `nada_admin_timeseries_attach_dsd`) before import.
#' @param api_key Optional API key; defaults to `nada_get_api_key()`.
#' @param api_base_url Optional API base URL; defaults to `nada_get_api_url()`.
#'
#' @return list with `status_code` (HTTP status of the last call: 200 when the
#'   import completed) and `response` (`{ status, result }`, where `result` has
#'   `status` = "completed", `inserted`, `lines_read`, `ignored_columns`,
#'   `unparsed_time_periods`, `resource_saved`). On failure `response$message`
#'   holds the server's error and a warning is raised.
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

  # Step 1: stage the CSV (header is validated; data unchanged)
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

  upload_response <- POST(.nada_timeseries_url("admin/timeseries/data/upload", api_base_url),
                          add_headers("X-API-KEY" = api_key),
                          body = body,
                          encode = "multipart",
                          accept_json(),
                          nada_http_verbose())
  if (!(upload_response$status_code %in% c(200, 201))) {
    warning(content(upload_response, "text"))
    return(list(
      status_code = upload_response$status_code,
      response    = nada_http_response_json(upload_response)
    ))
  }

  # Step 2: advance the staged import in batches until the server reports no more work
  .nada_timeseries_run_import(idno,
                              api_key = api_key,
                              api_base_url = api_base_url)
}


#' Resume or finish a staged timeseries import
#'
#' Wraps repeated POST /api/admin/timeseries/data/{idno}/import-process calls.
#' Use it to continue an import that `nada_admin_timeseries_import_csv` started
#' but did not finish (e.g. the R session was interrupted). Returns immediately
#' with the final state when the import has already completed or failed.
#'
#' @param idno Study idno (`surveys.idno`).
#' @param max_time Seconds the server works per call (10-120, default 30).
#' @param max_retries Retries per call on 409/502/503/504 or network errors.
#' @param progress_callback Optional function called with the server's progress
#'   list (`status`, `progress_percent`, `inserted`, `lines_read`, ...) after each call.
#' @param api_key Optional API key; defaults to `nada_get_api_key()`.
#' @param api_base_url Optional API base URL; defaults to `nada_get_api_url()`.
#'
#' @return list with `status_code` and `response`; see `nada_admin_timeseries_import_csv`.
#'
#' @examples
#' \dontrun{
#'   nada_admin_timeseries_import_resume(idno = "prices-study")
#' }
#'
#' @export
nada_admin_timeseries_import_resume <- function(idno,
                                                max_time = 30,
                                                max_retries = 30,
                                                progress_callback = NULL,
                                                api_key = NULL,
                                                api_base_url = NULL) {
  if (is.null(idno) || !nzchar(as.character(idno))) {
    stop("`idno` (study idno) is required.")
  }
  if (is.null(api_key)) api_key <- nada_get_api_key()
  .nada_timeseries_run_import(idno,
                              max_time = max_time,
                              max_retries = max_retries,
                              progress_callback = progress_callback,
                              api_key = api_key,
                              api_base_url = api_base_url)
}


#' Get progress of a staged timeseries import
#'
#' Wraps GET /api/admin/timeseries/data/{idno}/import-status.
#'
#' @param idno Study idno (`surveys.idno`).
#' @param api_key Optional API key; defaults to `nada_get_api_key()`.
#' @param api_base_url Optional API base URL; defaults to `nada_get_api_url()`.
#'
#' @return list with `status_code` and `response`. `response$result` is NULL when
#'   the study has no staged import, otherwise a list with `status` (ready,
#'   deleting, in_progress, finalizing, completed, failed), `has_more`,
#'   `progress_percent`, `inserted`, `lines_read`, `error`, ...
#'
#' @export
nada_admin_timeseries_import_status <- function(idno,
                                                api_key = NULL,
                                                api_base_url = NULL) {
  if (is.null(idno) || !nzchar(as.character(idno))) {
    stop("`idno` (study idno) is required.")
  }
  if (is.null(api_key)) api_key <- nada_get_api_key()
  endpoint <- paste0("admin/timeseries/data/",
                     utils::URLencode(as.character(idno), reserved = TRUE),
                     "/import-status")
  httpResponse <- GET(.nada_timeseries_url(endpoint, api_base_url),
                      add_headers("X-API-KEY" = api_key),
                      accept_json(),
                      nada_http_verbose())
  if (httpResponse$status_code != 200) {
    warning(content(httpResponse, "text"))
  }
  list(
    status_code = httpResponse$status_code,
    response    = nada_http_response_json(httpResponse)
  )
}


.nada_timeseries_url <- function(endpoint, api_base_url = NULL) {
  if (is.null(api_base_url)) {
    nada_get_api_url(endpoint)
  } else {
    paste0(api_base_url, "/", endpoint)
  }
}


.nada_timeseries_run_import <- function(idno,
                                        max_time = 30,
                                        max_retries = 30,
                                        progress_callback = NULL,
                                        api_key,
                                        api_base_url = NULL) {
  endpoint <- paste0("admin/timeseries/data/",
                     utils::URLencode(as.character(idno), reserved = TRUE),
                     "/import-process")
  url <- .nada_timeseries_url(endpoint, api_base_url)
  max_time <- max(10, min(120, as.integer(max_time)))
  failures <- 0

  repeat {
    httpResponse <- tryCatch(
      POST(url,
           add_headers("X-API-KEY" = api_key),
           body = list(max_time = max_time),
           encode = "json",
           accept_json(),
           httr::timeout(max_time + 600),
           nada_http_verbose()),
      error = function(e) e
    )

    network_error <- inherits(httpResponse, "error")
    status_code <- if (network_error) NA_integer_ else httpResponse$status_code
    retryable <- network_error || status_code %in% c(409, 502, 503, 504)

    if (retryable && failures < max_retries) {
      # 409: a previous call is still working on this study; gateway/network
      # errors: the call may still be running. Progress is saved per batch.
      failures <- failures + 1
      Sys.sleep(min(2 * failures, 15))
      next
    }
    if (network_error) {
      stop(paste0("Timeseries import request failed: ", conditionMessage(httpResponse)))
    }

    response <- nada_http_response_json(httpResponse)
    if (status_code != 200) {
      warning(content(httpResponse, "text"))
      return(list(status_code = status_code, response = response))
    }
    failures <- 0

    progress <- response$result
    if (is.function(progress_callback)) {
      progress_callback(progress)
    } else if (isTRUE(nada_get_verbose())) {
      message(sprintf("Timeseries import %s: %s %s%% (%s rows)",
                      idno, progress$status, progress$progress_percent, progress$inserted))
    }

    if (identical(progress$status, "failed")) {
      msg <- if (!is.null(progress$error)) progress$error else "Import failed"
      warning(msg)
      response$status <- "error"
      response$message <- msg
      return(list(status_code = status_code, response = response))
    }
    if (!isTRUE(progress$has_more)) {
      return(list(status_code = status_code, response = response))
    }
  }
}


#' Import observations from a large CSV file via resumable upload
#'
#' Uploads the CSV in chunks with `nada_resumable_upload`, then runs the chunked
#' import with `nada_admin_timeseries_import_csv` using the returned `upload_id`.
#' Use this for files larger than the web server's request body limit.
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
#' its canonical import CSV. The link to the DSD is kept.
#' `nada_admin_timeseries_import_csv` already replaces all rows, so this is not
#' needed before a re-import. Refused while an import is running for the study.
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
                       nada_http_verbose())

  if (httpResponse$status_code != 200) {
    warning(content(httpResponse, "text"))
  }

  list(
    status_code = httpResponse$status_code,
    response    = nada_http_response_json(httpResponse)
  )
}
