# Resumable file upload via POST /api/uploads/*

.nada_api_base <- function(api_base_url = NULL) {
  if (is.null(api_base_url) || !nzchar(api_base_url)) {
    return(sub("/+$", "", nada_get_api_url()))
  }
  sub("/+$", "", api_base_url)
}

.nada_api_url <- function(endpoint, api_base_url = NULL) {
  base <- .nada_api_base(api_base_url)
  endpoint <- sub("^/+", "", endpoint)
  paste0(base, "/", endpoint)
}

.nada_table_data_file_ext_ok <- function(file) {
  tolower(tools::file_ext(file)) %in% c("csv", "zip", "txt")
}

#' Get resumable uploads API URL
#'
#' Builds URLs under \code{/api/uploads/} from the configured API base
#' (e.g. \code{http://host/index.php/api/uploads/init}).
#'
#' @param endpoint Optional path segment after \code{uploads/} (e.g. \code{"limits"}).
#' @param api_base_url Optional API base override.
#' @return Character URL.
#' @export
nada_get_uploads_api_url <- function(endpoint = NULL, api_base_url = NULL) {
  base <- paste0(.nada_api_base(api_base_url), "/uploads")
  if (!is.null(endpoint) && nzchar(endpoint)) {
    endpoint <- sub("^/+", "", endpoint)
    base <- paste0(base, "/", endpoint)
  }
  base
}

#' Fetch server resumable upload limits
#'
#' Wraps \code{GET /api/uploads/limits}.
#'
#' @param api_key Optional API key.
#' @param api_base_url Optional API base URL.
#' @return List with \code{status_code} and \code{response}.
#' @export
nada_resumable_upload_limits <- function(api_key = NULL, api_base_url = NULL) {
  if (is.null(api_key)) {
    api_key <- nada_get_api_key()
  }

  url <- nada_get_uploads_api_url("limits", api_base_url)
  httpResponse <- GET(
    url,
    add_headers("X-API-KEY" = api_key),
    accept_json(),
    verbose(nada_get_verbose())
  )

  list(
    status_code = httpResponse$status_code,
    response = nada_http_response_json(httpResponse)
  )
}

#' Upload a large file with resume support
#'
#' Uses \code{POST /api/uploads/init}, chunked \code{POST /api/uploads/chunk/{id}},
#' and \code{GET /api/uploads/status/{id}} to resume interrupted uploads.
#'
#' @param file Path to the file to upload.
#' @param metadata Optional list stored in upload session metadata.
#' @param chunk_size Chunk size in bytes; fetched from \code{/uploads/limits} when NULL.
#' @param api_key Optional API key.
#' @param api_base_url Optional API base URL.
#' @param progress_callback Optional \code{function(chunk_index, total_chunks, bytes_loaded, total_bytes)}.
#' @return List with \code{upload_id}, \code{filename}, \code{file_size}, \code{status_code}, \code{response}.
#' @export
nada_resumable_upload <- function(
  file,
  metadata = list(),
  chunk_size = NULL,
  api_key = NULL,
  api_base_url = NULL,
  progress_callback = NULL) {

  if (is.null(api_key)) {
    api_key <- nada_get_api_key()
  }

  if (!file.exists(file)) {
    stop(paste("File does not exist:", file))
  }

  file_size <- file.info(file)$size
  if (file_size == 0) {
    stop(paste("File is empty:", file))
  }

  if (is.null(chunk_size)) {
    limits <- nada_resumable_upload_limits(api_key = api_key, api_base_url = api_base_url)
    max_chunk <- 5 * 1024 * 1024
    if (limits$status_code == 200 &&
        is.list(limits$response) &&
        !is.null(limits$response$limits$max_chunk_size)) {
      max_chunk <- as.numeric(limits$response$limits$max_chunk_size)
    }
    chunk_size <- max(256 * 1024, min(max_chunk, 8 * 1024 * 1024))
  }

  chunk_size <- as.integer(chunk_size)
  total_chunks <- as.integer(ceiling(file_size / chunk_size))
  filename <- basename(file)

  init_body <- list(
    filename = filename,
    total_size = file_size,
    total_chunks = total_chunks,
    chunk_size = chunk_size,
    metadata = metadata
  )

  init_url <- nada_get_uploads_api_url("init", api_base_url)
  init_resp <- POST(
    init_url,
    add_headers("X-API-KEY" = api_key),
    body = init_body,
    encode = "json",
    content_type_json(),
    accept_json(),
    verbose(nada_get_verbose())
  )

  if (init_resp$status_code != 200) {
    stop(paste0(
      "Resumable upload init failed (HTTP ", init_resp$status_code, "): ",
      content(init_resp, "text", encoding = "UTF-8")
    ))
  }

  init_json <- nada_http_response_json(init_resp)
  if (!is.list(init_json) || init_json$status != "success" || is.null(init_json$upload_id)) {
    stop(paste(
      "Resumable upload init failed:",
      if (is.list(init_json) && !is.null(init_json$message)) init_json$message else content(init_resp, "text")
    ))
  }

  upload_id <- init_json$upload_id
  uploaded <- integer(0)

  status_url <- nada_get_uploads_api_url(paste0("status/", upload_id), api_base_url)
  status_resp <- tryCatch(
    GET(status_url, add_headers("X-API-KEY" = api_key), accept_json(), verbose(nada_get_verbose())),
    error = function(e) NULL
  )
  if (!is.null(status_resp) && status_resp$status_code == 200) {
    status_json <- nada_http_response_json(status_resp)
    if (is.list(status_json) && !is.null(status_json$uploaded_chunks)) {
      uploaded <- as.integer(status_json$uploaded_chunks)
    }
  }

  con <- file(file, "rb")
  on.exit(close(con), add = TRUE)

  bytes_loaded <- 0L
  for (chunk_num in 0:(total_chunks - 1L)) {
    if (chunk_num %in% uploaded) {
      bytes_loaded <- bytes_loaded + if (chunk_num == total_chunks - 1L) {
        file_size - chunk_num * chunk_size
      } else {
        chunk_size
      }
      if (!is.null(progress_callback)) {
        progress_callback(chunk_num + 1L, total_chunks, min(bytes_loaded, file_size), file_size)
      }
      next
    }

    if (chunk_num > 0L) {
      seek(con, chunk_num * chunk_size, origin = "start")
    }

    read_size <- if (chunk_num == total_chunks - 1L) {
      file_size - chunk_num * chunk_size
    } else {
      chunk_size
    }

    chunk_data <- readBin(con, "raw", read_size)
    if (length(chunk_data) == 0L) {
      stop(paste("Unexpected end of file at chunk", chunk_num))
    }

    chunk_url <- nada_get_uploads_api_url(paste0("chunk/", upload_id), api_base_url)
    chunk_resp <- POST(
      chunk_url,
      body = chunk_data,
      add_headers(
        "X-API-KEY" = api_key,
        "Content-Type" = "application/octet-stream",
        "X-Upload-Chunk-Number" = as.character(chunk_num),
        "X-Upload-Chunk-Size" = as.character(length(chunk_data))
      ),
      verbose(nada_get_verbose())
    )

    if (chunk_resp$status_code != 200) {
      stop(paste0(
        "Resumable upload chunk ", chunk_num, " failed (HTTP ", chunk_resp$status_code, "): ",
        content(chunk_resp, "text", encoding = "UTF-8")
      ))
    }

    chunk_json <- nada_http_response_json(chunk_resp)
    if (!is.list(chunk_json) || chunk_json$status != "success") {
      stop(paste(
        "Resumable upload chunk", chunk_num, "failed:",
        if (is.list(chunk_json) && !is.null(chunk_json$message)) chunk_json$message else content(chunk_resp, "text")
      ))
    }

    bytes_loaded <- bytes_loaded + length(chunk_data)
    if (!is.null(progress_callback)) {
      progress_callback(chunk_num + 1L, total_chunks, min(bytes_loaded, file_size), file_size)
    }
  }

  list(
    upload_id = upload_id,
    filename = filename,
    file_size = file_size,
    total_chunks = total_chunks,
    chunk_size = chunk_size,
    status_code = 200L,
    response = list(status = "success", upload_id = upload_id)
  )
}
