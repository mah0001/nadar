#' Data File Metadata
#'
#' Functions to interact with data files and variables
#'
#' @param idno Character. Study unique ID number.
#' @param api_key API key (optional if API key is set using nada_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using nada_set_api_url)
#' @param as_data_table Logical. If `TRUE`, converts the result to `data.table`. Defaults to `TRUE`.
#'
#' @return A `data.table`, a list (parsed JSON), or raw text, depending on `format` and `as_data_table`.
#' @export
#'
#' @describeIn nada_list_datasets List all data files for a given study (`idno`).
#'
#' @examples
#' \dontrun{
#'   dt <- nada_list_datasets(idno = "AFG_2015_DHS_v01_M")
#' }
nada_list_datasets <- function(
    idno = NULL,
    api_key = NULL,
    api_base_url = NULL,
    as_data_table = TRUE
) {

  # survey id
  if (is.null(idno)) {
    cli::cli_abort("Study ID is not provided")
  }

  # Construct API endpoint
  endpoint<- paste0("catalog/", idno, "/data_files")

  if (is.null(api_base_url)) {
    url <- nada_get_api_url(endpoint = endpoint)
  } else {
    url <- paste0(api_base_url, endpoint)
  }

  # Send GET request
  httpResponse <- GET(url,
                      add_headers("X-API-KEY" = api_key),
                      accept_json(),
                      verbose(nada_get_verbose())
  )

  # Check for HTTP errors
  if (httr::http_error(httpResponse)) {

    cli::cli_abort(c(
      "x" = "HTTP error {.code {httr::status_code(httpResponse)}}",
      "!" = httr::content(httpResponse, "text")
    ))

  }

  # Parse content
  parsed <- httr::content(httpResponse, "parsed", type = "application/json")

  if (as_data_table) {

    dt <- data.table::rbindlist(
      lapply(parsed$datafiles, function(x) {
        data.table::data.table(
          id = x$id,
          sid = x$sid,
          file_id = x$file_id,
          file_name = x$file_name,
          description = x$description,
          case_count = x$case_count,
          var_count = x$var_count,
          producer = x$producer,
          data_checks = x$data_checks,
          missing_data = x$missing_data,
          version = x$version,
          notes = x$notes,
          metadata = x$metadata
        )
      }),
      fill = TRUE
    )

    return(dt)

  } else {
    return(parsed)
  }

}


#' @describeIn nada_list_datasets Get information about a specific data file (`fileid`) in a study.
#' @param fileid Character. Data file unique ID number.
#' @export
#'
#' @examples
#' \dontrun{
#'   dt <- nada_data_file_info(idno = "AFG_2015_DHS_v01_M",
#'   fileid = "F1")
#'   print(dt)
#' }
nada_data_file_info <- function(
    idno = NULL,
    fileid = NULL,
    api_key = NULL,
    api_base_url = NULL,
    as_data_table = TRUE
) {

  # survey id
  if (is.null(idno)) {
    cli::cli_abort("Study ID is not provided")
  }

  # file id provided as F1, F2, ....
  if (is.null(fileid)) {
    cli::cli_abort("File ID is not provided")
  }

  # Construct API endpoint
  endpoint <- paste0("catalog/", idno, "/data_files/", fileid)

  if (is.null(api_base_url)) {
    url <- nada_get_api_url(endpoint = endpoint)
  } else {
    url <- paste0(api_base_url, endpoint)
  }

  # Send GET request
  httpResponse <- GET(url,
                      add_headers("X-API-KEY" = api_key),
                      accept_json(),
                      verbose(nada_get_verbose())
  )

  # Check for HTTP errors
  if (httr::http_error(httpResponse)) {

    cli::cli_abort(c(
      "x" = "HTTP error {.code {httr::status_code(response)}}",
      "!" = httr::content(httpResponse, "text")
    ))

  }

  # Parse content
  parsed <- httr::content(httpResponse, "parsed", type = "application/json")

  if (as_data_table) {

    # Convert list to named vector and replace NULLs with NA
    fields <- lapply(parsed$datafile, function(x) if (is.null(x)) NA else x)

    # Convert to one-row data frame
    dt <- as.data.frame(fields, stringsAsFactors = FALSE) |>
      data.table::as.data.table()

    return(dt)

  } else {

    return(parsed)
  }
}

#' @describeIn nada_list_datasets Get variables in a specific data file (`fileid`) of a study.
#' @param fileid Character. Data file unique ID number.
#' @export
#'
#' @examples
#' \dontrun{
#'   dt <- nada_variables_info(idno = "AFG_2015_DHS_v01_M",
#'   fileid = "F1")
#'   print(dt)
#' }
nada_variables_info <- function(
    idno = NULL,
    fileid = NULL,
    api_key = NULL,
    api_base_url = NULL,
    as_data_table = TRUE
) {

  # survey id
  if (is.null(idno)) {
    cli::cli_abort("Study ID is not provided")
  }

  # file id provided as F1, F2, ....
  if (is.null(fileid)) {
    cli::cli_abort("File ID is not provided")
  }

  # Construct API endpoint
  endpoint <- paste0("catalog/", idno, "/data_files/", fileid, "/variables")

  # Send GET request
  httpResponse <- GET(url,
                      add_headers("X-API-KEY" = api_key),
                      accept_json(),
                      verbose(nada_get_verbose())
  )

  # Check for HTTP errors
  if (httr::http_error(httpResponse)) {

    cli::cli_abort(c(
      "x" = "HTTP error {.code {httr::status_code(response)}}",
      "!" = httr::content(httpResponse, "text")
    ))

  }

  # Parse content
  parsed <- httr::content(httpResponse, "parsed", type = "application/json")
  if (as_data_table) {

    dt <- data.table::rbindlist(
      lapply(parsed$variables, function(x) {
        data.table::data.table(
          uid = x$uid,
          sid = x$sid,
          fid = x$fid,
          vid = x$vid,
          name = x$name,
          labl = x$labl
        )
      }),
      fill = TRUE
    )

    return(dt)

  } else {
    return(parsed)
  }
}

#' @describeIn nada_list_datasets List all variables in a study (`idno`), regardless of data file.
#' @export
#'
#' @examples
#' \dontrun{
#'   dt <- nada_list_variables(idno = "AFG_2015_DHS_v01_M")
#'   print(dt)
#' }
nada_list_variables <- function(
    idno = NULL,
    api_key = NULL,
    api_base_url = NULL,
    as_data_table = TRUE
) {

  # survey id
  if (is.null(idno)) {
    cli::cli_abort("Study ID is not provided")
  }

  # Construct API endpoint
  endpoint<- paste0("catalog/", idno, "/variables")

  if (is.null(api_base_url)) {
    url <- nada_get_api_url(endpoint = endpoint)
  } else {
    url <- paste0(api_base_url, endpoint)
  }

  # Send GET request
  httpResponse <- GET(url,
                      add_headers("X-API-KEY" = api_key),
                      accept_json(),
                      verbose(nada_get_verbose())
  )

  # Check for HTTP errors
  if (httr::http_error(httpResponse)) {

    cli::cli_abort(c(
      "x" = "HTTP error {.code {httr::status_code(response)}}",
      "!" = httr::content(httpResponse, "text")
    ))

  }

  # Parse content
  parsed <- httr::content(httpResponse, "parsed", type = "application/json")
  if (as_data_table) {

    dt <- data.table::rbindlist(
      lapply(parsed$variables, function(x) {
        data.table::data.table(
          uid = x$uid,
          sid = x$sid,
          fid = x$fid,
          vid = x$vid,
          name = x$name,
          labl = x$labl
        )
      }),
      fill = TRUE
    )

    return(dt)

  } else {
    return(parsed)
  }
}

#' @describeIn nada_list_datasets Get metadata for a single variable (`varId`) in a study.
#' @param varId Character. Variable ID number.
#' @export
#'
#' @examples
#' \dontrun{
#'   nada_find_variable(idno = "AFG_2015_DHS_v01_M", varId = "V2233")
#' }
nada_find_variable <- function(
    idno = NULL,
    varId = NULL,
    api_key = NULL,
    api_base_url = NULL
) {

  # survey id
  if (is.null(idno)) {
    cli::cli_abort("Study ID is not provided")
  }

  # variable id
  if (is.null(varId)) {
    cli::cli_abort("Variable ID is not provided")
  }

  # Construct API endpoint
  endpoint <- paste0("catalog/", idno, "/variables/", varId)

  if (is.null(api_base_url)) {
    url <- nada_get_api_url(endpoint = endpoint)
  } else {
    url <- paste0(api_base_url, endpoint)
  }

  # Send GET request
  httpResponse <- GET(url,
                      add_headers("X-API-KEY" = api_key),
                      accept_json(),
                      verbose(nada_get_verbose())
  )

  # Check for HTTP errors
  if (httr::http_error(httpResponse)) {

    cli::cli_abort(c(
      "x" = "HTTP error {.code {httr::status_code(response)}}",
      "!" = httr::content(httpResponse, "text")
    ))

  }

  # Parse content
  parsed <- httr::content(httpResponse, "parsed", type = "application/json")

  return(parsed$variable)

}
