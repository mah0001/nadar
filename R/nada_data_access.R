#' Get list of data access types for studies
#'
#' Fetches the list of data access types available
#'
#'
#' @param as_data_table Logical. If `TRUE`, converts the result to a `data.table`. Defaults to `TRUE`.
#'
#' @return A `data.table` if `as_data_table = TRUE`, otherwise a list (parsed JSON) or raw text for other formats.
#' @export
#'
#' @examples
#' \dontrun{
#'   dt <- nada_study_access_types()
#'   print(dt)
#' }
nada_study_access_types <- function(
    as_data_table = TRUE,
    api_key = NULL,
    api_base_url = NULL){


  if(is.null(api_key)){
    api_key=nada_get_api_key();
  }

  url=nada_get_api_url('catalog/data_access_codes')


  httpResponse <- GET(url, add_headers("X-API-KEY" = api_key),
                      accept_json(),
                      verbose(nada_get_verbose())
                      )

  output=NULL

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
  }

  output=list(
    "status_code"=httpResponse$status_code,
    "response"=nada_http_response_json(httpResponse)
  )

  parsed <- httr::content(httpResponse, "parsed", type = "application/json")


  if (as_data_table) {

    dt <- data.table::rbindlist(
      lapply(parsed$codes, function(x) {
        data.table::data.table(
          id = x$id,
          type = x$type,
          title = x$title
        )
      }),
      fill = TRUE
    )
    return(dt)

  } else {
    return(parsed)
  }

}
