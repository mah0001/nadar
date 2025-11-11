#' Get Country Codes from World Bank Microdata API
#'
#' Fetches country codes and related metadata from the World Bank Microdata Library API.
#'
#' @param api_key API key (optional if API key is set using nada_set_api_key)
#' @param api_base_url API base endpoint (optional if API base endpoint is set using nada_set_api_url)
#' @param name Optional. Filter by country name.
#' @param iso Optional. Filter by ISO country code.
#' @param as_data_table Logical. If `TRUE` and format is JSON, converts the result to a `data.table`. Defaults to `TRUE`.
#'
#' @return A `data.table` if `as_data_table = TRUE`, otherwise a list (parsed JSON) or raw text for other formats.
#' @export
#'
#' @examples
#' \dontrun{
#'   dt <- nada_country_codes()
#'   print(dt)
#' }
nada_country_codes <- function(
    api_key = NULL,
    api_base_url = NULL,
    name = NULL,
    iso = NULL,
    as_data_table = TRUE
) {

  # Construct API endpoint
  endpoint <- paste0("catalog/country_codes")

  if (is.null(api_base_url)) {
    url <- nada_get_api_url(endpoint = endpoint)
  } else {
    url <- paste0(api_base_url, endpoint)
  }

  # Send GET request
  httpResponse <- GET(url,
                      accept_json(),
                      add_headers("X-API-KEY" = api_key),
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
      lapply(parsed$country_codes, function(x) {
        data.table::data.table(
          name = x$name,
          iso = x$iso
        )
      }),
      fill = TRUE
    )

    # return(data.table::as.data.table(parsed$country_codes))
    return(dt)
  } else {
    return(parsed)
  }

}
