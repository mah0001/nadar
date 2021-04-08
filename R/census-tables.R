# Publish census tables
library(nadar)

nadar::set_api_key("your-api-key")
nadar::set_api_url("http://digital-library.census.ihsn.org/index.php/api/")


# delete census tables
census_tables_delete <- function(api_key=NULL, api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }


  httpResponse=nadar::nada_http_delete('census_tables/delete')

  if(httpResponse$status_code!=200){
    warning(content(httpResponse, "text"))
    stop(content(httpResponse, "text"), call. = FALSE)
  }

  return (httpResponse)
}


census_tables_import_csv <- function(census_year, csv_file, api_key=NULL, api_base_url=NULL){

  if(is.null(api_key)){
    api_key=get_api_key();
  }

  if(!is.numeric(census_year)){
    warning("Invalid value for 'census_year'. Enter a year for census e.g. 2011")
    stop("exiting", call. = FALSE)
  }

  if (!file.exists(csv_file)){
    stop(paste0("csv_file not found: ",csv_file), call. = FALSE)
  }

  body_options=list(
      "file"=upload_file(csv_file)
  )

  url<-paste0('census_tables/import_table/',census_year)
  print(url)

  httpResponse=nadar::nada_http_post(url, options=body_options)
  return (httpResponse)
}



# DELETE census tables
result<-census_tables_delete()

# import CSV by census year
result<-census_tables_import_csv(census_year=2011, csv_file='/Users/m2/Downloads/global-census-tables/census_tables-2011.csv')

result<-census_tables_import_csv(census_year=2001, csv_file='/Users/m2/Downloads/global-census-tables/census_tables-2001.csv')
