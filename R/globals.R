#global variables for the package
pkg.globals <- new.env()

#API key
pkg.globals$api_key <- ""

#API base url
pkg.globals$api_base_url <- ""

#Enable/disable Verbose mode
pkg.globals$verbose <- FALSE


get_disposition_filename <- function(httpResponse) {
  filename=sub(".*filename=", "", headers(httpResponse)$`content-disposition`)
  filename=gsub('"','',filename)
  return (noquote(filename))
}

dctypes <- function() {
  list(
    'Document, Administrative'= '[doc/adm]',
    "Document, Analytical" ="[doc/anl]",
    "Document, Other" = "[doc/oth]",
    "Document, Questionnaire"= "[doc/qst]",
    "Document, Reference"="[doc/ref]",
    "Document, Report"= "[doc/rep]",
    "Document, Technical"= "[doc/tec]",
    "Database" ="[dat]",
    "Microdata File"= "[dat/micro]",
    "Table" ="[tbl]"
  )
}


dcformats <-function() {
  list(
  "ZIP"= "application/zip",
  "Text" = "text",
  "HTML document" ="text/html",
  "PDF document" = "application/pdf",
  "GIF" = "image/gif",
  "JPEG" = "image/jpeg",
  "PNG" = "image/png"
  )
}
