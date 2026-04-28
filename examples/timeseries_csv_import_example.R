# End-to-end example using current nadar admin functions:
#   1) create a timeseries project
#   2) create a DSD with codelists
#   3) attach DSD to the project
#   4) upload/import CSV observations
#
# Requires an admin API key.

library(nadar)

API_BASE <- "http://nada-catalog/index.php/api"
API_KEY  <- ""
nadar::nada_set_api(API_BASE, API_KEY, verbose = FALSE)

suffix       <- format(Sys.time(), "%Y%m%d%H%M%S")
study_idno   <- paste0("ts-example-", suffix)
dsd_idno     <- paste0("NADA_DSD_TS_", suffix)
cl_area_idno <- paste0("NADA_CL_AREA_", suffix)

# 1) Create timeseries project
cat("1) Creating timeseries project:", study_idno, "\n")
study_res <- nadar::nada_admin_timeseries_add(
  idno      = study_idno,
  published = 0,
  overwrite = "yes",
  metadata  = list(
    series_description = list(
      idno = study_idno,
      name = paste0("Timeseries Example ", suffix)
    )
  )
)
stopifnot(study_res$status_code == 200)

# 2) Create DSD with codelists (full import payload)
cat("2) Creating DSD + codelists:", dsd_idno, "\n")
dsd_res <- nadar::nada_admin_dsd_import_json(
  payload = list(
    structure = list(
      idno    = dsd_idno,
      name    = paste0("DSD_TS_", suffix),
      agency  = "NADA",
      version = "1.0.0",
      title   = "Timeseries DSD Example"
    ),
    components = list(
      list(
        name        = "REF_AREA",
        column_type = "geography",
        data_type   = "string",
        sort_order  = 0,
        codelist = list(
          idno    = cl_area_idno,
          name    = paste0("CL_AREA_", suffix),
          agency  = "NADA",
          version = "1.0.0",
          items = list(
            list(code = "FR", label = "France",  sort_order = 0),
            list(code = "DE", label = "Germany", sort_order = 1)
          )
        )
      ),
      list(
        name               = "TIME_PERIOD",
        column_type        = "time_period",
        data_type          = "string",
        time_period_format = "YYYY-MM",
        sort_order         = 1
      ),
      list(
        name        = "OBS_VALUE",
        column_type = "observation_value",
        data_type   = "double",
        sort_order  = 2
      )
    ),
    import_options = list(
      overwrite_codelists = FALSE,
      dry_run = FALSE
    )
  )
)
stopifnot(dsd_res$status_code %in% c(200, 201))

# 3) Attach DSD to project
cat("3) Attaching DSD to project\n")
attach_res <- nadar::nada_admin_timeseries_attach_dsd(
  idno = study_idno,
  dsd_idno = dsd_idno
)
stopifnot(attach_res$status_code == 200)

# 4) Upload/import CSV data
cat("4) Importing CSV data\n")
csv_path <- tempfile(pattern = "ts_import_", fileext = ".csv")
csv_rows <- data.frame(
  country = c("FR", "FR", "DE", "DE"),
  month   = c("2024-01", "2024-02", "2024-01", "2024-02"),
  value   = c(101.2, 101.8, 99.7, 100.4),
  stringsAsFactors = FALSE
)
utils::write.csv(csv_rows, csv_path, row.names = FALSE)

import_res <- nadar::nada_admin_timeseries_import_csv(
  idno = study_idno,
  file = csv_path,
  mapping = list(
    country = "REF_AREA",
    month   = "TIME_PERIOD",
    value   = "OBS_VALUE"
  )
)
stopifnot(import_res$status_code %in% c(200, 201))

cat("Done.\n")
cat("Study:", study_idno, "\n")
cat("DSD:  ", dsd_idno, "\n")
cat("Imported rows:", import_res$response$result$inserted, "\n")
