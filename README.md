# NADAR
An R client for NADA API

## Function Reference

### Public Functions

These functions are accessible to non-admin users and typically provide read-only access to the catalog.

#### Configuration & HTTP Utilities

| Function | Description |
|----------|-------------|
| `nada_set_api()` | Set all API settings (wrapper for URL, key, and verbose) |
| `nada_set_api_key()` | Set API key |
| `nada_get_api_key()` | Get API key |
| `nada_set_api_url()` | Set API URL |
| `nada_get_api_url()` | Get API URL |
| `nada_set_api_verbose()` | Set verbose mode |
| `nada_get_api_verbose()` | Get verbose mode |
| `nada_get_verbose()` | Get verbose mode (alias) |
| `nada_http_get()` | HTTP GET request |
| `nada_http_post()` | HTTP POST request |
| `nada_http_put()` | HTTP PUT request |
| `nada_http_delete()` | HTTP DELETE request |

#### Study Functions (Catalog/Datasets)

| Function | Description |
|----------|-------------|
| `nada_study_search()` | Search studies in catalog |
| `nada_study_get_by_id()` | Get study by ID |
| `nada_study_get_by_idno()` | Get study by IDNO |
| `nada_study_download_ddi()` | Download DDI metadata file |
| `nada_study_download_rdf()` | Download RDF metadata file |

#### Collection Functions

| Function | Description |
|----------|-------------|
| `nada_collection_list()` | List all collections |

#### Table Functions (Data API)

| Function | Description |
|----------|-------------|
| `nada_data_table_list()` | List all data API tables |

#### Resource Functions (External Resources)

| Function | Description |
|----------|-------------|
| `nada_resource_list()` | List external resources for a study |
| `nada_resource_download()` | Download external resource |

#### Widget Functions

| Function | Description |
|----------|-------------|
| `nada_widget_list()` | List all widgets |

#### Datafile Functions

| Function | Description |
|----------|-------------|
| `nada_datafile_get_dictionary()` | Get data dictionary |
| `nada_datafile_get_dictionary_no_stats()` | Get data dictionary without stats |
| `nada_datafile_write_csv()` | Write CSV utility (local file) |

#### Utility Functions

| Function | Description |
|----------|-------------|
| `nada_is_valid_url()` | URL validation utility |
| `nada_capture_pdf_cover()` | PDF utility |
| `nada_rdf_to_list()` | RDF parsing utility |

### Admin Functions

These functions require admin privileges and allow write/delete operations.

#### Study Operations

| Function | Description |
|----------|-------------|
| `nada_admin_study_list()` | List all studies/datasets |
| `nada_admin_study_create()` | Create study/dataset |
| `nada_admin_study_create_entry()` | Create catalog entry |
| `nada_admin_study_delete()` | Delete study entry |
| `nada_admin_study_import_ddi()` | Import DDI file |
| `nada_admin_study_replace_idno()` | Replace study IDNO |
| `nada_admin_study_get_json()` | Get study metadata as JSON |
| `nada_admin_study_write_json()` | Write study metadata as JSON |
| `nada_admin_study_get_collections()` | Get collections for study |
| `nada_admin_study_get_options()` | Get/Set study options |
| `nada_admin_study_attach_collections()` | Attach collections to study |
| `nada_admin_study_attach_related()` | Attach related studies |
| `nada_admin_study_attach()` | Attach data API to study |

#### Collection Operations

| Function | Description |
|----------|-------------|
| `nada_admin_collection_create()` | Create collection |
| `nada_admin_collection_delete()` | Delete collection |
| `nada_admin_collection_rename()` | Rename collection |
| `nada_admin_collection_update()` | Update collection |

#### Table Operations (Data API)

| Function | Description |
|----------|-------------|
| `nada_admin_data_table_create()` | Create data table |
| `nada_admin_data_table_delete()` | Delete data table |
| `nada_admin_data_table_upload_csv()` | Upload CSV to table |
| `nada_admin_data_table_import_csv()` | Import CSV data |
| `nada_admin_data_table_batch_import_csv()` | Batch import CSV data |
| `nada_admin_data_table_publish()` | Publish table (create + upload + import) |

#### Resource Operations (External Resources)

| Function | Description |
|----------|-------------|
| `nada_admin_resource_add()` | Add external resource |
| `nada_admin_resource_delete()` | Delete external resource |
| `nada_admin_resource_delete_all()` | Delete all external resources |
| `nada_admin_resource_import()` | Import external resources |
| `nada_admin_resource_upload()` | Upload external resource |

#### Content Operations

| Function | Description |
|----------|-------------|
| `nada_admin_document_add()` | Add document to study |
| `nada_admin_image_add()` | Add image to study |
| `nada_admin_microdata_add()` | Add microdata to study |
| `nada_admin_script_add()` | Add script to study |
| `nada_admin_table_add()` | Add table entry to catalog |
| `nada_admin_timeseries_add()` | Add timeseries to study |
| `nada_admin_timeseries_database_create()` | Create timeseries database |
| `nada_admin_video_add()` | Add video to study |
| `nada_admin_geospatial_add()` | Add geospatial data |
| `nada_admin_geospatial_import()` | Import geospatial data |

#### Thumbnail Operations

| Function | Description |
|----------|-------------|
| `nada_admin_thumbnail_upload()` | Upload thumbnail |
| `nada_admin_thumbnail_delete()` | Delete thumbnail |

#### Widget Operations

| Function | Description |
|----------|-------------|
| `nada_admin_widget_create()` | Create widget |
| `nada_admin_widget_delete()` | Delete widget |
| `nada_admin_widget_attach()` | Attach widget to study |
| `nada_admin_widget_detach()` | Detach widget from study |

#### System Operations (Solr)

| Function | Description |
|----------|-------------|
| `nada_admin_solr_index_entries()` | Index catalog entries in Solr |
| `nada_admin_solr_index_variables()` | Index variables in Solr |
