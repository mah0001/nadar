library(testthat)

# Records the last httr call made by the index wrappers
capture_http <- function() {
  calls <- new.env()
  fake <- function(method) {
    force(method)
    function(url, ..., body = NULL, encode = NULL) {
      calls$method <- method
      calls$url <- url
      calls$body <- body
      calls$encode <- encode
      structure(list(status_code = 200), class = "response")
    }
  }
  list(calls = calls, fake = fake)
}

with_index_mocks <- function(cap, code) {
  local_mocked_bindings(
    GET = cap$fake("GET"),
    POST = cap$fake("POST"),
    DELETE = cap$fake("DELETE"),
    content = function(...) '{"status":"success"}',
    .package = "httr"
  )
  local_mocked_bindings(
    nada_get_api_url = function(endpoint = NULL) paste0("http://nada/api/", endpoint),
    nada_get_api_key = function() "key",
    nada_get_verbose = function() FALSE,
    .package = "nadar"
  )
  code
}

test_that("index list calls GET tables/indexes/{db}/{table}", {
  cap <- capture_http()
  with_index_mocks(cap, {
    res <- nada_admin_data_table_index_list("wdi", "pop")
    expect_equal(res$status_code, 200)
    expect_equal(res$response$status, "success")
  })
  expect_equal(cap$calls$method, "GET")
  expect_equal(cap$calls$url, "http://nada/api/tables/indexes/wdi/pop")
})

test_that("index create sends comma-joined fields as JSON", {
  cap <- capture_http()
  with_index_mocks(cap, {
    nada_admin_data_table_index_create("wdi", "pop", c("REF_AREA", " TIME_PERIOD "))
  })
  expect_equal(cap$calls$method, "POST")
  expect_equal(cap$calls$url, "http://nada/api/tables/indexes/wdi/pop")
  expect_equal(cap$calls$encode, "json")
  expect_equal(cap$calls$body$index_fields, "REF_AREA,TIME_PERIOD")
})

test_that("index create accepts a comma-separated string", {
  cap <- capture_http()
  with_index_mocks(cap, {
    nada_admin_data_table_index_create("wdi", "pop", "a, b,,c")
  })
  expect_equal(cap$calls$body$index_fields, "a,b,c")
})

test_that("index create rejects empty fields", {
  expect_error(nada_admin_data_table_index_create("wdi", "pop", character(0)), "index_fields")
  expect_error(nada_admin_data_table_index_create("wdi", "pop", " , "), "at least one")
})

test_that("text index create posts to tables/text_index", {
  cap <- capture_http()
  with_index_mocks(cap, {
    nada_admin_data_table_text_index_create("wdi", "ind", c("name", "description"))
  })
  expect_equal(cap$calls$url, "http://nada/api/tables/text_index/wdi/ind")
  expect_equal(cap$calls$body$index_fields, "name,description")
})

test_that("index delete calls DELETE with encoded index name", {
  cap <- capture_http()
  with_index_mocks(cap, {
    nada_admin_data_table_index_delete("wdi", "pop", "REF_AREA_1")
  })
  expect_equal(cap$calls$method, "DELETE")
  expect_equal(cap$calls$url, "http://nada/api/tables/indexes/wdi/pop/REF_AREA_1")
})

test_that("index delete refuses _id_ and all", {
  expect_error(nada_admin_data_table_index_delete("wdi", "pop", "_id_"), "_id_")
  expect_error(nada_admin_data_table_index_delete("wdi", "pop", "all"), "delete_all")
})

test_that("index delete all posts to tables/indexes/{db}/{table}/all", {
  cap <- capture_http()
  with_index_mocks(cap, {
    nada_admin_data_table_index_delete_all("wdi", "pop")
  })
  expect_equal(cap$calls$method, "POST")
  expect_equal(cap$calls$url, "http://nada/api/tables/indexes/wdi/pop/all")
})

test_that("dictionary indexes posts to tables/create_dictionary_indexes", {
  cap <- capture_http()
  with_index_mocks(cap, {
    nada_admin_data_table_dictionary_indexes_create()
  })
  expect_equal(cap$calls$url, "http://nada/api/tables/create_dictionary_indexes")
})

test_that("explicit api_base_url overrides the configured one", {
  cap <- capture_http()
  with_index_mocks(cap, {
    nada_admin_data_table_index_list("wdi", "pop", api_base_url = "http://other/api/")
  })
  expect_equal(cap$calls$url, "http://other/api/tables/indexes/wdi/pop")
})

test_that("missing ids are rejected", {
  expect_error(nada_admin_data_table_index_list("", "pop"), "db_id")
  expect_error(nada_admin_data_table_index_list("wdi", NULL), "table_id")
})
