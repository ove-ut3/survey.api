#' listflow_import_csv
#'
#' @param file \dots
#'
#' @export
listflow_import_csv <- function(file) {

  listflow_import_csv <- read.csv2(file, fileEncoding = "UTF-8-BOM") %>%
    dplyr::as_tibble() %>%
    dplyr::rename_all(tolower) %>% 
    dplyr::rename_all(stringr::str_replace_all, "[[:punct:]\\s]+", "_") %>% 
    dplyr::rename_all(stringr::str_remove_all, "[^\\w]") %>% 
    dplyr::rename_all(stringi::stri_trans_general, "latin-ascii") %>% 
    dplyr::mutate(service = "listflow.io") %>%
    dplyr::mutate(
      email_validity = dplyr::case_when(
        email_validity %in% c("ok", "fail") ~ email_validity,
        email_validity_reason %in% c("email_exists", "ok_for_all") ~ "ok",
        email_validity_reason %in% c("email_disabled") ~ "fail",
        TRUE ~ email_validity
      )
    ) %>%
    dplyr::select(email, service, status = email_validity, status_date = last_update) %>%
    dplyr::mutate_at("status", dplyr::recode, "ok" = "valid", "fail" = "invalid") %>%
    dplyr::mutate_at("status_date", substr, 1, 10) %>%
    dplyr::mutate_at("status_date", as.numeric) %>%
    dplyr::mutate_at("status_date", lubridate::as_datetime) %>%
    dplyr::mutate_at("status_date", lubridate::as_date)

  return(listflow_import_csv)
}

#' lisflow_get_datasources
#'
#' @param limit \dots
#' @param skip \dots
#' @param sort \dots
#'
#' @export
lisflow_get_datasources <- function(limit = NULL, skip = NULL, sort = NULL) {

  httr::GET(
    "https://api.listflow.io/v2/datasources",
    httr::add_headers("Authorization" = api_key)
  ) %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

}

#' lisflow_get_datasource
#'
#' @param id \dots
#'
#' @export
lisflow_get_datasource <- function(id) {

  httr::GET(
    glue::glue("http://api.listflow.io/v2/datasources/{id}"),
    httr::add_headers("Authorization" = api_key)
  ) %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

}

#' lisflow_get_files
#'
#' @param limit \dots
#' @param skip \dots
#' @param sort \dots
#'
#' @export
lisflow_get_files <- function(limit = NULL, skip = NULL, sort = NULL) {

  httr::GET(
    "https://api.listflow.io/v2/files",
    httr::add_headers("Authorization" = api_key)
  ) %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

}

#' lisflow_get_file
#'
#' @param id \dots
#'
#' @export
lisflow_get_file <- function(id) {

  httr::GET(
    glue::glue("http://api.listflow.io/v2/files/{id}"),
    httr::add_headers("Authorization" = api_key)
  ) %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

}

#' lisflow_post_file
#'
#' @param name \dots
#'
#' @return the file ID
#'
#' @export
lisflow_post_file <- function(name) {

  name <- "test_api_listflow_httr"

  body <- glue::glue(.open = "{{", .close = "}}", '{
    "name": "{{name}}",
    "mapping": [
        {
            "name": "email",
            "index": 703,
            "property": "5cb71a9311f4fd001c71f705"
        }
    ],
    "importType": "person",
    "sample": [
        {
            "email": "testlistflowapi2@test.com"
        }
    ],
    "preparation_type": "advanced",
  	"preparation_settings": {
  		"customize_microservices": {
  			 "company-enrichment": true,
  			 "contacts-decompositor": true,
  			 "dedupe": true,
  			 "domain-details": true,
  			 "domain-technologies": true,
  		     "email-cleaning": true,
  		     "email-parsing": true,
  		     "email-to-linkedin": true,
  		     "firstname-analysis": true,
  		     "mx-check": true,
  		     "phone-cleaning": true
  		}
  	}
}') %>%
    jsonlite::fromJSON()

  body <- '{
	"name": "test_api_listflow_httr",
	"mapping": [
		{
			"name": "email",
			"index": 703,
			"property": "5cb71a9311f4fd001c71f705"
		}
	],
	"importType": "person",
	"sample": [
		{
			"email": "testlistflowapi@test.com"
		}
	],
	"preparation_type": "advanced",
	"preparation_settings": {
		"customize_microservices": {
			 "company-enrichment": true,
			 "contacts-decompositor": true,
			 "dedupe": true,
			 "domain-details": true,
			 "domain-technologies": true,
		     "email-cleaning": true,
		     "email-parsing": true,
		     "email-to-linkedin": true,
		     "firstname-analysis": true,
		     "mx-check": true,
		     "phone-cleaning": true
		}
	}
}' %>%
  jsonlite::fromJSON()

  req <- httr::POST(
    url = "http://api.listflow.io/v2/files",
    httr::add_headers(
      c(
        'User-Agent'= "PostmanRuntime/7.18.0",
        'Accept'= "*/*",
        'Cache-Control'= "no-cache",
        'Postman-Token'= "99472d68-a28d-4680-bce8-1aa3684b3a9d,44f7694d-1ca1-4e13-aee3-ed86f005bb80",
        'Host'= "api.listflow.io",
        'Accept-Encoding'= "gzip, deflate",
        # 'Content-Length'= "660",
        'Cookie'= "__cfduid=d8d99b6fb7aa4695347c15527b17d71a21571237777",
        'Connection'= "keep-alive",
        'cache-control'= "no-cache",
        'Authorization' = "EN155PJ-RXJMC7Z-H75K75S-THTZSV3",
        'Content-Type'= "application/json"
      )
    ),
    body = body,
    encode = "json"
  )

  # req <- httr::POST(
  #   url = "http://api.listflow.io/v2/files",
  #   httr::add_headers("Authorization" = api_key, "Content-Type" = "application/json"),
  #   body = body,
  #   encode = "json"
  # )

  content <- httr::content(req, as = "text") %>%
    jsonlite::fromJSON()

  content$data %>%
    nrow()

  return()

}

#' lisflow_delete_file
#'
#' @param id \dots
#'
#' @export
lisflow_delete_file <- function(id) {

  #id <- '5db044ae0997dd001481d14b'

  delete <- httr::DELETE(
    glue::glue("http://api.listflow.io/v2/files/{id}"),
    httr::add_headers(c("Authorization" = api_key))
  )

  # delete %>%
  #   httr::content(as = "text") %>%
  #   jsonlite::fromJSON()

}

#' lisflow_get_contacts
#'
#' @param limit \dots
#' @param skip \dots
#' @param sort \dots
#'
#' @export
lisflow_get_contacts <- function(limit = NULL, skip = NULL, sort = NULL) {

  httr::GET(
    "https://api.listflow.io/v2/contacts?$limit=1&$skip=3",
    httr::add_headers("Authorization" = api_key)
  ) %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

}

#' lisflow_get_contact
#'
#' @param id \dots
#'
#' @export
lisflow_get_contact <- function(id) {

  # id <- "8xadn902k0ojw7imbp3cetqc3ghwon"
  # id <- "5ceceb8cc9a569001cc7c406"

  httr::GET(
    glue::glue("http://api.listflow.io/v2/contacts/{id}"),
    httr::add_headers("Authorization" = api_key)
  ) %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

}

#' listflow_post_contacts_file
#'
#' @param file \dots
#' @param csv_file \dots
#'
#' @export
listflow_post_contacts_file <- function(file, csv_file) {

  httr::POST(
    url = glue::glue("http://api.listflow.io/v2/files/:{file}/contacts"),
    httr::add_headers(
      c("Authorization" = api_key,
        "Content-Type" = "multipart/form-data",
        )
    )
  )

  'curl -X POST "http://api.listflow.io/v2/files/:file_id/contacts" -H  "accept: application/json" -H  "Authorization: EN155PJ-RXJMC7Z-H75K75S-THTZSV3" -H  "Content-Type: multipart/form-data" -F "file=@listflow.csv;type=application/vnd.ms-excel"'

}

#' lisflow_delete_contact
#'
#' @param id \dots
#'
#' @export
lisflow_delete_contact <- function(id) {

  delete <- httr::DELETE(
    glue::glue("http://api.listflow.io/v2/contact/{id}"),
    httr::add_headers("Authorization" = api_key)
  )

}

