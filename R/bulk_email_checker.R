#' bulk_email_checker
#'
#' @param email \dots
#' @param key \dots
#' @param sleep \dots
#'
#' @export
bulk_email_checker <- function(email, key, sleep = 0) {

  Sys.sleep(sleep)

  json <- jsonlite::fromJSON(
      glue::glue("http://api-v4.bulkemailchecker.com/?key={key}&email={email}")
    )

  time <- as.character(
    lubridate::today()
  )
  
  json$status <- switch(
    json$status,
    passed = "valid",
    unknown = "unknown",
    failed = "invalid"
  )

  data.frame("status" = json$status, "event" = json$event, "details" = json$details, "time" = time, stringsAsFactors = FALSE)

}
