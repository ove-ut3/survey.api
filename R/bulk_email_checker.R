#' bulk_email_checker
#'
#' @param email \dots
#' @param key \dots
#' @param sleep \dots
#'
#' @export
bulk_email_checker <- function(email, key, sleep = 0) {

  Sys.sleep(sleep)

  json <- glue::glue("http://api-v4.bulkemailchecker.com/?key={key}&email={email}") %>%
    jsonlite::fromJSON()

  time <- lubridate::today() %>%
    as.character()

  data.frame("status" = json$status, "event" = json$event, "details" = json$details, "time" = time)

}
