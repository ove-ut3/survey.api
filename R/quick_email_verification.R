#' quick_email_verification
#'
#' @param email \dots
#' @param key \dots
#' @param sleep \dots
#'
#' @export
quick_email_verification <- function(email, key, sleep = 0) {

  Sys.sleep(sleep)

  json <- glue::glue("http://api.quickemailverification.com/v1/verify?apikey={key}&email={email}") %>%
    jsonlite::fromJSON()

  time <- lubridate::today() %>%
    as.character()

  json$result <- switch(
    json$result,
    passed = "valid",
    failed = "invalid",
    unknown = "unknown"
  )

  c("result" = json$result, "reason" = json$reason, "time" = time)

}
