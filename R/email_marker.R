#' email_marker
#'
#' @param email \dots
#' @param key \dots
#' @param sleep \dots
#'
#' @export
email_marker <- function(email, key, sleep = 0) {

  Sys.sleep(sleep)

  json <- glue::glue("https://app.emailmarker.com/api/verify?apiKey={key}&email={email}") %>%
    jsonlite::fromJSON()

  time <- lubridate::today() %>%
    as.character()

  emailmarker = c("result" = json$result, "time" = "time")

}
