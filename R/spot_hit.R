#' spot_hit_send_sms
#'
#' @param message \dots
#' @param destinataires \dots
#' @param key API key
#' @param expediteur \dots
#' @param type premium or lowcost
#' @param smslong \dots
#' @param encodage \dots
#' @param nom \dots
#'
#' @export
spot_hit_send_sms <- function(message, destinataires, key, expediteur = NULL, type = "premium", smslong = 0, encodage = "auto", nom = NULL) {

  if (!type %in% c("premium", "lowcost")) {
    stop("\"type\" argument must be one of these values: ", paste(c("premium", "lowcost"), collapse = ", "), call. = FALSE)
  }

  if (!is.null(expediteur)) {
    if (nchar(expediteur) > 11) {
      warning("\"expediteur\" argument is truncated to 11 characters.")
      expediteur <- substr(expediteur, 1, 11)
    }
    expediteur <- glue::glue("&expediteur={expediteur}") %>%
      iconv(to = "UTF-8") %>%
      utils::URLencode()
  }
  if (!is.null(nom)) {
    nom <- glue::glue("&nom={nom}") %>%
      iconv(to = "UTF-8") %>%
      utils::URLencode()
  }

  message <- message %>%
    iconv(to = "UTF-8") %>%
    utils::URLencode()

  spot_hit_send_sms <- httr::GET(
    glue::glue("https://www.spot-hit.fr/api/envoyer/sms?key={key}&type={type}&message={message}&destinataires={destinataires}&encodage={encodage}&smslong={smslong}") %>%
      paste0(expediteur, nom)
  )

}

#' spot_hit_list_short_url
#'
#' @param key API key
#'
#' @export
spot_hit_list_short_url <- function(key) {

  spot_hit_remove_short_url <- httr::GET(
    glue::glue("https://www.spot-hit.fr/api/liens-courts/lister?key={key}")
  ) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON()

  return(spot_hit_remove_short_url)
}

#' spot_hit_short_url
#'
#' @param adresse_cible \dots
#' @param extension \dots
#' @param key API key
#' @param nom \dots
#' @param domaine_id \dots
#' @param sous_domaine \dots
#'
#' @export
spot_hit_add_short_url <- function(adresse_cible, extension, key, nom = NULL, domaine_id = NULL, sous_domaine = NULL) {

  if (!is.null(nom)) {
    nom <- glue::glue("&nom={nom}") %>%
      iconv(to = "UTF-8") %>%
      utils::URLencode()
  }
  if (!is.null(domaine_id)) {
    domaine_id <- glue::glue("&domaine_id={domaine_id}") %>%
      iconv(to = "UTF-8") %>%
      utils::URLencode()
  }
  if (!is.null(sous_domaine)) {
    sous_domaine <- glue::glue("&sous_domaine={sous_domaine}") %>%
      iconv(to = "UTF-8") %>%
      utils::URLencode()
  }

  spot_hit_add_short_url <- httr::GET(
    glue::glue("https://www.spot-hit.fr/api/liens-courts/creer?key={key}&extension={extension}&adresse_cible={adresse_cible}") %>%
      paste0(nom, domaine_id, sous_domaine)
  )

}

#' spot_hit_remove_short_url
#'
#' @param id \dots
#' @param key API key
#'
#' @export
spot_hit_remove_short_url <- function(id, key) {

  spot_hit_remove_short_url <- httr::GET(
    glue::glue("https://www.spot-hit.fr/api/liens-courts/supprimer?key={key}&id={id}")
  )

}
