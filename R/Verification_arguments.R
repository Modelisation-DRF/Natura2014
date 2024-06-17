#' Vérification des arguments de la fonction principale du simulateur Natura
#'
#' @description Vérification des arguments de la fonction principale \code{SimulNatura()} du simulateur Natura-2014
#'
#' @inheritParams SimulNatura
#'
#' @return Une chaine de caractères contant "ok" s'il n'y a pas d'erreur, sinon, contient un message d'erreur.
#' @export
#'
CheckArguments <- function(file_arbre, file_etude, file_compile, horizon, dec_perturb, dec_tbe1, tbe1, dec_tbe2, tbe2, ht, vol, climat) {

  # on ne doit pas spécifier les 3 fichiers en même temps
  if (missing(file_arbre) & missing(file_etude) & missing(file_compile)) {
    erreur <- "Au moins un des deux file_arbre et file_etude OU file_compile doit etre specifie"
  }
  # Si on spécifie le fichier compilé, les 2 autres fichiers doivent être vides
  else if ((!missing(file_arbre)  | !missing(file_etude)) & !missing(file_compile)){
    erreur <- "Seulement un des deux file_arbre et file_etude OU file_compile doit etre specifie"
  }
  # Si on ne spécifie pas de fichier compilé, il faut spécifier les 2 autres
  else if ((missing(file_arbre)  | missing(file_etude)) & missing(file_compile)){
    erreur <- "Si file_compile n'est pas specifie, file_arbre ET file_etude doivent etre specifies"
  }
  # l'horizon doit être entre 1 et 15
  else if (horizon>15 | horizon<1) {
    erreur <- c("horizon doit etre de 1 a 15")
  }
  # la décennie de la perturbation ne doit pas dépasser l'horizon
  else if (dec_perturb > horizon) {
    erreur <- c("dec_perturb doit etre <= horizon")
  }
  # la décennie de la 1e tbe ne doit pas dépasser l'horizon
  else if (dec_tbe1 > horizon) {
    erreur <- c("dec_tbe1 doit etre <= horizon")
  }
  # si on spécifie une décennie de tbe, on doit aussi spécifier l'indice de tbe
  else if (dec_tbe1>0 & tbe1==0) {
    erreur <- c("Si dec_tbe1 est specifie, tbe1 doit etre specifie aussi")
  }
  # si on spécifie un indice de tbe, on doit aussi spécifier la décennie de tbe
  else if (dec_tbe1==0 & tbe1>0) {
    erreur <- c("Si tbe1 est specifie, dec_tbe1 doit etre specifie aussi")
  }
  # l'indice de tbe doit être un entier 1-2-3-4-5
  else if (dec_tbe1>0 & !tbe1 %in% c(1,2,3,4,5)) {
    erreur <- c("Si dec_tbe1 est specifie, tbe1 doit etre 1, 2, 3, 4 ou 5")
  }
  # pour spécifier une 2e tbe, il faut en avoir spécifié une premiere
  else if (dec_tbe2>0 & (dec_tbe1==0 | tbe1==0)) {
    erreur <- c("dec_tbe1 et tbe1 doivent etre specifies pour pouvoir utiliser dec_tbe2")
  }
  # la décennie de la 2e tbe ne doit pas dépasser l'horizon
  else if (dec_tbe2 > horizon) {
    erreur <- c("dec_tbe2 doit etre <= horizon")
  }
  # la 2e tbe doit être après la 1e tbe
  else if (dec_tbe2>0 & dec_tbe2 <= dec_tbe1) {
    erreur <- c("dec_tbe2 doit etre > dec_tbe1")
  }
  # si on spécifie une décennie de tbe, on doit aussi spécifier l'indice de tbe
  else if (dec_tbe2>0 & tbe2==0) {
    erreur <- c("Si dec_tbe2 est specifie, tbe2 doit etre specifie aussi")
  }
  # si on spécifie un indice de tbe, on doit aussi spécifier la décennie de tbe
  else if (dec_tbe2==0 & tbe2>0) {
    erreur <- c("Si tbe2 est specifie, dec_tbe2 doit etre specifie aussi")
  }
  # l'indice de tbe doit être un entier 1-2-3-4-5
  else if (dec_tbe2>0 & !tbe2 %in% c(1,2,3,4,5)) {
    erreur <- c("Si dec_tbe2 est specifie, tbe2 doit etre 1, 2, 3, 4 ou 5")
  }
  # l'argument du calcul de la hauteur doit être binaire
  else if (!ht %in% c(TRUE, FALSE)) {
    erreur <- c("ht doit etre TRUE ou FALSE")
  }
  # l'argument du calcul de volume doit être binaire
  else if (!vol %in% c(TRUE, FALSE)) {
    erreur <- c("vol doit etre TRUE ou FALSE")
  }
  # l'argument du calcul ddu climat doit être binaire
  else if (!climat %in% c(TRUE, FALSE)) {
    erreur <- c("climat doit etre TRUE ou FALSE")
  }
  else erreur <- c("ok")

  return(erreur)

}
