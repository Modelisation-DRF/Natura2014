#' Vérifier le contenu des variables des fichier d'entrée
#'
#' @description Vérifier le contenu des variables des fichiers d'entrée
#
#' @param type_fic Type du fichier à vérifier: arbres, etudes
#' @param fichier Nom de la table à vérifier
#'
#' @return Table ou message d'erreur
#'
#' @export
valid_arbre <- function(type_fic, fichier) {
  # type_fic='arbres'; fichier=Arbres

  # essence = c('SAB','EPX','EPB','BOP')
  # dhpcm = c(8,10, 400, 50)
  # etat = c('10','10','10','11')
  # longitude = c(-70, -70, -70, -82)
  # fichier <- data.frame(essence, dhpcm, etat, longitude)
  # ht=T; vol=T; iqs=T; climat=T; sol=T;

  # test: test <- fichier_arbres_aveccov %>% mutate(type_eco = ifelse(type_eco=='RE20','FE32',type_eco)); names(test) <- tolower(names(test))
  # type_fic="arbres"; fichier=fic; ht=T; vol=T; iqs=F; climat=F; sol=F;

  names(fichier) <- tolower(names(fichier))

  if (type_fic == "arbres") {
    valid <- fic_validation %>% filter(fichier %in% c("arbres", "arbres, etudes"))
  }

  if (type_fic == "etudes") {
    valid <- fic_validation %>% filter(fichier %in% c("etudes", "arbres, etudes", "etudes, compil"))
  }

  # on accumule tous les messages
  erreur <- NULL
  for (i in 1:nrow(valid)) {
    # i=4

    # les valeurs possibles
    val <- as.character(valid[i, 2])

    # le message d'erreur si mauvaises valeurs
    message <- as.character(valid[i, 3])

    # on garde les lignes qui ne sont pas dans les valeurs possibles d'une variable
    fichier_val <- fichier %>% filter(!eval(parse(text = val)))

    # s'il y a des lignes avec des erreurs, on ajoute le message d'erreur
    if (nrow(fichier_val) > 0) {
      erreur <- c(erreur, message)
    }
  }

  # si erreur n'est pas vide on retourne l'erreur, sinon on retourne le fichier
  # if (!is.null(erreur)) {result <- erreur} else result <- fichier
  if (!is.null(erreur)) {
    # result <- erreur
    result <- paste(erreur, collapse = ", ")
  } else {
    result <- fichier
  }

  return(result)
}
