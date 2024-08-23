#' Vérifier le contenu des variables des fichier d'entrée
#'
#' @description Vérifier le contenu des variables des fichiers d'entrée
#
#' @param type_fic Type du fichier à vérifier: arbres ou compil ou valid
#' @param fichier Nom de la table à vérifier
#' @inheritParams SimulNatura
#'
#' @return Table ou message d'erreur
# #' @export
#'
# @examples
valid_placette <- function(type_fic, fichier, ht=NULL, climat=NULL){

  # type_fic = 'arbres'

  # essence = c('SAB','EPX','EPB','BOP')
  # dhpcm = c(8,10, 400, 50)
  # etat = c('10','10','10','11')
  # longitude = c(-70, -70, -70, -82)
  # fichier <- data.frame(essence, dhpcm, etat, longitude)
  # ht=T; vol=T; iqs=T; climat=T; sol=T;

  # test: test <- fichier_arbres_aveccov %>% mutate(type_eco = ifelse(type_eco=='RE20','FE32',type_eco)); names(test) <- tolower(names(test))
  # type_fic="arbres"; fichier=test; ht=T; vol=T; iqs=F; climat=F; sol=F;
  # type_fic="compile"; iqs=F; climat=F; sol=F;

  # type_fic='arbres'; fichier=Arbres; ht=ht; climat=climat;

  names(fichier) <- tolower(names(fichier))


  fichier <- fichier %>%
    group_by(placette) %>%
    mutate(
      sdom_bio = case_when(
        n_distinct(sdom_bio) == 1 & sdom_bio == 2 ~ "2E",
        n_distinct(sdom_bio) == 1 & sdom_bio == 3 ~ "3E",
        n_distinct(sdom_bio) == 1 & sdom_bio == 4 ~ "4E",
        n_distinct(sdom_bio) == 1 & sdom_bio == 5 ~ "5E",
        n_distinct(sdom_bio) == 1 & sdom_bio == 6 ~ "6E",
        n_distinct(sdom_bio) == 1 & sdom_bio == 7 ~ "7E",
        TRUE ~ as.character(sdom_bio)  # default case to handle other conditions
      )
    )



  if (type_fic=='arbres') {

    valid1 <- NULL; valid2 <- NULL; valid3 <- NULL; valid4 <- NULL; valid5 <- NULL;

    valid1 <- fic_validation %>% filter(fichier %in% c("peup"))

    # si on fournit le climat et ht, il faut juste verifier t_mat et ptot
    if (isFALSE(climat) & isFALSE(ht)) valid2 <- fic_validation %>% filter(fichier == 'climat_ht')

    # si on fournit le climat et pas ht, il faut vérifier t_mat et ptot et altitude
    if (isFALSE(climat) & isTRUE(ht)) valid3 <- fic_validation %>% filter(fichier %in% c('climat_ht', 'ht'))

    # si on ne fournit pas le climat et qu'il faut calculer ht, il vérifier altitude
    if (isTRUE(climat) & isTRUE(ht)) valid4 <- fic_validation %>% filter(fichier == 'ht')

    # s'il faut aller dans les cartes, il faut les coord
    if (isTRUE(climat)) valid5 <- fic_validation %>% filter(fichier == 'coord')


    valid <- bind_rows(valid1, valid2, valid3, valid4, valid5)

  }

  if (type_fic=='compile') {

    valid1 <- NULL; valid2 <- NULL; valid3 <- NULL;

    valid1 <- fic_validation %>% filter(fichier %in% c("compil", "peup", "etudes, compil"))

    # si on fournit climat
    if (isFALSE(climat)) valid2 <- fic_validation %>% filter(fichier %in% c('climat_ht'))

    # s'il faut aller dans les cartes pour au moins une variables, il faut les coord
    if (isTRUE(climat)) valid3 <- fic_validation %>% filter(fichier == 'coord')


    valid <- bind_rows(valid1, valid2, valid3)
  }

  if (type_fic=='valid') {
    valid <- fic_validation %>% filter(fichier=='valid')
  }

  # fichier=test # 29 obs
  # fichier = fichier_arbres_aveccov; names(fichier_arbres_aveccov) <- tolower(names(fichier_arbres_aveccov))
  # fichier <- fichier_compile_aveccov; names(fichier)  <- tolower(names(fichier))
  fichier_complet <- fichier
  erreur <- NULL  # on accumule tous les messages
  for (i in 1:nrow(valid)) {
    #i=1
    val <-   as.character(valid[i,2])  # les valeurs possibles
    message <- as.character(valid[i,3]) # le message d'erreur si mauvaises valeurs
    fichier_val <- fichier_complet %>% filter(!eval(parse(text = val)))  # on garde les lignes qui ne sont pas dans les valeurs possibles d'une variable
    fichier <- fichier %>% filter(eval(parse(text = val)))  # on filtre le fichier
    if (nrow(fichier_val)>0) {# s'il y a des lignes en dehors des plages
      fichier_val$message <- message # on ajoute le message au fichier
      erreur <- bind_rows(erreur, fichier_val) # on accumule les lignes avec erreur
    }
  }

  # si erreur n'est pas vide on garde une ligne par placette/message
  if (!is.null(erreur)) {
    #erreur <- erreur %>% group_by(id_pe) %>% slice(1)
    erreur <- erreur %>% dplyr::select(placette,message) %>% unique() %>% arrange(placette)
    }


 return(list(fichier, erreur))

}


