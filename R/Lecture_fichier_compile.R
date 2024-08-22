#' Lire le fichier des placettes à simuler avec Natura et valider le nom des colonnes
#'
#' @description Lire le fichier des placettes à simuler avec Natura et valider le nom des colonnes.
#'
#' @inheritParams SimulNatura
#'
#' @return Table à l'échelle de la placette ou un message d'erreur s'il y a une erreur dans le nom des colonnes.
#' @export
#'
Lecture_compile <- function(file_compile, climat){


  # lire le fichier des arbres
  if (!is.data.frame(file_compile)) {
    suppressMessages(
      if (grepl(".xls", file_compile)) {comp <- readxl::read_excel(file_compile)}
      else if (grepl(".csv", file_compile)) {comp <- readr::read_delim(file_compile, delim = ";")} # fread met ID_PE numérique, mais pas read_delim
    )
  }
  else comp <- file_compile
  names(comp) <- tolower(names(comp))

  # vérification des variables obligatoires
  nom <- names(comp)

  # liste des noms de variables attendues
  nom_coor <- as.matrix(nom_variables[nom_variables$categorie=="coor","variable"])
  nom_clim <- as.matrix(nom_variables[nom_variables$categorie=="climat","variable"])
  nom_plot <- as.matrix(nom_variables[nom_variables$categorie=="plot","variable"])
  nom_dendro <- as.matrix(nom_variables[nom_variables$categorie=="dendro","variable"])

  nom_base <- c(nom_plot, nom_dendro)

  # liste des variables dans le fichier, ne garder que celles attendues et le compter
  # nom_base <- c("placette", "sdom_bio", "altitude", "type_eco",
  #               "age", "is", "hd",
  #               "nfi", "nft", "nri", "nrt", "nsab",
  #               "stfi", "stft", "stri", "strt", "stsab",
  #               "vfi", "vft", "vri", "vrt", "vsab")
  # nom_coord <- c("latitude", "longitude")
  # nom_clim <- c("p_tot","t_ma")



  # setdiff : Find Elements that Exist Only in First, But Not in Second Vector

  # vérification des noms de variables de base
  difference_nom_base <- setdiff(nom_base, nom)

  if (length(difference_nom_base) >0){
    comp =  paste0("Les variables suivantes sont requises dans le fichier d'inventaire compile : ", paste(difference_nom_base, collapse = ', '))

  }else{

    # vérification des noms de variables si climat sont à extraire : il faut lat-long
    if (isTRUE(climat)) {
      difference_nom_coor_climat <- setdiff(nom_coor, nom)

      if (length(difference_nom_coor_climat) >0) {comp = paste0("Coordonnées des placettes manquantes pour extraire le climat. Les variables suivantes sont requises : ",
                                                                paste(difference_nom_coor_climat, collapse = ', '))}
    }

    # vérification des variables climatiques s'ils sont fournis dans le fichier d'inventaire
    if (isFALSE(climat)) {

      difference_nom_clim <- setdiff(nom_clim, nom)

      if (length(difference_nom_clim) >0) {comp = paste0("Nom des variables climatiques annuelles incorrect dans le fichier d'inventaire compilé. Les variables suivantes sont requises : ",
                                                         paste(difference_nom_clim, collapse = ', '))}
    }

  }

  # vérification des noms de variables de base
  # if (length(setdiff(nom_base, nom)) >0) {comp = paste0("Nom des variables de base incorrect dans le fichier file_compile")}
  # else{
  #
  #   # verification si climat fourni
  #   if (isFALSE(climat)) {
  #     if (length(setdiff(nom_clim, nom)) >0) {arbres = paste0("Nom des variables climatiques incorrect dans le fichier des arbres")}
  #   }
  #
  #   # verification si climat non fourni
  #   if (isTRUE(climat)) {
  #     if (length(setdiff(nom_coord, nom)) >0) {arbres = paste0("Nom des variables de coordonnees incorrect dans le fichier des arbres")}
  #   }
  # }

  return(comp)
}
