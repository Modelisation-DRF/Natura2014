#' Lire le fichier des arbres à simuler avec Natura et valider le nom des colonnes
#'
#' @description Lire le fichier des arbres à simuler avec Natura et valider le nom des colonnes.
#'
#' @inheritParams SimulNatura
#'
#' @return Table dont les arbres ont été filtrés ou un message d'erreur s'il y a une erreur dans le nom des colonnes.
#' @export
#'
Lecture_arbres <- function(file_arbre, ht, vol, climat){


  #file_arbre=file_arbre

# lire le fichier des arbres

# vérifier si le fichier est un objet R
if (!is.data.frame(file_arbre)) {
suppressMessages(
  if (grepl(".xls", file_arbre)) {
    arbres <- readxl::read_excel(file_arbre)
  } else if (grepl(".csv", file_arbre)) {
    arbres <- readr::read_delim(file_arbre, delim = ";") # fread met ID_PE numérique, mais pas read_delim
  }
)
}
else {arbres <- file_arbre}
names(arbres) <- tolower(names(arbres))

nom <- names(arbres)

# nom_coord <- c("latitude", "longitude")
# nom_plot <- c("placette","type_eco","altitude","sdom_bio")
# nom_clim <- c("p_tot","t_ma")
# nom_arbre <- c("essence","dhpcm","etat", "tige_ha")
# nom_vol <- c("vol_dm3")
# nom_ht <- c("hauteur_pred")



nom_coor <- as.matrix(nom_variables[nom_variables$categorie=="coor","variable"])
nom_mod_ht <- as.matrix(nom_variables[nom_variables$categorie=="modele_ht","variable"])
nom_plot <- as.matrix(nom_variables[nom_variables$categorie=="plot","variable"])
nom_clim <- as.matrix(nom_variables[nom_variables$categorie=="climat","variable"])
nom_arbre <- as.matrix(nom_variables[nom_variables$categorie=="arbre","variable"])
nom_vol <- as.matrix(nom_variables[nom_variables$categorie=="vol","variable"])
nom_ht <- as.matrix(nom_variables[nom_variables$categorie=="ht","variable"])

nom_base <- c(nom_plot, nom_arbre)



# Vérification des variables obligatoires: placette, sdom_bio, p_tot, t_ma, essence, dhpcm, type_eco, tige_ha, etat

# liste des variables dans le fichier, ne garder que celles attendues et le compter
#nom <- as.data.frame(names(arbres))
#names(nom) <- "nom"


# setdiff : Find Elements that Exist Only in First, But Not in Second Vector
difference <- setdiff(nom_base, nom)



if (length(difference) >0){

  arbres  <- paste0("Les variables suivantes sont requises dans le fichier des arbres : ", paste(difference, collapse = ', '))

} else {

  # vérification des variables si ht est à estimer et que le climat est fourni
  if (isTRUE(ht) & isFALSE(climat)) {

    difference_nom_mod_ht <- setdiff(nom_mod_ht, nom)

    if (length(difference_nom_mod_ht) >0) {
      arbres = paste0("Les variables suivantes sont requises dans le fichier des arbres pour estimer la hauteur : " , paste(difference_nom_mod_ht, collapse = ', '))
    }

  }
  # vérification des variables si ht est à estimer et que le climat n'est pas fourni
  if (isTRUE(ht) & isTRUE(climat)) {
    nom_mod_ht2 <- nom_mod_ht[-which(nom_mod_ht=="t_ma")] # enlever t_ma de la liste
    nom_mod_ht2 <- nom_mod_ht2[-which(nom_mod_ht2=="p_tot")] # enlever p_tot de la liste

    difference_nom_mod_ht2 <- setdiff(c(nom_mod_ht2, nom_coor), nom)

    if (length(difference_nom_mod_ht2) >0) {
      arbres = paste0("Les variables suivantes sont requises dans le fichier des arbres pour estimer la hauteur : " , paste(difference_nom_mod_ht2, collapse = ', '))
    }
  }

  # vérification des noms de variables si climat sont à extraire : il faut lat-long-an_mes
  if (isTRUE(climat)) {
    difference_nom_coor_nom <- setdiff(nom_coor, nom)
    if (length(difference_nom_coor_nom) >0) {arbres = paste0("Coordonnées des placettes manquantes pour extraire le climat. Les variables suivantes sont requises : ", paste(difference_nom_coor_nom, collapse = ', '))}
  }

   # vérification de la ht si elle est fournie dans le fichier d'inventaire
  if (isFALSE(ht) && isTRUE(vol)) {
    difference_nom_ht <-setdiff(nom_ht, nom)
    if (length(difference_nom_ht) >0) {arbres = paste0("Nom de la variable de hauteur incorrect dans le fichier des arbres. Les variables suivantes sont requises : " , paste(difference_nom_ht, collapse = ', '))}
  }
  # vérification du volume s'il est fourni dans le fichier d'inventaire
  if (isFALSE(vol)) {
    difference_nom_vol <- setdiff(nom_vol, nom)
    if (length(difference_nom_vol) >0) {arbres = paste0("Nom de la variable de volume incorrect dans le fichier des arbres. Les variables suivantes sont requises : " , paste(difference_nom_vol, collapse = ', '))}
  }
  # vérification des variables climatiques s'ils sont fournis dans le fichier d'inventaire mais pas besoin d'estimer la hauteur
  if (isFALSE(climat) & isFALSE(ht)) {
    difference_nom_clim <- setdiff(nom_clim, nom)
    if (length(difference_nom_clim) >0) {arbres = paste0("Nom des variables climatiques incorrect dans le fichier des arbres. Les variables suivantes sont requises : " , paste(difference_nom_clim, collapse = ', '))}
  }
  # vérification des variables climatiques s'ils sont fournis dans le fichier d'inventaire mais aussi besoin d'estimer la hauteur
  if (isFALSE(climat) & isTRUE(ht)) {
    difference_nom_clim_nom <- setdiff(nom_mod_ht, nom)

    if (length(difference_nom_clim_nom) >0) {arbres = paste0("Nom des variables climatiques incorrect dans le fichier des arbres. Les variables suivantes sont requises : " , paste(difference_nom_clim_nom, collapse = ', '))}
  }
}

# créer le sdom_bio avec seulement 2 caractères et la variable veg_pot
# arbres <- arbres %>%
#   mutate(sdom_bio = substr(sdom_bio,1,2),
#          veg_pot = substr(type_eco,1,3))

# # vérification des noms de variables de base
# if (length(setdiff(nom_base, nom)) >0) {arbres = paste0("Nom des variables de base incorrect dans le fichier des arbres")}
# else{
#
#   # vérification de la ht si elle est fournie dans le fichier d'inventaire
#   if (isFALSE(ht)) {
#     if (length(setdiff(nom_ht, nom)) >0) {arbres = paste0("Nom de la variable de hauteur incorrect dans le fichier des arbres")}
#   }
#   # vérification du volume s'il est fourni dans le fichier d'inventaire
#   if (isFALSE(vol)) {
#     if (length(setdiff(nom_vol, nom)) >0) {arbres = paste0("Nom de la variable du volume incorrect dans le fichier des arbres")}
#   }
#   # verification si climat fourni
#   if (isFALSE(climat)) {
#     if (length(setdiff(nom_clim, nom)) >0) {arbres = paste0("Nom des variables climatiques incorrect dans le fichier des arbres")}
#   }
#   # verification si climat non fourni
#   if (isTRUE(climat)) {
#     if (length(setdiff(nom_coor, nom)) >0) {arbres = paste0("Nom des variables de coordonnees incorrect dans le fichier des arbres")}
#   }
# }

return(arbres)
}
