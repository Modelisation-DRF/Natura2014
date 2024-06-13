#' Lire le fichier des arbres à simuler avec Natura et valider le nom des colonnes
#'
#' @description Lire le fichier des arbres à simuler avec Natura et valider le nom des colonnes.
#'
#' @inheritParams SimulNatura
#'
#' @return Table dont les arbres ont été filtrés ou un message d'erreur s'il y a une erreur dans le nom des colonnes.
#' @export
#'
Lecture_arbres <- function(file_arbre, ht, vol){


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

nom_clim <- c("p_tot","t_ma")
nom_arbre <- c("essence","dhpcm","etat", "tige_ha")
nom_plot <- c("placette","type_eco","altitude","sdom_bio")
nom_vol <- c("vol_dm3")
nom_ht <- c("hauteur_pred")

nom_base <- c(nom_plot, nom_arbre, nom_clim)



# Vérification des variables obligatoires: placette, sdom_bio, altitude, p_tot, t_ma, essence, dhpcm, type_eco, tige_ha, etat

# liste des variables dans le fichier, ne garder que celles attendues et le compter
#nom <- as.data.frame(names(arbres))
#names(nom) <- "nom"


# vérification des noms de variables de base si ht et vol doivent etre calculés
if (isTRUE(ht) & isTRUE(vol)) {
  if (length(setdiff(nom_base, nom)) >0) {arbres = paste0("Nom des variables incorrect dans le fichier des arbres")}
}
else{

  # vérification de la ht si elle est fournie dans le fichier d'inventaire
  if (isFALSE(ht)) {
    if (length(setdiff(nom_ht, nom)) >0) {arbres = paste0("Nom de la variable de hauteur incorrect dans le fichier des arbres")}
  }
  # vérification du volume s'il est fourni dans le fichier d'inventaire
  if (isFALSE(vol)) {
    if (length(setdiff(nom_vol, nom)) >0) {arbres = paste0("Nom de la variable du volume incorrect dans le fichier des arbres")}
  }
}

return(arbres)
}
