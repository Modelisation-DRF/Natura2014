#' Lire le fichier des placettes à simuler avec Natura et valider le nom des colonnes
#'
#' @description Lire le fichier des placettes à simuler avec Natura et valider le nom des colonnes.
#'
#' @inheritParams SimulNatura
#'
#' @return Table à l'échelle de la placette ou un message d'erreur s'il y a une erreur dans le nom des colonnes.
#' @export
#'
Lecture_compile <- function(file_compile, climat) {
  # lire le fichier des arbres
  if (!is.data.frame(file_compile)) {
    suppressMessages(
      if (grepl(".xls", file_compile)) {
        comp <- readxl::read_excel(file_compile)
      } else if (grepl(".csv", file_compile)) {
        comp <- readr::read_delim(file_compile, delim = ";")
      } # fread met ID_PE numérique, mais pas read_delim
    )
  } else {
    comp <- file_compile
  }
  names(comp) <- tolower(names(comp))

  # vérification des variables obligatoires
  nom <- names(comp)

  # liste des variables dans le fichier, ne garder que celles attendues et le compter

  nom_base <- c(
    "placette", "sdom_bio", "altitude", "type_eco",
    "age", "is", "hd",
    "nfi", "nft", "nri", "nrt", "nsab",
    "stfi", "stft", "stri", "strt", "stsab",
    "vfi", "vft", "vri", "vrt", "vsab"
  )
  nom_coord <- c("latitude", "longitude")
  nom_clim <- c("p_tot", "t_ma")



  # setdiff : Find Elements that Exist Only in First, But Not in Second Vector

  # vérification des noms de variables de base
  nom_base_manquant <- setdiff(nom_base, nom)
  if (length(nom_base_manquant) > 0) {
    comp <- paste("Nom des variables de base incorrect dans le fichier file_compile: ", paste(nom_base_manquant, collapse = ", "))
  } else {
    # verification si climat fourni
    nom_clim_manquant <- setdiff(nom_clim, nom)
    if (isFALSE(climat) && length(nom_clim_manquant) > 0) {
      arbres <- paste("Nom des variables climatiques incorrect dans le fichier des arbres: ", paste(nom_clim_manquant, collapse = ", "))
    }

    # verification si climat non fourni
    nom_coord_manquant <- setdiff(nom_coord, nom)
    if (isTRUE(climat) && length(nom_coord_manquant) > 0) {
      arbres <- paste("Nom des variables de coordonnees incorrect dans le fichier des arbres: ", paste(nom_coord_manquant, collapse = ", "))
    }
  }

  return(comp)
}
