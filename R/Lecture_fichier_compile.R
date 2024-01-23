#' Lire le fichier des placettes à simuler avec Natura et valider le nom des colonnes
#'
#' @description Lire le fichier des placettes à simuler avec Natura et valider le nom des colonnes.
#'
#' @param file Nom du fichier des placette à lire (table ou Excel)
#'
#' @return Table à l'échelle de la placette ou un message d'erreur s'il y a une erreur dans le nom des colonnes.
#' @export
#'
#' @examples
Lecture_compile <- function(file_compile){


  # lire le fichier des arbres
  if (!is.data.frame(file_compile)) {
    suppressMessages(
      if (grepl(".xls", file_compile)) {comp <- read_excel(file_compile)}
      else if (grepl(".csv", file_compile)) {comp <- read_delim(file_compile, delim = ";")} # fread met ID_PE numérique, mais pas read_delim
    )
  }
  else comp <- file_compile
  names(comp) <- tolower(names(comp))

  # vérification des variables obligatoires
  nom <- names(comp)

  # liste des variables dans le fichier, ne garder que celles attendues et le compter

  nom_base <- c("placette", "sdom_bio", "altitude", "p_tot", "t_ma", "type_eco",
                "age", "is", "hd",
                "nfi", "nft", "nri", "nrt", "nsab",
                "stfi", "stft", "stri", "strt", "stsab",
                "vfi", "vft", "vri", "vrt", "vsab")



  # setdiff : Find Elements that Exist Only in First, But Not in Second Vector

  # vérification des noms de variables de base
  if (length(setdiff(nom_base, nom)) >0) {comp = paste0("Nom des variables incorrect dans le fichier file_compile")}

  return(comp)
}
