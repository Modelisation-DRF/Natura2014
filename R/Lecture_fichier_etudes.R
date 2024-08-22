#' Lire le fichier des arbres-études et valider le nom des colonnes
#'
#' @description Lire le fichier des arbres-études et valider le nom des colonnes.
#'
#' @param file_etude Nom du fichier à lire (table ou Excel)
#'
#' @return Table dont les arbres-études ont été filtrés ou un message d'erreur s'il y a une erreur dans le nom des colonnes.
#' @export
#'
Lecture_etudes <- function(file_etude){

  # lire le fichier des arbres-etudes
  if (!is.data.frame(file_etude)) {
    suppressMessages(
      if (grepl(".xls", file_etude)) {etudes <- readxl::read_excel(file_etude)}
      else if (grepl(".csv", file_etude)) {etudes <- readr::read_delim(file_etude, delim = ";")} # fread met ID_PE numérique, mais pas read_delim
    )
  }
  else etudes <- file_etude
  names(etudes) <- tolower(names(etudes))

  # vérification des variables obligatoires: placette, etage, essence, dhpcm, hauteur, age

  # liste des variables dans le fichier, ne garder que celles attendues et le compter
  nom <- names(etudes)

  #nom_base <- c("placette", "essence", "etage", "dhpcm", "hauteur", "age")
  nom_base <- as.matrix(nom_variables[nom_variables$categorie=="etude","variable"])


  # setdiff : Find Elements that Exist Only in First, But Not in Second Vector
  difference_nom_etude <- setdiff(nom_base, nom)

  # vérification des noms de variables de base
  #if (length(setdiff(nom_base, nom)) >0) {etudes = paste0("Nom des variables incorrect dans le fichier des arbres-etudes")}
  if (length(difference_nom_etude) >0) {etudes = paste0("Nom des variables incorrect dans le fichier des arbres-études. Les variables suivantes sont requises : ",
                                                        paste(difference_nom_etude, collapse = ', '))
  }else if(!any(etudes$etage %in% c("C", "D"))){

    etudes = paste0("Aucun arbre avec l'étage C ou D " )
  }


  return(etudes)
}
