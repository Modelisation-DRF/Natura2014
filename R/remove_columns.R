
#' Cette fonction supprime les colonnes spécifiées d'un data frame si elles existent.
#'
#' @param data Un data frame à partir duquel les colonnes seront supprimées.
#' @param columns_to_remove Un vecteur de caractères contenant les noms des colonnes à vérifier et à supprimer si elles existent dans le data frame.
#'
#' @return Un data frame avec les colonnes spécifiées supprimées si elles existent.
#' @export
#'
#' @examples
#'
#' # Liste des colonnes à supprimer
#' variable_climat <- c("p_tot", "t_ma")
#'
#' # Appliquer la fonction pour supprimer les colonnes spécifiées
#' data <- remove_columns(data, columns_to_remove)
#'
#'


remove_columns <- function(data, columns_to_remove) {

  names(data) <- tolower(names(data))

  columns_present <- columns_to_remove[columns_to_remove %in% colnames(data)]

  data <- data[, !(colnames(data) %in% columns_present)]

  return(data)
}






#' Valide la correspondance des placettes entre le fichier des arbres et celui des arbres-études
#'
#' Cette fonction vérifie que toutes les placettes présentes dans le fichier des arbres sont également présentes dans le fichier des arbres-études
#'
#' @param data_arbre Nom du fichier contenant les informations sur les arbres et les placettes
#' @param data_etude Nom du fichier contenant les arbres-études
#' @return Une liste de messages d'erreurs. Si aucune erreur n'est trouvée, une liste vide est retournée.
#' @examples
#'
#' data_arbre <- data.frame(Placette = c("TEM23APC5001", "TEM23APC5002", "TEM23APC5003"))
#' data_etude <- data.frame(Placette = c("TEM23APC5001", "TEM23APC5002"))
#' valide_placette_etudes(data_arbre, data_etude)
#' # Les erreurs seront retournées comme ceci
#' # 'Aucune des placettes suivantes ne sont valide dans le fichier des arbres-études : TEM23APC5003'
#' @export
valide_placette_etudes <-function (data_arbre , data_etude){

  names(data_arbre) <- tolower(names(data_arbre))
  names(data_etude) <- tolower(names(data_etude))

  erreurs <-list()

  if(!"id_pe" %in% names(data_arbre) ||!"id_pe" %in% names(data_etude) ){
    erreurs<-paste("La colonne 'Placette' est manquante dans le fichier etude ou dans le fichier des arbres.")

    return(erreurs)
  }else  if(length(data_arbre$id_pe) == 0|| length(data_etude$id_pe) == 0){
    erreurs<-paste("La colonne 'Placette' est vide dans le fichier etude ou dans le fichier des arbres.")
    return(erreurs)
  }


  pacette_arbre <- unique(data_arbre$id_pe)
  placette_etude <- unique(data_etude$id_pe)

  diff_placette <- setdiff(pacette_arbre, placette_etude)

  erreurs <-list()

  if (length(diff_placette) != 0 & all(pacette_arbre == diff_placette)) {
    erreurs <- paste("Aucune des placettes suivantes ne sont valides dans le fichier des arbres-études : ", paste(diff_placette, collapse = ", "))
  }



  return(erreurs)
}
