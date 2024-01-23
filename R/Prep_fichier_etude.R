#' Calcule la hauteur dominante et l'âge moyen à la step 0
#'
#' @description Calcule la hauteur dominante et l'âge moyen des placettes à la step 0 à partir des arbres-études, filtrer les arbres-études.
#'
#' @param fic_etude Table des arbres-études
#' @param compile_arbre Table contenant les placettes compilées
#'
#' @return Table contenant la hauteur dominante et l'âge de chaque placette, une ligne par placette.
#' @export
#'
#' @examples
Prep_etude <- function(fic_etude, compile_arbre){


  #fic_etude=EtudeA
  #compile_arbre=DataCompile

# ajouter le dhp moyen des 4 plus gros arbres de la placette au fichier des études d'arbres
# sélectionner les variables et filtrer les états  et dhp
etude <- compile_arbre %>%
  dplyr::select(placette, dhp4_moy) %>%
  inner_join(fic_etude, by='placette') %>%
  filter(dhpcm>9, etage %in% c('C','D','c','d'), !is.na(dhp4_moy)) %>%
  filter(!is.na(age), !is.na(hauteur), hauteur>=2, age>=10, age<=400) %>%
  dplyr::select(placette, essence, dhpcm, hauteur, age, dhp4_moy)



# calculer l'age, dhp et ht moyens
# appliquer l'équation de hd pour tenir compte que la moyenne des ht n'est pas nécessairement sur les 4 plus gros arbres
etude_moy <- etude %>%
  group_by(placette) %>%
  summarise(age = mean(age),
            ht = mean(hauteur),
            dhp_moy  = mean(dhpcm),
            dhp4_moy  = mean(dhp4_moy)) %>%
  mutate(hd = 1.3 + (dhp4_moy/(  (dhp_moy/(ht-1.3)) + b2*(dhp4_moy-dhp_moy) ) )) %>%
  dplyr::select(placette, age, hd)


return(etude_moy)

}
