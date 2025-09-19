#' Calcule la hauteur dominante et l'âge moyen à la step 0
#'
#' @description Calcule la hauteur dominante et l'âge moyen des placettes à la step 0 à partir des arbres-études, filtrer les arbres-études.
#'
#' @param fic_etude Table des arbres-études
#' @param fic_arbre Table contenant les placettes compilées
#'
#' @return Table contenant la hauteur dominante et l'âge de chaque placette, une ligne par placette.
#' @export
#'
Prep_etude <- function(fic_etude, fic_arbre){


  #fic_etude=EtudeA
  #compile_arbre=DataCompile

  # fic_etude=EtudeA; fic_arbre=Arbres_prep;

 # calcul du dhp moyen des 4 plus gros arbres (pour le calcul de la hauteur dominante)
  dhp4 <- fic_arbre %>%
    dplyr::select(placette, dhpcm, nb_tige) %>%
    group_by(placette) %>%
    arrange(placette, desc(dhpcm)) %>%
    mutate(somme_poids = cumsum(nb_tige),
           somme_poids_prec = dplyr::lag(somme_poids),
           somme_poids_prec = ifelse(is.na(somme_poids_prec),0,somme_poids_prec),
           select_id = ifelse(somme_poids_prec>=4 & somme_poids>4,0,1)) %>%
    filter(select_id==1) %>%
    mutate(poids = ifelse(somme_poids<=4, nb_tige, 4-somme_poids_prec)) %>%
    summarise(dhp4_nb = round(as.numeric(sum(poids)),0), # il y avait un problème d'arrondi lorsqu'il y avait des fractions d'arbres, même si la somme des poids etait 4, la selection >4 ne fonctionnait pas
              dhp4_moy = sum(poids*dhpcm)/sum(poids),
              dhp4_moy = ifelse(dhp4_nb<4, NA, dhp4_moy)) %>%
    dplyr::select(-dhp4_nb)


# ajouter le dhp moyen des 4 plus gros arbres de la placette au fichier des études d'arbres
etude <- dhp4 %>%
  #dplyr::select(placette, dhp4_moy) %>%
  inner_join(fic_etude, by='placette')
  #%>%
  #filter(dhpcm>9, etage %in% c('C','D','c','d'), !is.na(dhp4_moy)) %>%
  #filter(!is.na(age), !is.na(hauteur), hauteur>=2, age>=10, age<=400) %>%
  #dplyr::select(placette, essence, dhpcm, hauteur, age, dhp4_moy)



# calculer l'age, dhp et ht moyens des etudes
# appliquer l'équation de hd pour tenir compte que la moyenne des ht n'est pas nécessairement sur les 4 plus gros arbres
etude_moy <- etude %>%
  group_by(placette) %>%
  summarise(age = mean(age),
            ht = mean(hauteur),
            dhp_moy  = mean(dhpcm),
            dhp4_moy  = mean(dhp4_moy)) %>%
  mutate(hd = 1.3 + (dhp4_moy/(  (dhp_moy/(ht-1.3)) + b2*(dhp4_moy-dhp_moy) ) )) %>%
  dplyr::select(placette, age, hd)

# si on applique l'équation à chaque arbre et ensuite on fait la moyenne
# etude_moy <- etude %>%
#   group_by(placette) %>%
#   mutate(hd = 1.3 + (dhp4_moy/(  (dhpcm/(ht-1.3)) + b2*(dhp4_moy-dhpcm) ) )) %>%
#   summarise(age = mean(age),
#             ht = mean(hauteur),
#             dhp_moy  = mean(dhpcm),
#             dhp4_moy  = mean(dhp4_moy),
#             hd = mean(ht)) %>%
#  dplyr::select(placette, age, hd)

return(etude_moy)

}
