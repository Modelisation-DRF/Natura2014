#' Compilation des placettes à la step 0
#'
#' @description Filtre les états, les essences et les dhp. Filtre les végétations potentielles et les sous-domaines bioclimatiques. Crée les variables veg_pet et milieu.
#' Estime la hauteur et le volume des arbres si nécessaire, calcule l'indice de Shannon et calcule les caractéristiques dendrométriques (N, ST, V) par groupe d'essences.
#' Calcule le dhp moyen des 4 plus gros arbres pour le calcul de la hauteur dominante.
#'
#'
#' @param fic_arbre Table avec une ligne par arbre (ou par classe de dhp) pour chaque placette, avec les colonnes : placette, sdom_bio, altitude, p_tot, t_ma, essence, dhpcm, type_eco, tige_ha, etat
#' @inheritParams SimulNatura
#'
#' @return Table des caractéristiques des placettes (une ligne par placette)
#' @export
#'
Prep_arbres <- function(fic_arbre, ht, vol){

  # fic_arbre=Arbres; ht=ht; vol=vol;

Data <- fic_arbre

# Vérifier la présence des variables Ht et Vol
if (isTRUE(ht)) {
  Data <- Data %>%
    mutate(hauteur_pred=NA)
}
if (isTRUE(vol)) {
  Data <- Data %>%
    mutate(vol_dm3=NA)
}
if (isFALSE(vol) & isFALSE(ht)) {
  Data <- Data %>%
    mutate(hauteur_pred=NA) # si le volume est fourni, la hauteur n'est pas nécessaire
}

# sélectionner les variables et filtrer les états  et dhp
Data <- Data %>%
  dplyr::select(placette, sdom_bio, altitude, p_tot, t_ma, essence, dhpcm, type_eco, tige_ha, etat, hauteur_pred, vol_dm3) %>%
  mutate(sdom_bio=substr(sdom_bio,1,2),
         veg_pot = substr(type_eco,1,3),
         milieu = as.character(substr(type_eco,4,4)),
         id_pe=placette, # pour cubage et rel_h_d, il faut la variable id_pe
         no_arbre=row_number(), # pour cubage et rel_h_d, il faut la variable no_arbre
         nb_tige = tige_ha/25) %>% # pour la relation h_d, il faut les tiges dans 400 m2 et pour natura aussi
  filter(etat %in% c(10,12,40,42,30,32,50,52), dhpcm>9)

Data1 <- Data %>%
  filter(sdom_bio %in% c('1', '2E','2O','3E','3O','4E','4O','5E','5O','6E','6O')) %>%
  filter(milieu %in% c("0","1","2","3","4","5","6","7","8","9"))
  # filter(veg_pot %in% c('FE1','FE2','FE3','FE4','FE5','FE6','FC1','FO1','ME1','MF1','MJ1','MJ2','MS1','MS2','MS4','MS6','RB1','RB2','RB5','RC3','RE1','RE2','RE3','RE4','RP1','RS1','RS2','RS3','RS4','RS5','RT1'))
  # je ne mettrai pas de filtrer ici sur les essence, ça va se faire lorsque le fichier sera merge au fichier des associations d'essences


# créer un fichier des info placettes
info_plac <- Data1 %>%
  dplyr::select(placette, sdom_bio, altitude, p_tot, t_ma, type_eco, veg_pot, milieu) %>%
  group_by(placette) %>%
  unique()


# Convertir essence en groupe_ess
Data1 <- inner_join(Data1, espece, by='essence')


# Ne garder que les vp retenues
Data1 <- inner_join(Data1, vp, by='veg_pot')


# calcul de la hauteur des arbres
if (isTRUE(ht)) {
  DataHt <- TarifQC::relation_h_d(fic_arbres=Data1)
}
else DataHt <- Data1

# Calcul du volume des arbres: prévoit le volume en dm3 pour un arbre entier (ne tient pas compte du nombre d'arbres)
if (isTRUE(vol)) {
  DataHtVol <- TarifQC::cubage(fic_arbres=DataHt)
}
else DataHtVol <- DataHt


# compilation de N, St, Vol par placette/groupe_ess
compil <- DataHtVol %>%
  group_by(placette, groupe_ess) %>%
  mutate(vol_m3ha = vol_dm3/1000 * tige_ha,
         st_m2ha = pi * (dhpcm/2/100)^2 * tige_ha) %>%
  summarise(nb_tige = sum(nb_tige),
            st_m2ha = sum(st_m2ha),
            vol_m3ha = sum(vol_m3ha)) %>%
  ungroup()

# ajouter les groupes d'essences absent du fichier de données
gr <- c('Fi','Ft','Ri','Rt','Sab')
gr <- as.data.frame(gr) %>%
  rename(groupe_ess=gr) %>%
  mutate(groupe_ess = as.character(groupe_ess))
compil2 <- compil %>%
  full_join(gr, by='groupe_ess')


# transposer les groupes d'essences en colonne
compil_n <- compil2 %>%
  dplyr::select(placette, groupe_ess, nb_tige) %>%
  group_by(placette) %>%
  spread(key = groupe_ess, value = nb_tige) %>%
  filter(!is.na(placette)) %>%
  rename(nfi=Fi, nft=Ft, nri=Ri, nrt=Rt, nsab=Sab) %>%
  mutate(nfi = ifelse(is.na(nfi), 0, nfi),
         nft = ifelse(is.na(nft), 0, nft),
         nri = ifelse(is.na(nri), 0, nri),
         nrt = ifelse(is.na(nrt), 0, nrt),
         nsab = ifelse(is.na(nsab), 0, nsab),
         ncom = nfi+nft+nri+nrt+nsab
         )

compil_st <- compil2 %>%
  dplyr::select(placette, groupe_ess, st_m2ha) %>%
  group_by(placette) %>%
  spread(key = groupe_ess, value = st_m2ha) %>%
  filter(!is.na(placette)) %>%
  rename(stfi=Fi, stft=Ft, stri=Ri, strt=Rt, stsab=Sab) %>%
  mutate(stfi = ifelse(is.na(stfi), 0, stfi),
         stft = ifelse(is.na(stft), 0, stft),
         stri = ifelse(is.na(stri), 0, stri),
         strt = ifelse(is.na(strt), 0, strt),
         stsab = ifelse(is.na(stsab), 0, stsab),
         stcom = stfi+stft+stri+strt+stsab
  )

compil_v <- compil2 %>%
  dplyr::select(placette, groupe_ess, vol_m3ha) %>%
  group_by(placette) %>%
  spread(key = groupe_ess, value = vol_m3ha) %>%
  filter(!is.na(placette)) %>%
  rename(vfi=Fi, vft=Ft, vri=Ri, vrt=Rt, vsab=Sab) %>%
  mutate(vfi = ifelse(is.na(vfi), 0, vfi),
         vft = ifelse(is.na(vft), 0, vft),
         vri = ifelse(is.na(vri), 0, vri),
         vrt = ifelse(is.na(vrt), 0, vrt),
         vsab = ifelse(is.na(vsab), 0, vsab),
         vcom = vfi+vft+vri+vrt+vsab
  )


# calcul de l'indice de Shannon par placette
shanon <- DataHtVol %>%
  # sommation des arbres par classe de 2 cm, toutes essences confondues
  mutate(st_m2ha = pi * (dhpcm/2/100)^2 * tige_ha,
         cl_dhp=round((dhpcm-0.1)/2,0)*2) %>%
  group_by(placette, cl_dhp) %>%
  summarise(sttiges_cls = sum(st_m2ha)) %>%
  group_by(placette) %>%
  mutate(st_ha_tot = sum(sttiges_cls),
         p=sttiges_cls/st_ha_tot,
         p_logp = p * log(p)
         ) %>%
  summarise(is = -sum(p_logp)/log(19))


# calcul du dhp moyen des 4 plus gros arbres (pour le calcul de la hauteur dominante)
dhp4 <- DataHtVol %>%
  dplyr::select(placette, dhpcm, nb_tige) %>%
  group_by(placette) %>%
  arrange(placette, desc(dhpcm)) %>%
  mutate(somme_poids = cumsum(nb_tige),
         somme_poids_prec = lag(somme_poids),
         somme_poids_prec = ifelse(is.na(somme_poids_prec),0,somme_poids_prec),
         select_id = ifelse(somme_poids_prec>=4 & somme_poids>4,0,1)) %>%
  filter(select_id==1) %>%
  mutate(poids = ifelse(somme_poids<=4, nb_tige, 4-somme_poids_prec)) %>%
  summarise(dhp4_nb = round(as.numeric(sum(poids)),0), # il y avait un problème d'arrondi lorsqu'il y avait des fractions d'arbres, même si la somme des poids etait 4, la selection >4 ne fonctionnait pas
            dhp4_moy = sum(poids*dhpcm)/sum(poids),
            dhp4_moy = ifelse(dhp4_nb<4, NA, dhp4_moy)) %>%
  dplyr::select(-dhp4_nb)



# mettre tous les fichiers compilés ensemble
compil3 <- inner_join(info_plac, compil_n, by = "placette")
compil4 <- inner_join(compil3, compil_st, by = "placette")
compil5 <- inner_join(compil4, compil_v, by = "placette")
compil6 <- inner_join(compil5, shanon, by = "placette")
compil7 <- inner_join(compil6, dhp4, by = "placette")


return(compil7)
}
