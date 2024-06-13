#' Lire les paramètres des équations de Natura-2014 et les joint au fichier des placettes
#'
#' @description Lit les paramètres des équations de Natura-2014 et les joint au fichier des placettes.
#'
#' @param data Table contenant les placettes compilées
#'
#' @return Table contenant toutes les colonnes nécessaires pour effectuer une simulation, incluant les paramètres
#' @export
#'
Prep_parametre<-function(data){


   # on met les paramètres des 5 équations des groupes d'essences ensemble
  parms1 <- bind_cols(ParmsFi, ParmsFt, ParmsRi, ParmsRt, ParmsSab) %>%
    mutate(id=1)

  # mettre hd et isd ensemble
  parms_hd_is <- inner_join(ParmsIsd, ParmsHd, by='sdom_bio')


  # ajouter les moyennes
  parms_hd_is2 <- inner_join(parms_hd_is, ParmsMoy, by='sdom_bio') %>%
        rename(cchmax=chmax, ccdt=cdt, ccis=cis, bbis=bis, bbage=bage, bbdt=bdt) %>%
        mutate(id=1)

  # mettre tous les paramètres dans un seul fichier
  parms_tous <- left_join(parms_hd_is2, parms1, by='id') %>%
    rename(sdom_bio_hd=sdom_bio) %>%
    mutate(sdom_bio_hd = as.character(sdom_bio_hd)) %>%
    dplyr::select(-id)

  # point de départ des simulations avec les paramètres
  sim000 <- data %>%
    mutate(sdom_bio_hd = ifelse(sdom_bio %in% c('1','1O'), '2O', sdom_bio)) %>%
    inner_join(parms_tous, by='sdom_bio_hd') %>%
    dplyr::select(-sdom_bio_hd) %>%
    mutate(
  ifi = ifelse(nfi>0, 1, 0),
  ift = ifelse(nft>0, 1, 0),
  iri = ifelse(nri>0, 1, 0),
  irt = ifelse(nrt>0, 1, 0),
  isab = ifelse(nsab>0, 1, 0),

  sd1 = ifelse(sdom_bio %in% c('1', '1O'), 1, 0),
  sd2e = ifelse(sdom_bio == '2E', 1, 0),
  sd2o = ifelse(sdom_bio == '2O', 1, 0),
  sd3e = ifelse(sdom_bio == '3E', 1, 0),
  sd3o = ifelse(sdom_bio == '3O', 1, 0),
  sd4e = ifelse(sdom_bio == '4E', 1, 0),
  sd4o = ifelse(sdom_bio == '4O', 1, 0),
  sd5e = ifelse(sdom_bio == '5E', 1, 0),
  sd5o = ifelse(sdom_bio == '5O', 1, 0),
  sd6e = ifelse(sdom_bio == '6E', 1, 0),
  sd6o = ifelse(sdom_bio == '6O', 1, 0),

  vpfe2 = ifelse(veg_pot %in% c('FE2','FE1'), 1, 0),
  vpfe3 = ifelse(veg_pot %in% c('FE3','FE4'), 1, 0),
  vpfe6 = ifelse(veg_pot %in% c('FE6','FC1','FE5'), 1, 0),
  vpmf1 = ifelse(veg_pot %in% c('MF1','FO1'), 1, 0),
  vpmj1 = ifelse(veg_pot=='MJ1', 1, 0),
  vpmj2 = ifelse(veg_pot=='MJ2', 1, 0),
  vpms1 = ifelse(veg_pot=='MS1', 1, 0),
  vpms2 = ifelse(veg_pot %in% c('MS2','MS4','ME1'), 1, 0),
  vpms6 = ifelse(veg_pot=='MS6', 1, 0),
  vprb_ = ifelse(veg_pot %in% c('RB5','RB1','RB2'), 1, 0),
  vprc3 = ifelse(veg_pot=='RC3', 1, 0),
  vpre1 = ifelse(veg_pot=='RE1', 1, 0),
  vpre2 = ifelse(veg_pot %in% c('RE2','RE4'), 1, 0),
  vpre3 = ifelse(veg_pot=='RE3', 1, 0),
  vprp1 = ifelse(veg_pot=='RP1', 1, 0),
  vprs1 = ifelse(veg_pot=='RS1', 1, 0),
  vprs2 = ifelse(veg_pot %in% c('RS2','RS4','RS5'), 1, 0),
  vprs3 = ifelse(veg_pot=='RS3', 1, 0),
  vprt1 = ifelse(veg_pot=='RT1', 1, 0)
  ) %>%
  rename(tmoy = t_ma, ptot=p_tot)


return(sim000)

}
