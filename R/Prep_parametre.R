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
    dplyr::select(-sdom_bio_hd)


return(sim000)

}
