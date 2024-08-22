#' Préparation du fichier compilé à la step 0 pour la simulation dans Natura
#'
#' @description Préparation du fichier compilé à la step 0 pour la simulation dans Natura: création des variables nécessaires et retrait des variables inutiles
#'
#' @param fichier_compile Table avec une ligne par placette
#'
#' @return Le fichier \code{fichier_compile} avec toutes les variables nécessaires au modèle Natura
# #' @export
#'
# @examples
Prep_compile <- function(fichier_compile){

 # fichier_compile=DataCompile_final0

  nom_plot <- as.matrix(nom_variables[nom_variables$categorie=="plot","variable"])
  nom_dendro <- as.matrix(nom_variables[nom_variables$categorie=="dendro","variable"])
  nom_clim <- as.matrix(nom_variables[nom_variables$categorie=="climat","variable"])
  nom_sol <- as.matrix(nom_variables[nom_variables$categorie=="sol","variable"])

  # listes des variables nécessaires
  var_base <- c(nom_plot, nom_dendro, nom_clim)

  # nom des variables dans le fichier
  nom_fic <- names(fichier_compile)
  # variables non nécessaires
  retrait <- setdiff(nom_fic, var_base) # setdiff(x, y) finds all rows in x that aren't in y
  # retirer les variables non nécessaires
  fichier_compile[retrait] <- list(NULL)


  # préparation des variables nécessaires
  compile <- fichier_compile %>%
    mutate(annee=0,
           tbe=0,
           pert=0,

           sdom_bio = substr(sdom_bio,1,2),
           veg_pot = substr(type_eco,1,3),
           milieu = substr(type_eco,4,4),

           ncom = nfi+nft+nri+nrt+nsab,
           stcom = stfi+stft+stri+strt+stsab,
           vcom = vfi+vft+vri+vrt+vsab,

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


return(compile)

}
