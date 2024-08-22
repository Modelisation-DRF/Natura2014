# data: fichier compilé à la placette par groupe d'essences, départ des simulations
# horizon: nombre de décennies à simuler, max 15 (150 ans)
# dec_perturb: numéro de la décennie avec une pertunation (chablis partielle, brulis partiel), mettre 0 si pas de perturbation
# dec_tbe1: numéro de la première décennie avec une épidémie de TBE, mettre 0 si pas d'épidémie
# TBE1: sévérit de la première épidémie, un nombre de 1 à 5, mettre 0 si pas de TBE
# dec_tbe2: numéro de la deuxième décennie avec une épidémie de TBE, mettre 0 si pas de 2e épidémie
# TBE2: sévérité de la deuxième épidémie, nombre de 1 à 5, mettre 0 si pas de 2e épidémie

#' Effectue une simulation complète (tous les pas de simulation) avec le modèle Natura-2014
#'
#' @description Effectue une simulation complète (tous les pas de simulation) avec le modèle Natura-2014
#'
#' @param data Fichier compilé à la placette par groupe d'essences, départ des simulations
#' @inheritParams SimulNatura
#'
#' @return Table avec une ligne par placette et pas de simulation contenant les prévision de HD et IS et de N, ST et V de chacun des groupes d'essences
#' @export
#'
Natura <- function(data, horizon, dec_perturb=0, dec_tbe1=0, tbe1=0, dec_tbe2=0, tbe2=0){

# data=Data_compile2; horizon=horizon; dec_perturb=dec_perturb; dec_tbe1=dec_tbe1; tbe1=tbe1; dec_tbe2=dec_tbe2; tbe2=tbe2;

  #Longueur d'un pas de simulation
  dt <- 10

  # Après coupe forcé à 0
  apresc <- 0


  # fichier de départ des simulation avec année de départ
  Plac <- data %>% mutate(annee = 0)



  # Création placette d'origine
  PlacOri <- Plac %>%
    mutate(tbe=0, pert=0) %>%
    dplyr::select(placette, annee, age, hd, is,
           stfi, stft, stri, strt, stsab, stcom,
           nfi, nft, nri, nrt, nsab, ncom,
           vfi, vft, vri, vrt, vsab, vcom,
           tbe, pert)


  # Initialisation du fichier qui accumulera les résultats de simulation
  outputTot <- c()



  ######################### boucle pour les k décennies a simuler ##########################
  for (k in 1:horizon){


    ###################################### Mise a jour des variables  #######################

    if  (k==1){                # Si premier pas de simulation, on utilise le fichier de depart de la placette
      Plac <- Plac %>%
        mutate(annee = k*dt)
      }
      else {                    # Si 2e pas de simulation ou plus, on prend le fichier qui contient les simulations et on garde seulement le dernier pas
        Plac <- outputTot %>%
          filter(annee == (k*dt)-dt) %>%
          mutate(annee = k*dt)
       }

      # si on est à la décennie de la perturbation, mettre la variable indicatrice de perturb à 1
      if (k==dec_perturb) {ind_pert <- 1}
        else {ind_pert <- 0}


      # si on est à la décennie de la tbe , mettre la variable tbe à la bonne valeur
      if (k==dec_tbe1) {
        ind_tbe <- tbe1
      } else if (k==dec_tbe2) {
        ind_tbe <- tbe2
      } else {ind_tbe <- 0}

      # s'il y a eu de la TBE ou de la perturb à la décennie précédente, on met la variable indicatrice apresp à 1
      if ((dec_tbe1>0 & k==dec_tbe1+1) | (dec_tbe2>0 & k==dec_tbe2+1) | (dec_perturb>0 & k==dec_perturb+1)) {apresp <- 1}
        else {apresp <- 0}


      # Application des équations
      Plac2 <- Plac %>%
        mutate(tbe = ind_tbe, pert = ind_pert,

             # Prédiction de HD
             h_max = 1+((hd-1)/(1-exp(-b9*age))^(b9*b8)),
             h_max = ifelse(h_max>40, 40, h_max),          # 20170403: il y a des cas où hmax devient bcp trop grand, mauvaise combinaison age1-hd1
             beta8 = b9*(1+cchmax*(h_max-h_max_m)/h_max_m)*(1+ccdt*(dt-dt_m)/dt_m)*(1+ccis*(is-is_m)/is_m),
             beta8 = ifelse(beta8>0.30, 0.3,             # 20170403: malgré un contrôle de hmax, beta8 peut déraper
                            ifelse(beta8<0, 0.0000001,   # 20170403: devient négatif quand hmax est trop petit, trop petit hd pour l'âge
                                   beta8)),
             beta9 = b8*beta8,
             beta9 = ifelse(beta9>17, 17, beta9),         # 20170403: malgré un contrôle de hmax, beta9 peut déraper
             hd2_p = 1 + (hd-1)*((1-exp(-beta8*(age+dt)))/(1-exp(-beta8*age)))^beta9,

             # Prédiction de l'indice de Shannon
             is2_p = int + bbis*is + bbage*age + bbdt*dt,

             # prédiction du nombre de tiges par groupe d'espèces
             nfi2_p = ifi*(exp(
               + fdt*dt
               + (fsd1*sd1 + fsd2e*sd2e + fsd2o*sd2o + fsd3e*sd3e + fsd3o*sd3o + fsd4e*sd4e + fsd4o*sd4o + fsd5e*sd5e + fsd5o*sd5o + fsd6e*sd6e + fsd6o*sd6o)
               + (fnsd1*sd1 + fnsd2e*sd2e + fnsd2o*sd2o + fnsd3e*sd3e + fnsd3o*sd3o + fnsd4e*sd4e + fnsd4o*sd4o + fnsd5e*sd5e + fnsd5o*sd5o + fnsd6e*sd6e + fnsd6o*sd6o)*log(nfi+1)
               + fhd*hd
               + fagenfi*age*log(nfi+1)
               + fageprop*(age*stfi/stcom)
               + ftbe*tbe
               + (0*sd1 + fstsd2e*sd2e + 0*sd2o + fstsd3e*sd3e + fstsd3o*sd3o + fstsd4e*sd4e + fstsd4o*sd4o + fstsd5e*sd5e + fstsd5o*sd5o + fstsd6e*sd6e + fstsd6o*sd6o)*log(stfi+0.1)

               + fperturb*pert
               + fntot*log(ncom)

               + (fvpfe2*vpfe2 +fvpfe3*vpfe3 +fvpfe6*vpfe6 +fvpmf1*vpmf1 +fvpmj1*vpmj1 +fvpmj2*vpmj2 +fvpms1*vpms1 +0*vpms2 +fvpms6*vpms6 +fvprb_*vprb_
                  +fvprc3*vprc3 +fvpre1*vpre1 +fvpre2*vpre2 +fvpre3*vpre3 +fvprp1*vprp1 +0*vprs1 +fvprs2*vprs2 +fvprs3*vprs3 +fvprt1*vprt1)
               + (fnvpfe2*vpfe2 +fnvpfe3*vpfe3 +0*vpfe6 +fnvpmf1*vpmf1 +fnvpmj1*vpmj1 +fnvpmj2*vpmj2 +fnvpms1*vpms1 +0*vpms2 +0*vpms6 +fnvprb_*vprb_
                  +0*vprc3 +0*vpre1 +0*vpre2 +0*vpre3 +0*vprp1 +0*vprs1 +0*vprs2 +0*vprs3 +0*vprt1)*log(nfi+1)

               + ftmoy*tmoy)
               ),
             nft2_p = ift*(exp(
               + gdt*dt
               + (gsd1*sd1 + gsd2e*sd2e + gsd2o*sd2o + gsd3e*sd3e + gsd3o*sd3o + gsd4e*sd4e + gsd4o*sd4o + gsd5e6e*sd5e + gsd5o6o*sd5o + gsd5e6e*sd6e + gsd5o6o*sd6o)
               + (gnsd1*sd1 + gnsd2e*sd2e + gnsd2o*sd2o + gnsd3e*sd3e + gnsd3o*sd3o + gnsd4e*sd4e + gnsd4o*sd4o + gnsd5e6e*sd5e + gnsd5o6o*sd5o + gnsd5e6e*sd6e + gnsd5o6o*sd6o)*log(nft+1)
               + (gstsd1*sd1 + gstsd2e*sd2e + gstsd2o*sd2o + gstsd3e*sd3e + gstsd3o*sd3o + gstsd4e*sd4e + gstsd4o*sd4o + gstsd5e6e*sd5e + gstsd5o6o*sd5o + gstsd5e6e*sd6e + gstsd5o6o*sd6o)*log(stft+0.1)
               + ghd*hd
               + gage*(1/age)
               + gntot*log(ncom)
               + (gvpfe2*vpfe2 +0*vpfe3 +gvpfe6*vpfe6 +0*vpmf1 +gvpmj1*vpmj1 +gvpmj2*vpmj2 +gvpms1*vpms1 +gvpms2*vpms2 +gvpms6*vpms6
                   +gvpms1*vprb_ +gvpre_*vprc3 +gvpre_*vpre1 +gvpre_*vpre2 +gvpre_*vpre3 +gvprp1*vprp1 +gvprs1*vprs1 +gvprs2*vprs2 +gvprs2*vprs3 +gvprt1*vprt1)
               + (gvpnfe2*vpfe2 +0*vpfe3 +0*vpfe6 +0*vpmf1 +gvpnmj1*vpmj1 +gvpnmj2*vpmj2 +gvpnms1*vpms1 +gvpnms2*vpms2 +gvpnms6*vpms6
                   +gvpnms1*vprb_ +gvpnre_*vprc3 +gvpnre_*vpre1 +gvpnre_*vpre2 +gvpnre_*vpre3 +gvpnrp1*vprp1 +gvpnrs1*vprs1 +gvpnrs2*vprs2 +gvpnrs2*vprs3 +gvpnrt1*vprt1)*log(nft+1))
             ),
             nri2_p = iri*(exp(
               + hdt*dt
               + (hsd123*sd1 + hsd123*sd2e + hsd123*sd2o + hsd123*sd3e + hsd123*sd3o + hsd4*sd4e + hsd4*sd4o + hsd5*sd5e + hsd5*sd5o + hsd6*sd6e + hsd6*sd6o)
               + (hnsd123*sd1 + hnsd123*sd2e + hnsd123*sd2o + hnsd123*sd3e + hnsd123*sd3o + hnsd4*sd4e + hnsd4*sd4o + hnsd5*sd5e + hnsd5*sd5o + hnsd6*sd6e + hnsd6*sd6o)*log(nri+1)
               + (0*sd1 + 0*sd2e + 0*sd2o + 0*sd3e + 0*sd3o + 0*sd4e + 0*sd4o + hstsd5*sd5e + hstsd5*sd5o + hstsd6*sd6e + hstsd6*sd6o)*log(stri+0.1)

               + hage*age*log(nri+1)
               + hhd*hd
               + (0*sd1 + 0*sd2e + 0*sd2o + 0*sd3e + 0*sd3o + 0*sd4e + hpertsd4o*sd4o + 0*sd5e + hpertsd5o*sd5o + 0*sd6e + hpertsd6o*sd6o)*pert
               + hntot*log(ncom)

               + (hvpfeu*vpfe2 +hvpfeu*vpfe3 +hvpfeu*vpfe6 +hvpfeu*vpmf1 +hvpfeu*vpmj1 +hvpfeu*vpmj2 +hvpfeu*vpms1 +hvpms2*vpms2 +hvpms2*vpms6 +hvpms2*vprb_
                   +hvpfeu*vprc3 +hvpre1*vpre1 +0*vpre2 +hvpre3*vpre3 +hvprprt*vprp1 +hvprs1*vprs1 +hvprs1*vprs2 +hvpre3*vprs3 +hvprprt*vprt1)

               + (0*vpfe2 +0*vpfe3 +0*vpfe6 +0*vpmf1 +0*vpmj1 +0*vpmj2 +0*vpms1 +hnvpms2*vpms2 +hnvpms2*vpms6 +hnvpms2*vprb_
                   +0*vprc3 +0*vpre1 +0*vpre2 +0*vpre3 +0*vprp1 +0*vprs1 +0*vprs2 +0*vprs3 +0*vprt1)*log(nri+1))
               ),
             nrt2_p = irt*(exp(
               + idt*dt
               + (isd1*sd1 + isd2e*sd2e + isd2o*sd2o + isd3e*sd3e + isd3o*sd3o + isd4e*sd4e + isd4o*sd4o + isd5e*sd5e + isd5o*sd5o + isd6e*sd6e + isd6o*sd6o)
               + (insd1*sd1 + insd2e*sd2e + insd2o*sd2o + insd3e*sd3e + insd3o*sd3o + insd4e*sd4e + insd4o*sd4o + insd5e*sd5e + insd5o*sd5o + insd6e*sd6e + insd6o*sd6o)*log(nrt+1)

               + (istsd1*sd1 + istsd2e*sd2e + istsd2o*sd2o + istsd3e*sd3e + istsd3o*sd3o + istsd4e*sd4e + istsd4o*sd4o + istsd5e*sd5e + istsd5o*sd5o + istsd6e*sd6e + istsd6o*sd6o)*log(strt+0.1)

               + iage*log(nrt+1)*(1/age)

               + (ihdsd12o3o*sd1 + ihdsd2e3e4e5e6e*sd2e + ihdsd12o3o*sd2o + ihdsd2e3e4e5e6e*sd3e + ihdsd12o3o*sd3o + ihdsd2e3e4e5e6e*sd4e + ihdsd4o5o6o*sd4o + ihdsd2e3e4e5e6e*sd5e + ihdsd4o5o6o*sd5o + ihdsd2e3e4e5e6e*sd6e + ihdsd4o5o6o*sd6o)*hd

               + itbe*tbe
               + (0*sd1 + 0*sd2e + 0*sd2o + 0*sd3e + ipert3o*sd3o + ipert4e*sd4e + ipert4o*sd4o + ipert5e*sd5e + ipert5o*sd5o + ipert6e*sd6e + ipert6o*sd6o)*pert*log(nrt+1)

               + intot*log(ncom)

               + (ivpfeu*vpfe2 +ivpfeu*vpfe3 +ivpfeu*vpfe6 +ivpmf1ms1*vpmf1 +ivpmj12*vpmj1 +ivpmj12*vpmj2 +ivpmf1ms1*vpms1 +ivpms2*vpms2 +ivpms6*vpms6 +ivprb_*vprb_
                   +ivprc3*vprc3 +ivpre1*vpre1 +0*vpre2 +ivpre3*vpre3 +0*vprp1 +ivprs1*vprs1 +ivprs2*vprs2 +ivprs3*vprs3 +0*vprt1)

               + (invpfeu*vpfe2 +invpfeu*vpfe3 +invpfeu*vpfe6 +invpmf1ms1*vpmf1 +invpmj12*vpmj1 +invpmj12*vpmj2 +invpmf1ms1*vpms1 +invpms2*vpms2 +invpms6*vpms6 +0*vprb_
                   +invprc3*vprc3 +invpre1*vpre1 +0*vpre2 +invpre3*vpre3 +invprp1*vprp1 +0*vprs1 +0*vprs2 +invprs3*vprs3 +0*vprt1)*log(nrt+1))
             ),

             nsab2_p = isab*(exp(
               + jdt*dt
               + (jsd1*sd1 + jsd2e*sd2e + jsd2o*sd2o + jsd3e*sd3e + jsd3o*sd3o + jsd4e*sd4e + jsd4o*sd4o + jsd5e*sd5e + jsd5o*sd5o + jsd6e*sd6e + jsd6o*sd6o)
               + (jnsd1*sd1 + jnsd2e*sd2e + jnsd2o*sd2o + jnsd3e*sd3e + jnsd3o*sd3o + jnsd4e*sd4e + jnsd4o*sd4o + jnsd5e*sd5e + jnsd5o*sd5o + jnsd6e*sd6e + jnsd6o*sd6o)*log(nsab+1)

               + (0*sd1 + jagesd2e3o*sd2e + 0*sd2o + 0*sd3e + jagesd2e3o*sd3o + 0*sd4e + jagesd4o*sd4o + 0*sd5e + 0*sd5o + 0*sd6e + 0*sd6o)*log(nsab+1)*(1/age)
               + (0*sd1 + 0*sd2e + 0*sd2o + jage2sd2e3e*sd3e + 0*sd3o + jage2sd4e*sd4e + 0*sd4o + jage2sd5e*sd5e + 0*sd5o + jage2sd6e*sd6e + 0*sd6o)*log(nsab+1)*age

               + (0*sd1 + jhdsd2e*sd2e + 0*sd2o + jhdsd3e*sd3e + jhdsd3o*sd3o + jhdsd4e*sd4e + jhdsd4o*sd4o + jhdsd5e*sd5e + jhdsd5o*sd5o + 0*sd6e + jhdsd6o*sd6o)*hd
               + (0*sd1 + 0*sd2e + jtbesd2o*sd2o + jtbesd3e*sd3e + jtbesd3o*sd3o + jtbesd4e*sd4e + jtbesd4o*sd4o + jtbesd5e*sd5e + jtbesd5o*sd5o + jtbesd6e*sd6e + 0*sd6o)*tbe*log(nsab+1)
               + jpert*pert*log(nsab+1)
               + (0*sd1 + jn2sd2e*sd2e + 0*sd2o + jn2sd3e*sd3e + 0*sd3o + jn2sd4e*sd4e + jn2sd4o*sd4o + jn2sd5e*sd5e + 0*sd5o + jn2sd6e*sd6e + jn2sd6o*sd6o)*log(nsab+1)**2


               + (jvpfe2*vpfe2 +jvpfe3*vpfe3 +jvpfe6*vpfe6 +0*vpmf1 +jvpmj1*vpmj1 +jvpmj2*vpmj2 +jvpms1*vpms1 +0*vpms2 +0*vpms6 +jvprb_*vprb_
                   +jvprc3*vprc3 +jvpre1*vpre1 +jvpre2*vpre2 +jvpre3*vpre3 +jvprp1*vprp1 +jvprs1*vprs1 +jvprs2*vprs2 +jvprs3*vprs3 +jvprt1*vprt1)

               + (0*vpfe2 +jnvpfe3*vpfe3 +jnvpfe6*vpfe6 +0*vpmf1 +jnvpmj1*vpmj1 +jnvpmj2*vpmj2 +0*vpms1 +0*vpms2 +0*vpms6 +0*vprb_
                   +0*vprc3 +0*vpre1 +jnvpre2*vpre2 +0*vpre3 +jnvprp1*vprp1 +jnvprs1*vprs1 +jnvprs2*vprs2 +0*vprs3 +jnvprt1*vprt1)*log(nsab+1))
               ),

             # prédiction de la surface terrière par groupe d'espèces
             stfi2_p = ifi*(exp(
               + adt*dt
               + (asd1*sd1 + asd2e*sd2e + asd2o*sd2o + asd3e*sd3e + asd3o*sd3o + asd4e*sd4e + asd4o*sd4o + asd5e*sd5e + asd5o*sd5o + asd6e*sd6e + asd6o*sd6o)

               + (0*vpfe2 +avpfe3*vpfe3 +avpfe6*vpfe6 +avpmf1*vpmf1 +0*vpmj1 +0*vpmj2 +avpms1*vpms1 +0*vpms2 +avpms6*vpms6 +0*vprb_
                   +avprc3re1re3*vprc3 +avprc3re1re3*vpre1 +avpre2*vpre2 +avprc3re1re3*vpre3 +0*vprp1 +avprs1*vprs1 +avprs2*vprs2 +0*vprs3 +0*vprt1)
               + (0*vpfe2 +0*vpfe3 +0*vpfe6 +0*vpmf1 +0*vpmj1 +0*vpmj2 +0*vpms1 +0*vpms2 +0*vpms6 +0*vprb_
                   +0*vprc3 +0*vpre1 +0*vpre2 +0*vpre3 +0*vprp1 +0*vprs1 +0*vprs2 +astvprs3*vprs3 +astvprt1*vprt1)*log(stfi+0.1)


               + (astsd1*sd1 + astsd2e*sd2e + astsd2o*sd2o + astsd3e*sd3e + astsd3o*sd3o + astsd4e*sd4e + astsd4o*sd4o + astsd5e*sd5e + astsd5o*sd5o + astsd6e*sd6e + astsd6o*sd6o)*log(stfi+0.1)
               + (aagesd1*sd1 + aagesd2e*sd2e + aagesd2o*sd2o + aagesd3e*sd3e + aagesd3o*sd3o + aagesd4e*sd4e + aagesd4o*sd4o + aagesd5e*sd5e + aagesd5o*sd5o + aagesd6e*sd6e + aagesd6o*sd6o)*(1/age)
               + atbe*tbe*log(stfi+0.1)
               + aperturb*pert*log(stfi+0.1)
               + aprop*stfi/stcom
               + anfi*log(nfi+1)
               + aapresp*apresp*log(stfi+0.1))
             ),
             stft2_p = ift*(exp(
               + bdt*dt
               + (bsd1*sd1 + bsd2e*sd2e + bsd2o*sd2o + bsd3e*sd3e + bsd3o*sd3o + bsd4e*sd4e + bsd4o*sd4o + bsd5e6e*sd5e + bsd5o6o*sd5o + bsd5e6e*sd6e + bsd5o6o*sd6o)
               + bst*log(stft+0.1)
               + bage*age
               + bstage*log(stft+0.1)*age
               + bprop*stft/stcom
               + bnft*log(nft+1)
               + bapresc*apresc
               + btbe*tbe*log(stft+0.1)
               + bpert*pert*log(stft+0.1)

               + (bvpfe2*vpfe2 +bvpfe3*vpfe3 +0*vpfe6 +0*vpmf1 +0*vpmj1 +0*vpmj2 +bvpms1*vpms1 +0*vpms2 +0*vpms6 +0*vprb_
                   +bvprc3*vprc3 +0*vpre1 +0*vpre2 +0*vpre3 +0*vprp1 +0*vprs1 +0*vprs2 +0*vprs3 +0*vprt1)

               + (bvpstfe2*vpfe2 +bvpstfe3*vpfe3 +0*vpfe6 +0*vpmf1 +0*vpmj1 +0*vpmj2 +0*vpms1 +bvpstms2*vpms2 +0*vpms6 +bvpstrbre_*vprb_
                   +0*vprc3 +bvpstrbre_*vpre1 +bvpstrbre_*vpre2 +bvpstrbre_*vpre3 +bvpstrp1*vprp1 +0*vprs1 +bvpstrs2rs3*vprs2 +bvpstrs2rs3*vprs3 +0*vprt1)*log(stft+0.1)

               + bapresp*apresp)
             ),
             stri2_p = iri*(exp(
               + cdt*dt
               + (csd123*sd1 + csd123*sd2e + csd123*sd2o + csd123*sd3e + csd123*sd3o + csd4e5e*sd4e + csd4o*sd4o + csd4e5e*sd5e + csd5o*sd5o + csd6e6o*sd6e + csd6e6o*sd6o)
               + (cstsd123*sd1 + cstsd123*sd2e + cstsd123*sd2o + cstsd123*sd3e + cstsd123*sd3o + cstsd4e5e*sd4e + cstsd4o*sd4o + cstsd4e5e*sd5e + cstsd5o*sd5o + cstsd6e6o*sd6e + cstsd6e6o*sd6o)*log(stri+0.1)
               + (cagesd123*sd1 + cagesd123*sd2e + cagesd123*sd2o + cagesd123*sd3e + cagesd123*sd3o + cagesd456*sd4e + cagesd456*sd4o + cagesd456*sd5e + cagesd456*sd5o + cagesd456*sd6e + cagesd456*sd6o)*(1/age)
               + chd*hd
               + cpropri*stri/stcom
               + cpert*pert*log(stri+0.1)

               + (0*vpfe2 +0*vpfe3 +0*vpfe6 +0*vpmf1 +0*vpmj1 +0*vpmj2 +0*vpms1 +0*vpms2 +0*vpms6 +0*vprb_
                   +0*vprc3 +cvpre1*vpre1 +0*vpre2 +cvpre3*vpre3 +cvprp1*vprp1 +cvprs2*vprs1 +cvprs2*vprs2 +cvpre3*vprs3 +0*vprt1)

               + cptot*ptot)
             ),
             strt2_p = irt*(exp(
               + ddt*dt

               + (dsd12o*sd1 + dsd2e3e4e*sd2e + dsd12o*sd2o + dsd2e3e4e*sd3e + dsd3o*sd3o + dsd2e3e4e*sd4e + dsd4o*sd4o + dsd5e*sd5e + dsd5o*sd5o + dsd6e*sd6e + dsd6o*sd6o)
               + (dstsd12o*sd1 + dstsd2e3e4e*sd2e + dstsd12o*sd2o + dstsd2e3e4e*sd3e + dstsd3o*sd3o + dstsd2e3e4e*sd4e + dstsd4o*sd4o + dstsd5e*sd5e + dstsd5o*sd5o + dstsd6e*sd6e + dstsd6o*sd6o)*log(strt+0.1)

               + (dagesd1234e*sd1 + dagesd1234e*sd2e + dagesd1234e*sd2o + dagesd1234e*sd3e + dagesd1234e*sd3o + dagesd1234e*sd4e + dagesd4o*sd4o + dagesd5e*sd5e + dagesd5o*sd5o + dagesd6e*sd6e + dagesd6o*sd6o)*(1/age)

               + (dhdsd12o*sd1 + dhdsd2e3e4e*sd2e + dhdsd12o*sd2o + dhdsd2e3e4e*sd3e + dhdsd3o*sd3o + dhdsd2e3e4e*sd4e + dhdsd4o*sd4o + dhdsd5e*sd5e + dhdsd5o*sd5o + dhdsd6e*sd6e + dhdsd6o*sd6o)*hd

               + dtbe*tbe*log(strt+0.1)
               + (0*sd1 + 0*sd2e + 0*sd2o + 0*sd3e + dpertsd3o*sd3o + dpertsd4e*sd4e + dpertsd4o*sd4o + dpertsd5e*sd5e + dpertsd5o*sd5o + dpertsd6e*sd6e + dpertsd6o*sd6o)*pert*log(strt+0.1)

               + dapresc*apresc*log(strt+0.1)

               + dnrt*log(nrt+1)

               + (dpropsd12o*sd1 + dpropsd2e3e4e*sd2e + dpropsd12o*sd2o + dpropsd2e3e4e*sd3e + dpropsd3o*sd3o + dpropsd2e3e4e*sd4e + dpropsd4o5o*sd4o + dpropsd5e*sd5e + dpropsd4o5o*sd5o + dpropsd6e*sd6e + 0*sd6o)*strt/stcom


               + (0*vpfe2 +dvpfe3*vpfe3 +dvpfe6*vpfe6 +dvpmf1*vpmf1 +dvpmj1*vpmj1 +dvpmj2*vpmj2 +dvpms1*vpms1 +dvpms2*vpms2 +dvpms6*vpms6 +dvprb_*vprb_
                   +dvprc3*vprc3 +dvpre1*vpre1 +0*vpre2 +dvpre3*vpre3 +0*vprp1 +dvprs1*vprs1 +0*vprs2 +dvprs3*vprs3 +dvprt1*vprt1)

               + (0*vpfe2 +dstvpfe3*vpfe3 +dstvpfe6*vpfe6 +dstvpmf1*vpmf1 +dstvpmj1*vpmj1 +dstvpmj2*vpmj2 +dstvpms1*vpms1 +0*vpms2 +dstvpms6*vpms6 +dstvprb_*vprb_
                   +0*vprc3 +0*vpre1 +0*vpre2 +dstvpre3*vpre3 +dstvprp1*vprp1 +0*vprs1 +0*vprs2 +dstvprs3*vprs3 +dstvprt1*vprt1)*log(strt+0.1))
             ),
             stsab2_p = isab*(exp(
               + edt*dt

               + (esd1*sd1 + esd2e*sd2e + esd2o*sd2o + esd3e*sd3e + esd3o*sd3o + esd4e*sd4e + esd4o*sd4o + esd5e*sd5e + esd5o*sd5o + esd6e*sd6e + esd6o*sd6o)
               + (estsd1*sd1 + estsd2e*sd2e + estsd2o*sd2o + estsd3e*sd3e + estsd3o*sd3o + estsd4e*sd4e + estsd4o*sd4o + estsd5e*sd5e + estsd5o*sd5o + estsd6e*sd6e + estsd6o*sd6o)*log(stsab+0.1)

               + eage*log(stsab+0.1)*(1/age)

               + ehd*hd
               + etbe*tbe*log(stsab+0.1)
               + ensab*log(nsab+1)

               + (evpfe26*vpfe2 +evpfe3*vpfe3 +evpfe26*vpfe6 +evpmf1*vpmf1 +0*vpmj1 +0*vpmj2 +evpms1*vpms1 +0*vpms2 +0*vpms6 +evprb*vprb_
                   +evprc3rp1rt1*vprc3 +evpre123*vpre1 +evpre123*vpre2 +evpre123*vpre3 +evprc3rp1rt1*vprp1 +evprs1*vprs1 +evprs2*vprs2 +evprs3*vprs3 +evprc3rp1rt1*vprt1)

               + (0*vpfe2 +estvpfe3*vpfe3 +0*vpfe6 +0*vpmf1 +0*vpmj1 +0*vpmj2 +0*vpms1 +0*vpms2 +0*vpms6 +0*vprb_
                   +estvprc3rp1rt1*vprc3 +0*vpre1 +0*vpre2 +0*vpre3 +estvprc3rp1rt1*vprp1 +0*vprs1 +0*vprs2 +0*vprs3 +estvprc3rp1rt1*vprt1)*log(stsab+0.1))
               ),

             # pour les modèles de V, il faut les valeurs prédites du temps2
             hd2=hd2_p,
             stfi2=ifelse(stfi2_p==0, 0.01, stfi2_p),
             stft2=ifelse(stft2_p==0, 0.01, stft2_p),
             stri2=ifelse(stri2_p==0, 0.01, stri2_p),
             strt2=ifelse(strt2_p==0, 0.01, strt2_p),
             stsab2=ifelse(stsab2_p==0, 0.01, stsab2_p),
             nfi2=ifelse(nfi2_p==0, 0.01, nfi2_p),
             nft2=ifelse(nft2_p==0, 0.01, nft2_p),
             nri2=ifelse(nri2_p==0, 0.01, nri2_p),
             nrt2=ifelse(nrt2_p==0, 0.01, nrt2_p),
             nsab2=ifelse(nsab2_p==0, 0.01, nsab2_p),

             vfi2_p = ifelse(vfi>0,
                ifi*(exp(
               + kdt*dt
               + (ksd1*sd1+ksd2e*sd2e+ksd2o*sd2o+ksd3e*sd3e+ksd3o*sd3o+ksd4e*sd4e+ksd4o*sd4o+ksd5e*sd5e+ksd5o*sd5o+ksd6e*sd6e+ksd6o*sd6o)
               + kvfi*log(vfi+0.1)
               + (kstsd1*sd1+kstsd2e*sd2e+kstsd2o*sd2o+kstsd3e*sd3e+kstsd3o*sd3o+kstsd4e*sd4e+kstsd4o*sd4o+kstsd5e*sd5e+kstsd5o*sd5o+kstsd6e*sd6e+kstsd6o*sd6o)*log(stfi2)
               + khd*hd2
               + kage*(1/(age+dt))
               + knfi*log(nfi2)
               + (0*vpfe2 +0*vpfe3 +0*vpfe6 +0*vpmf1 +0*vpmj1 +0*vpmj2 +0*vpms1 +0*vpms2 +0*vpms6 +0*vprb_
                   +0*vprc3 +0*vpre1 +0*vpre2 +0*vpre3 +0*vprp1 +0*vprs1 +0*vprs2 +kvprs3*vprs3 +0*vprt1)

               + (0*vpfe2 +0*vpfe3 +kstvpfe6*vpfe6 +kstvpmf1*vpmf1 +kstvpmj1*vpmj1 +0*vpmj2 +0*vpms1 +0*vpms2 +kstvpms6*vpms6 +0*vprb_
                   +kstvprc3re1re3*vprc3 +kstvprc3re1re3*vpre1 +kstvpre2*vpre2 +kstvprc3re1re3*vpre3 +kstvprp1*vprp1 +kstvprs1*vprs1 +kstvprs2*vprs2 +0*vprs3 +0*vprt1)*log(stfi2))),
               0),
             vft2_p = ifelse(vft>0,
                ift*(exp(
               + ldt*dt
               + (lsd1*sd1+lsd2e*sd2e+lsd2o*sd2o+lsd3e*sd3e+lsd3o*sd3o+lsd4e*sd4e+lsd4o*sd4o+lsd5e6e*sd5e+lsd5o6o*sd5o+lsd5e6e*sd6e+lsd5o6o*sd6o)
               + lvft*log(vft+0.1)
               + (lstsd1*sd1+lstsd2e*sd2e+lstsd2o*sd2o+lstsd3e*sd3e+lstsd3o*sd3o+lstsd4e*sd4e+lstsd4o*sd4o+lstsd5e6e*sd5e+lstsd5o6o*sd5o+lstsd5e6e*sd6e+lstsd5o6o*sd6o)*log(stft2)
               + lhd*hd2
               + lage*(1/(age+dt))
               + lnft*log(nft2)

               + (lvpfe2*vpfe2 +0*vpfe3 +lvpfe6*vpfe6 +0*vpmf1 +lvpmj1*vpmj1 +lvpmj2*vpmj2 +lvpms1*vpms1 +lvpms2*vpms2 +lvpms6*vpms6 +lvpre*vprb_
                   +lvprc3*vprc3 +lvpre*vpre1 +lvpre*vpre2 +lvpre*vpre3 +lvprp1*vprp1 +lvprs1*vprs1 +lvprs2*vprs2 +lvpre*vprs3 +lvprt1*vprt1)*log(stft2))),
               0),
             vri2_p = ifelse(vri>0,
                iri*(exp(
               + mdt*dt
               + (msd123*sd1+msd123*sd2e+msd123*sd2o+msd123*sd3e+msd123*sd3o+msd4e5e*sd4e+msd4o*sd4o+msd4e5e*sd5e+msd5o*sd5o+msd6e6o*sd6e+msd6e6o*sd6o)
               + mvri*log(vri+0.1)
               + (mstsd123*sd1+mstsd123*sd2e+mstsd123*sd2o+mstsd123*sd3e+mstsd123*sd3o+mstsd4e5e*sd4e+mstsd4o*sd4o+mstsd4e5e*sd5e+mstsd5o*sd5o+mstsd6e6o*sd6e+mstsd6e6o*sd6o)*log(stri2)
               + mhd*hd2
               + mage*(age+dt)
               + mnri*log(nri2)
               + (0*vpfe2 +0*vpfe3 +0*vpfe6 +0*vpmf1 +0*vpmj1 +0*vpmj2 +0*vpms1 +mvpms2*vpms2 +mvpms2*vpms6 +0*vprb_
                   +0*vprc3 +mvpre1*vpre1 +0*vpre2 +mvpre3*vpre3 +0*vprp1 +0*vprs1 +0*vprs2 +mvpre3*vprs3 +0*vprt1))),
               0),
             vrt2_p = ifelse(vrt>0,
                irt*(exp(
               + ndt*dt
               + (nsd1*sd1+nsd2e*sd2e+nsd2o*sd2o+nsd3e*sd3e+nsd3o*sd3o+nsd4e*sd4e+nsd4o*sd4o+nsd5e*sd5e+nsd5o*sd5o+nsd6e*sd6e+nsd6o*sd6o)
               + nvrt*log(vrt+0.1)
               + (nstsd1*sd1+nstsd2e*sd2e+nstsd2o*sd2o+nstsd3e*sd3e+nstsd3o*sd3o+nstsd4e*sd4e+nstsd4o*sd4o+nstsd5e*sd5e+nstsd5o*sd5o+nstsd6e*sd6e+nstsd6o*sd6o)*log(strt2)
               + nhd*hd2
               + nage*(age+dt)
               + nnrt*log(nrt2)

               + (0*vpfe2 +0*vpfe3 +0*vpfe6 +0*vpmf1 +nvpmj1*vpmj1 +0*vpmj2 +0*vpms1 +0*vpms2 +0*vpms6 +nvprb_*vprb_
                   +nvprc3*vprc3 +0*vpre1 +nvprbre23*vpre2 +nvprbre23*vpre3 +nvprp1*vprp1 +0*vprs1 +0*vprs2 +0*vprs3 +0*vprt1)

               + (nstvpfe2*vpfe2 +nstvpfe3mf1mj12*vpfe3 +nstvpfe6*vpfe6 +nstvpfe3mf1mj12*vpmf1 +nstvpfe3mf1mj12*vpmj1 +nstvpfe3mf1mj12*vpmj2 +nstvpms1*vpms1 +nstvpms26*vpms2 +nstvpms26*vpms6 +nstvprbre23*vprb_
                   +nstvprc3*vprc3 +nstvpre1*vpre1 +nstvprbre23*vpre2 +nstvprbre23*vpre3 +0*vprp1 +nstvprs1*vprs1 +0*vprs2 +0*vprs3 +nstvprt1*vprt1)*log(strt2))),
               0),
             vsab2_p = ifelse(vsab>0,
                isab*(exp(
               + odt*dt
               + (osd1*sd1+osd2e*sd2e+osd2o*sd2o+osd3e*sd3e+osd3o*sd3o+osd4e*sd4e+osd4o*sd4o+osd5e*sd5e+osd5o*sd5o+osd6e*sd6e+osd6o*sd6o)
               + ovsab*log(vsab+0.1)
               + (ostsd1*sd1+ostsd2e*sd2e+ostsd2o*sd2o+ostsd3e*sd3e+ostsd3o*sd3o+ostsd4e*sd4e+ostsd4o*sd4o+ostsd5e*sd5e+ostsd5o*sd5o+ostsd6e*sd6e+ostsd6o*sd6o)*log(stsab2)
               + ohd*hd2
               + oage*(age+dt)
               + onsab*log(nsab2)

               + (0*vpfe2 +0*vpfe3 +0*vpfe6 +0*vpmf1 +0*vpmj1 +0*vpmj2 +ovpms1*vpms1 +ovpms2*vpms2 +0*vpms6 +0*vprb_
                   +0*vprc3 +0*vpre1 +0*vpre2 +0*vpre3 +0*vprp1 +0*vprs1 +0*vprs2 +0*vprs3 +ovprt1*vprt1)

               + (0*vpfe2 + 0*vpfe3 + 0*vpfe6 +0*vpmf1 +0*vpmj1 +0*vpmj2 +0*vpms1 +0*vpms2 +0*vpms6 +ostvprb_*vprb_
                   +0*vprc3 +0*vpre1 +0*vpre2 +0*vpre3 +0*vprp1 +ostvprs1*vprs1 +0*vprs2 +0*vprs3 +ostvprt1*vprt1)*log(stsab2))),
               0),

             vfi2_p = ifelse(stfi2_p==0, 0, vfi2_p),
             vft2_p = ifelse(stft2_p==0, 0, vft2_p),
             vri2_p = ifelse(stri2_p==0, 0, vri2_p),
             vrt2_p = ifelse(strt2_p==0, 0, vrt2_p),
             vsab2_p = ifelse(stsab2_p==0, 0, vsab2_p),

             ncom2_p = iri*nri2_p + irt*nrt2_p + isab*nsab2_p + ifi*nfi2_p + ift*nft2_p,
             stcom2_p = iri*stri2_p + irt*strt2_p + isab*stsab2_p + ifi*stfi2_p + ift*stft2_p,
             vcom2_p = ifi*vfi2_p + ift*vft2_p + iri*vri2_p + irt*vrt2_p + isab*vsab2_p,

             age_p=age+dt
      ) %>%
        dplyr::select(-is, -hd, -hd2, -age, -ncom, -stcom, -vcom,
             -nfi, -nft, -nri, -nrt, -nsab,
             -nfi2, -nft2, -nri2, -nrt2, -nsab2,
             -stfi, -stft, -stri, -strt, -stsab,
             -stfi2, -stft2, -stri2, -strt2, -stsab2,
             -vfi, -vft, -vri, -vrt, -vsab) %>%
      rename(
        age=age_p,
        hd=hd2_p,
        is=is2_p,
        ncom=ncom2_p,
        stcom=stcom2_p,
        vcom=vcom2_p,
        nfi=nfi2_p, nft=nft2_p, nri=nri2_p, nrt=nrt2_p, nsab=nsab2_p,
        stfi=stfi2_p, stft=stft2_p, stri=stri2_p, strt=strt2_p, stsab=stsab2_p,
        vfi=vfi2_p, vft=vft2_p, vri=vri2_p, vrt=vrt2_p, vsab=vsab2_p
        )

      # Ajout du point de départ de la simulation
      outputTot <- bind_rows(outputTot, Plac2)

    }  # fin de la boucle d'un pas de simulation


  # Ajout du point de départ de la simulation aux simulations et vérification si simulation hors limite
  outputFinal <- bind_rows(PlacOri, outputTot) %>%
    arrange(placette, annee) %>%
    mutate(nfi = ifelse(nfi>200, NA, nfi),
           nft = ifelse(nft>200, NA, nft),
           nri = ifelse(nri>200, NA, nri),
           nrt = ifelse(nrt>200, NA, nrt),
           nsab = ifelse(nsab>200, NA, nsab),

           stfi = ifelse(stfi>100, NA, stfi),
           stft = ifelse(stft>100, NA, stft),
           stri = ifelse(stri>100, NA, stri),
           strt = ifelse(strt>100, NA, strt),
           stsab = ifelse(stsab>100, NA, stsab),

           Warning = ifelse(nfi>200 | nft>200 | nri>200 | nrt>200 | nsab>200 | stfi>100 | stft>100 | stri>100 | strt>100 | stsab>100, 'Simulation hors limite', '')
           ) %>%
    mutate(nfi = nfi*25,
           nft = nft*25,
           nri = nri*25,
           nrt = nrt*25,
           nsab = nsab*25,
           ncom = ncom*25,
           dqfi = ifelse(nfi>0, sqrt((stfi*40000)/(nfi*pi)), NA),
           dqft = ifelse(nft>0, sqrt((stft*40000)/(nft*pi)), NA),
           dqri = ifelse(nri>0, sqrt((stri*40000)/(nri*pi)), NA),
           dqrt = ifelse(nrt>0, sqrt((strt*40000)/(nrt*pi)), NA),
           dqsab = ifelse(nsab>0, sqrt((stsab*40000)/(nsab*pi)), NA),
           dqcom = ifelse(ncom>0, sqrt((stcom*40000)/(ncom*pi)), NA),
           dqfi = ifelse(dqfi<9, NA, dqfi),
           dqft = ifelse(dqft<9, NA, dqft),
           dqri = ifelse(dqri<9, NA, dqri),
           dqrt = ifelse(dqrt<9, NA, dqrt),
           dqsab = ifelse(dqsab<9, NA, dqsab),
           dqcom = ifelse(dqcom<9, NA, dqcom)
           ) %>%
    rename(ntot=ncom, sttot=stcom, vtot=vcom, dqtot=dqcom, indice_tbe=tbe, presence_perturb = pert) %>%
    dplyr::select(placette, annee, age, is, hd, ntot, sttot, vtot, dqtot,
           nfi, nft, nri, nrt, nsab,
           stfi, stft, stri, strt, stsab,
           vfi, vft, vri, vrt, vsab,
           dqfi, dqft, dqri, dqrt, dqsab,
           indice_tbe, presence_perturb,
           Warning)
  # ajouter les info placette
  outputFinal2  <- data %>%
    dplyr::select(placette, sdom_bio, ptot, tmoy, type_eco) %>%
    inner_join(outputFinal, by='placette')


  return(outputFinal2)

}
