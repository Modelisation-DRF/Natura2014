# 4 types de fichier à valider le contenu:
# arbres
# etudes: essence dhpcm, etage, hauteur: ok
# compil: lat, long, prec_gs, temp_gs, an_mes, type_eco, origine, sdom, oc, clay, sand, ph, cec, iqs, hd, is, n, st, v
# valid: ntot, vtot, sttot


test_that("La fonction valid_arbre() fonctionne comme attendu pour le fichier arbres_etudes avec des erreurs", {

  etude = data.frame(placette=c(1,2,3,4,5,6,7),
                     essence=c('ERX','SAB','EPN','BOJ','ERR','ERR', 'SAB'),
                     dhpcm=c(8,201,12,14,16,16, 16),
                     etage=c('C','D','i','O','V','I','I'),
                     hauteur=c(3,4,5,1,41,12,12),
                     age=c(20, 20, 20, 20, 20, 20, 405))

  # il devrait avoir 6 messages d'erreurs, il y a un seul arbre sans erreur

  verif <- valid_arbre(type_fic='etudes', fichier=etude)

  verif_noerreur <- verif[[1]]
  verif_erreur <- verif[[2]]

  expect_equal(nrow(verif_noerreur),1)
  expect_equal(nrow(verif_erreur),6)

  expect_true('message' %in% names(verif_erreur))



})

test_that("La fonction valid_arbre() fonctionne comme attendu pour le fichier arbres_etudes sans erreurs", {

  etude = data.frame(placette=c(1,2,3,4,5),
                     essence=c('SAB','EPN','BOJ','ERR','ERR'),
                     dhpcm=c(8,12,14,16,16),
                     etage=c('C','D','O','V','I'),
                     hauteur=c(3,4,5,39,12),
                     age=c(20,20,20,20,20))

  verif <- valid_arbre(type_fic='etudes', fichier=etude)

  verif_noerreur <- verif[[1]]
  verif_erreur <- verif[[2]]

  nb_noerreur <- nrow(verif_noerreur)
  nb_erreur <- nrow(verif_erreur)


  expect_equal(nb_noerreur,5)
  expect_equal(nb_erreur,0)


})



test_that("La fonction valid_arbre() fonctionne comme attendu pour le fichier arbres avec erreur", {

    arbre = data.frame(placette=seq(1,23),
                       etat=c('10',rep('10',22)),
                       tige_ha=c(25, 0, rep(25,21)),
                       essence=c(rep('SAB',23)),
                       dhpcm=c(rep(12,23)),
                       type_eco=c(rep('MS22',23)),
                       origine=c(rep('BR', 23)),
                       sdom_bio=c(rep('2O',23)),
                       temps=c(rep(50,23)),
                       prec_gs=c(rep(1000,23)),
                       temp_gs=c(rep(5,23)),
                       oc=c(rep(10,23)),
                       clay=c(rep(50,23)),
                       sand=c(rep(50,23)),
                       ph=c(rep(5,23)),
                       cec=c(rep(12,23)),
                       iqs_pot_epn=c(rep(11,23)),
                       iqs_pot_epb=c(rep(11,23)),
                       iqs_pot_pig=c(rep(11,23)),
                       iqs_pot_tho=c(rep(11,23)),
                       iqs_pot_pib=c(rep(11,23)),
                       iqs_pot_sab=c(rep(11,23)),
                       iqs_pot_bop=c(rep(11,23)),
                       iqs_pot_pex=c(rep(11,23)))



  verif <- valid_arbre(type_fic='arbres', fichier=arbre)

  verif_noerreur <- verif[[1]]
  verif_erreur <- verif[[2]]

  expect_equal(nrow(verif_noerreur),22)

  verif_erreur <- paste(verif_erreur$message, collapse = ", ")
  expect_equal(verif_erreur,"Valeurs de nb_tiges non permises (>0)")


})

test_that("La fonction valid_arbre() fonctionne comme attendu pour le fichier arbres sans erreur", {

  arbre = data.frame(placette=seq(1,23),
                     etat=c('10',rep('10',22)),
                     tige_ha=c(25, 25, rep(25,21)),
                     essence=c(rep('SAB',23)),
                     dhpcm=c(rep(12,23)),
                     type_eco=c(rep('MS22',23)),
                     origine=c(rep('BR', 23)),
                     sdom_bio=c(rep('2O',23)),
                     temps=c(rep(50,23)),
                     prec_gs=c(rep(1000,23)),
                     temp_gs=c(rep(5,23)),
                     oc=c(rep(10,23)),
                     clay=c(rep(50,23)),
                     sand=c(rep(50,23)),
                     ph=c(rep(5,23)),
                     cec=c(rep(12,23)),
                     iqs_pot_epn=c(rep(11,23)),
                     iqs_pot_epb=c(rep(11,23)),
                     iqs_pot_pig=c(rep(11,23)),
                     iqs_pot_tho=c(rep(11,23)),
                     iqs_pot_pib=c(rep(11,23)),
                     iqs_pot_sab=c(rep(11,23)),
                     iqs_pot_bop=c(rep(11,23)),
                     iqs_pot_pex=c(rep(11,23)))



  verif <- valid_arbre(type_fic='arbres', fichier=arbre)

  verif_noerreur <- verif[[1]]
  verif_erreur <- verif[[2]]

  expect_equal(nrow(verif_noerreur),23)
  expect_equal(nrow(verif_erreur),0)


})
