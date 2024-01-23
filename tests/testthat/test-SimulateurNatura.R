test_that("La fonction principale SimulNatura() donne les mêmes résultats que Capsis sur 40 ans pour toutes les variables simulées, sans TBE ni PERTURB, avec un fichier d'entrée à l'échelle de l'arbre", {

  data_arbre <- readRDS(test_path("fixtures", "fic_arbre_ex.rds"))
  data_etude <- readRDS(test_path("fixtures", "fic_etude_ex.rds"))
  resul_attendu <- readRDS(test_path("fixtures", "fic_capsis_50ans.rds")) # 10 obs 32 var.


  data_simul <- SimulNatura(file_arbre=data_arbre, file_etude=data_etude, horizon=4) # 10 obs 37 var.
  data_simul2 <- data_simul %>% dplyr::select(-sdom_bio, -ptot, -tmoy, -altitude, -type_eco) # 10 obs 32 var.


  # comparer la step 0
  # resul_attendu0 <- resul_attendu %>% filter(annee==0)
  # data_simul0 <- data_simul2 %>% filter(annee==0)
  #
  # expect_equal(data_simul0$sttot, resul_attendu0$sttot) # ok
  # expect_equal(data_simul0$vtot, resul_attendu0$vtot) # ok
  # expect_equal(data_simul0$ntot, resul_attendu0$ntot) # ok
  # expect_equal(round(data_simul2$hd,1), round(resul_attendu$hd,1)) # ok
  # expect_equal(round(data_simul2$is,1), round(resul_attendu$is,1)) # ok


  # vérifier si on utilise les mêmes paramètres dans capsis et r
  # parmsfi_capsis <- read_delim("E:\\MrnMicro\\Applic\\_Modeles-DRF\\Capsis-3ModelesDRF-20230418\\src\\natura2014\\parameters\\ParamCroissFi.csv", delim=';')
  # parmsfi_capsis_tr <- parmsfi_capsis %>% dplyr::select(parms, Estimate) %>% pivot_wider(names_from = parms, values_from = Estimate)
  # parmsfi_r <- as.matrix(ParmsFi)
  # parmsfi_capsis_tr2 <- as.matrix(as.data.frame(parmsfi_capsis_tr))
  # expect_equal(round(parmsfi_r,6), round(parmsfi_capsis_tr2,6)) # ok, pareils
  #
  # parmsft_capsis <- read_delim("E:\\MrnMicro\\Applic\\_Modeles-DRF\\Capsis-3ModelesDRF-20230418\\src\\natura2014\\parameters\\ParamCroissFt.csv", delim=';')
  # parmsft_capsis_tr <- parmsft_capsis %>% dplyr::select(parms, Estimate) %>% pivot_wider(names_from = parms, values_from = Estimate)
  # parmsft_r <- as.matrix(ParmsFt)
  # parmsft_capsis_tr2 <- as.matrix(as.data.frame(parmsft_capsis_tr))
  # expect_equal(round(parmsft_r,6), round(parmsft_capsis_tr2,6)) # ok, pareils
  #
  # parmsrt_capsis <- read_delim("E:\\MrnMicro\\Applic\\_Modeles-DRF\\Capsis-3ModelesDRF-20230418\\src\\natura2014\\parameters\\ParamCroissRt.csv", delim=';')
  # parmsrt_capsis_tr <- parmsrt_capsis %>% dplyr::select(parms, Estimate) %>% pivot_wider(names_from = parms, values_from = Estimate)
  # parmsrt_r <- as.matrix(ParmsRt)
  # parmsrt_capsis_tr2 <- as.matrix(as.data.frame(parmsrt_capsis_tr))
  # expect_equal(round(parmsrt_r,6), round(parmsrt_capsis_tr2,6)) # ok, pareils
  #
  # parmsri_capsis <- read_delim("E:\\MrnMicro\\Applic\\_Modeles-DRF\\Capsis-3ModelesDRF-20230418\\src\\natura2014\\parameters\\ParamCroissRi.csv", delim=';')
  # parmsri_capsis_tr <- parmsri_capsis %>% dplyr::select(parms, Estimate) %>% pivot_wider(names_from = parms, values_from = Estimate)
  # parmsri_r <- as.matrix(ParmsRi)
  # parmsri_capsis_tr2 <- as.matrix(as.data.frame(parmsri_capsis_tr))
  # expect_equal(round(parmsri_r,6), round(parmsri_capsis_tr2,6)) # ok, pareils
  #
  # parmssab_capsis <- read_delim("E:\\MrnMicro\\Applic\\_Modeles-DRF\\Capsis-3ModelesDRF-20230418\\src\\natura2014\\parameters\\ParamCroissSab.csv", delim=';')
  # parmssab_capsis_tr <- parmssab_capsis %>% dplyr::select(parms, Estimate) %>% pivot_wider(names_from = parms, values_from = Estimate)
  # parmssab_r <- as.matrix(ParmsSab)
  # parmssab_capsis_tr2 <- as.matrix(as.data.frame(parmssab_capsis_tr))
  # expect_equal(round(parmssab_r,6), round(parmssab_capsis_tr2,6)) # ok, pareils
  # LES PARAMETRES SONT ÉGAUX

   result_simule <- as.matrix(data_simul2 %>%   ungroup() %>%
    dplyr::select(age, hd, is,
    ntot, vtot, sttot, dqtot,
    nfi, nft, nri, nrt, nsab,
    stfi, stft, stri, strt, stsab,
    vfi, vft, vri, vrt, vsab,
    dqfi, dqft, dqri, dqrt, dqsab) %>%
      mutate(hd = round(hd,1),
             is = round(is,2),
             ntot = round(ntot,0),
             nfi = round(nfi,0),
             nft = round(nft,0),
             nri = round(nri,0),
             nrt = round(nrt,0),
             nsab = round(nsab,0),
             sttot = round(sttot,1),
             stfi = round(stfi,1),
             stft = round(stft,1),
             stri = round(stri,1),
             strt = round(strt,1),
             stsab = round(stsab,1),
             vtot = round(ntot,0),
             vfi = round(vfi,0),
             vft = round(vft,0),
             vri = round(vri,0),
             vrt = round(vrt,0),
             vsab = round(vsab,0),
             dqtot = round(dqtot,01),
             dqfi = round(dqfi,1),
             dqft = round(dqft,1),
             dqri = round(dqri,1),
             dqrt = round(dqrt,1),
             dqsab = round(dqsab,1))
    )

  resul_attendu <- as.matrix(resul_attendu %>% ungroup() %>%
    dplyr::select(age, hd, is,
                  ntot, vtot, sttot, dqtot,
                  nfi, nft, nri, nrt, nsab,
                  stfi, stft, stri, strt, stsab,
                  vfi, vft, vri, vrt, vsab,
                  dqfi, dqft, dqri, dqrt, dqsab) %>%
      mutate(hd = round(hd,1),
             is = round(is,2),
             ntot = round(ntot,0),
             nfi = round(nfi,0),
             nft = round(nft,0),
             nri = round(nri,0),
             nrt = round(nrt,0),
             nsab = round(nsab,0),
             sttot = round(sttot,1),
             stfi = round(stfi,1),
             stft = round(stft,1),
             stri = round(stri,1),
             strt = round(strt,1),
             stsab = round(stsab,1),
             vtot = round(ntot,0),
             vfi = round(vfi,0),
             vft = round(vft,0),
             vri = round(vri,0),
             vrt = round(vrt,0),
             vsab = round(vsab,0),
             dqtot = round(dqtot,01),
             dqfi = round(dqfi,1),
             dqft = round(dqft,1),
             dqri = round(dqri,1),
             dqrt = round(dqrt,1),
             dqsab = round(dqsab,1))
    )

  expect_equal(result_simule, resul_attendu)

})



test_that("La fonction principale SimulNatura() donne les mêmes résultats que Capsis sur 40 ans pour toutes les variables simulées, sans TBE ni PERTURB, avec un fichier d'entrée à l'échelle de la placette", {

  data_compile <- readRDS(test_path("fixtures", "fic_compil_ex.rds"))
  resul_attendu <- readRDS(test_path("fixtures", "fic_capsis_compile_40ans.rds")) # 10 obs 32 var.


  data_simul <- SimulNatura(file_compile=data_compile, horizon=4) # 10 obs 37 var.
  data_simul2 <- data_simul %>% dplyr::select(-sdom_bio, -ptot, -tmoy, -altitude, -type_eco) # 10 obs 32 var.

  result_simule <- as.matrix(data_simul2 %>%   ungroup() %>%
                               dplyr::select(age, hd, is,
                                             ntot, vtot, sttot, dqtot,
                                             nfi, nft, nri, nrt, nsab,
                                             stfi, stft, stri, strt, stsab,
                                             vfi, vft, vri, vrt, vsab,
                                             dqfi, dqft, dqri, dqrt, dqsab) %>%
                               mutate(hd = round(hd,1),
                                      is = round(is,2),
                                      ntot = round(ntot,0),
                                      nfi = round(nfi,0),
                                      nft = round(nft,0),
                                      nri = round(nri,0),
                                      nrt = round(nrt,0),
                                      nsab = round(nsab,0),
                                      sttot = round(sttot,1),
                                      stfi = round(stfi,1),
                                      stft = round(stft,1),
                                      stri = round(stri,1),
                                      strt = round(strt,1),
                                      stsab = round(stsab,1),
                                      vtot = round(ntot,0),
                                      vfi = round(vfi,0),
                                      vft = round(vft,0),
                                      vri = round(vri,0),
                                      vrt = round(vrt,0),
                                      vsab = round(vsab,0),
                                      dqtot = round(dqtot,01),
                                      dqfi = round(dqfi,1),
                                      dqft = round(dqft,1),
                                      dqri = round(dqri,1),
                                      dqrt = round(dqrt,1),
                                      dqsab = round(dqsab,1))
  )

  resul_attendu <- as.matrix(resul_attendu %>% ungroup() %>%
                               dplyr::select(age, hd, is,
                                             ntot, vtot, sttot, dqtot,
                                             nfi, nft, nri, nrt, nsab,
                                             stfi, stft, stri, strt, stsab,
                                             vfi, vft, vri, vrt, vsab,
                                             dqfi, dqft, dqri, dqrt, dqsab) %>%
                               mutate(hd = round(hd,1),
                                      is = round(is,2),
                                      ntot = round(ntot,0),
                                      nfi = round(nfi,0),
                                      nft = round(nft,0),
                                      nri = round(nri,0),
                                      nrt = round(nrt,0),
                                      nsab = round(nsab,0),
                                      sttot = round(sttot,1),
                                      stfi = round(stfi,1),
                                      stft = round(stft,1),
                                      stri = round(stri,1),
                                      strt = round(strt,1),
                                      stsab = round(stsab,1),
                                      vtot = round(ntot,0),
                                      vfi = round(vfi,0),
                                      vft = round(vft,0),
                                      vri = round(vri,0),
                                      vrt = round(vrt,0),
                                      vsab = round(vsab,0),
                                      dqtot = round(dqtot,01),
                                      dqfi = round(dqfi,1),
                                      dqft = round(dqft,1),
                                      dqri = round(dqri,1),
                                      dqrt = round(dqrt,1),
                                      dqsab = round(dqsab,1))
  )

  expect_equal(result_simule, resul_attendu)

})


test_that("La fonction principale SimulNatura() retourne un message d'erreur si pas les bons noms de colonnes dans le fichier file_arbre", {

  data_arbre <- data.frame(placette=1, essence="ERS", dhp=12)
  data_etude <- readRDS(test_path("fixtures", "fic_etude_ex.rds"))
  expect_error(SimulNatura(file_arbre = data_arbre, file_etude = data_etude, horizon=4) )

})

test_that("La fonction principale SimulNatura() retourne un message d'erreur si pas les bons noms de colonnes dans le fichier file_etude", {

  data_arbre <- readRDS(test_path("fixtures", "fic_arbre_ex.rds"))
  data_etude <- data.frame(placette=1, essence='SAB', etage='D', dhp=10, hauteur=13, age=40)
  expect_error(SimulNatura(file_arbre = data_arbre, file_etude = data_etude, horizon=4) )

})

test_that("La fonction principale SimulNatura() retourne un message d'erreur si pas les bons noms de colonnes dans le fichier file_compile", {

  data_compile <- data.frame(placette=1)
  expect_error(SimulNatura(file_compile = data_compile, horizon=4) )

})

test_that("La fonction principale SimulNatura() retourne un message d'erreur si les arguments de la fonction ne sont pas bien utilisés", {

   expect_error(SimulNatura(file_arbre = data_arbre, horizon=4) )
   expect_error(SimulNatura(file_etude = data_etude, horizon=4) )
   expect_error(SimulNatura(file_compile = data_compile, file_etude = data_etude, horizon=4) )
   expect_error(SimulNatura(horizon=16) )
   expect_error(SimulNatura(file_compile = data_compile, horizon=16) )
   expect_error(SimulNatura(file_compile = data_compile, horizon=2, dec_perturb = 3) )
   expect_error(SimulNatura(file_compile = data_compile, horizon=2, dec_tbe1 = 3) )
   expect_error(SimulNatura(file_compile = data_compile, horizon=2, dec_tbe1 = 2) )
   expect_error(SimulNatura(file_compile = data_compile, horizon=2, tbe1 = 2) )
   expect_error(SimulNatura(file_compile = data_compile, horizon=2, dec_tbe1=2, tbe1 = 2.5) )
   expect_error(SimulNatura(file_compile = data_compile, horizon=2, dec_tbe2=2, tbe2 = 2) )
   expect_error(SimulNatura(file_compile = data_compile, horizon=3, dec_tbe1=1, tbe1=2, dec_tbe2=4, tbe2 = 2) )
   expect_error(SimulNatura(file_compile = data_compile, horizon=3, dec_tbe1=2, tbe1=2, dec_tbe2=1, tbe2 = 2) )
   expect_error(SimulNatura(file_compile = data_compile, horizon=3, dec_tbe1=1, tbe1=2, dec_tbe2=2) )
   expect_error(SimulNatura(file_compile = data_compile, horizon=3, dec_tbe1=1, tbe1=2, tbe2=2) )
   expect_error(SimulNatura(file_compile = data_compile, horizon=3, dec_tbe1=1, tbe1=2, dec_tbe2=2, tbe2=2.5) )
   expect_error(SimulNatura(file_arbre=data_arbre, file_etude=data_etude, horizon=5, ht='T') )
   expect_error(SimulNatura(file_arbre=data_arbre, file_etude=data_etude, horizon=5, vol='T') )

})


# test_that("La fonction principale SimulNatura() retourne un fichier csv avec le résusultats de la simulation", {
#
#   data_compile <- readRDS(test_path("fixtures", "fic_compil_ex.rds"))
#   data_simul <- SimulNatura(file_compile=data_compile, horizon=4, file_export = "tests/testthat/fixtures/resultats.csv") # 10 obs 37 var.
#
#
# })
