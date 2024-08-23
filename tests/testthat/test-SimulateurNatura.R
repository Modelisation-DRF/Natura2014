test_that("La fonction principale SimulNatura() donne les mêmes résultats que Capsis sur 40 ans pour toutes les variables simulées, sans TBE ni PERTURB, avec un fichier d'entrée à l'échelle de l'arbre, avec climat fourni", {

  data_arbre <- readRDS(test_path("fixtures", "fic_arbre_ex.rds"))
  data_etude <- readRDS(test_path("fixtures", "fic_etude_ex.rds"))
  resul_attendu <- readRDS(test_path("fixtures", "fic_capsis_50ans.rds")) # 10 obs 32 var.


  data_simul <- SimulNatura(file_arbre=data_arbre, file_etude=data_etude, horizon=4, climat=F) # 10 obs 37 var.
  data_simul2 <- data_simul %>% dplyr::select(-sdom_bio, -ptot, -tmoy, -type_eco) # 10 obs 32 var.


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



test_that("La fonction principale SimulNatura() donne les mêmes résultats que Capsis sur 40 ans pour toutes les variables simulées, sans TBE ni PERTURB, avec un fichier d'entrée à l'échelle de la placette, avec climat fourni", {

  data_compile <- readRDS(test_path("fixtures", "fic_compil_ex.rds"))
  resul_attendu <- readRDS(test_path("fixtures", "fic_capsis_compile_40ans.rds")) # 10 obs 32 var.


  data_simul <- SimulNatura(file_compile=data_compile, horizon=4, climat=F) # 10 obs 37 var.
  data_simul2 <- data_simul %>% dplyr::select(-sdom_bio, -ptot, -tmoy, -type_eco) # 10 obs 32 var.

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
  expect_error(SimulNatura(file_arbre = data_arbre, file_etude = data_etude, horizon=4, climat=F),"Les variables suivantes sont requises dans le fichier des arbres : type_eco, sdom_bio, dhpcm, tige_ha, etat")

})

test_that("La fonction principale SimulNatura() retourne un message d'erreur si pas les bons noms de colonnes dans le fichier file_etude", {

  data_arbre <- readRDS(test_path("fixtures", "fic_arbre_ex.rds"))
  data_etude <- data.frame(placette=1, essence='SAB', etage='D', dhp=10, hauteur=13, age=40)
  expect_error(SimulNatura(file_arbre = data_arbre, file_etude = data_etude, horizon=4, climat=F),"Nom des variables incorrect dans le fichier des arbres-etudes. Les variables suivantes sont requises : dhpcm" )

})

test_that("La fonction principale SimulNatura() retourne un message d'erreur si pas les bons noms de colonnes dans le fichier file_compile", {

  data_compile <- data.frame(placette=1)
  expect_error(SimulNatura(file_compile = data_compile, horizon=4,climat=F),"Les variables suivantes sont requises dans le fichier d'inventaire compile : type_eco, sdom_bio, nfi, nft, nsab, nri, nrt, stfi, stft, stsab, stri, strt, vfi, vft, vsab, vri, vrt, hd, is, age" )

})

test_that("La fonction principale SimulNatura() retourne un message d'erreur si les arguments de la fonction ne sont pas bien utilisés", {

   expect_error(SimulNatura(file_arbre = data_arbre, horizon=4),"Si file_compile n'est pas specifie, file_arbre ET file_etude doivent etre specifies" )
   expect_error(SimulNatura(file_etude = data_etude, horizon=4),"Si file_compile n'est pas specifie, file_arbre ET file_etude doivent etre specifies" )
   expect_error(SimulNatura(file_compile = data_compile, file_etude = data_etude, horizon=4),"Seulement un des deux file_arbre et file_etude OU file_compile doit etre specifie" )
   expect_error(SimulNatura(horizon=4),"Au moins un des deux file_arbre et file_etude OU file_compile doit etre specifie")
   expect_error(SimulNatura(file_compile = data_compile, horizon=16),"horizon doit etre de 1 a 15" )
   expect_error(SimulNatura(file_compile = data_compile, horizon=2, dec_perturb = 3),"dec_perturb doit etre <= horizon")
   expect_error(SimulNatura(file_compile = data_compile, horizon=2, dec_tbe1 = 3),"dec_tbe1 doit etre <= horizon" )
   expect_error(SimulNatura(file_compile = data_compile, horizon=2, dec_tbe1 = 2),"Si dec_tbe1 est specifie, tbe1 doit etre specifie aussi" )
   expect_error(SimulNatura(file_compile = data_compile, horizon=2, tbe1 = 2),"Si tbe1 est specifie, dec_tbe1 doit etre specifie aussi" )
   expect_error(SimulNatura(file_compile = data_compile, horizon=2, dec_tbe1=2, tbe1 = 2.5) )
   expect_error(SimulNatura(file_compile = data_compile, horizon=2, dec_tbe2=2, tbe2 = 2),"dec_tbe1 et tbe1 doivent etre specifies pour pouvoir utiliser dec_tbe2" )
   expect_error(SimulNatura(file_compile = data_compile, horizon=3, dec_tbe1=1, tbe1=2, dec_tbe2=4, tbe2 = 2),"dec_tbe2 doit etre <= horizon" )
   expect_error(SimulNatura(file_compile = data_compile, horizon=3, dec_tbe1=2, tbe1=2, dec_tbe2=1, tbe2 = 2) )
   expect_error(SimulNatura(file_compile = data_compile, horizon=3, dec_tbe1=1, tbe1=2, dec_tbe2=2),"Si dec_tbe2 est specifie, tbe2 doit etre specifie aussi" )
   expect_error(SimulNatura(file_compile = data_compile, horizon=3, dec_tbe1=1, tbe1=2, tbe2=2),"Si tbe2 est specifie, dec_tbe2 doit etre specifie aussi" )
   expect_error(SimulNatura(file_compile = data_compile, horizon=3, dec_tbe1=1, tbe1=2, dec_tbe2=2, tbe2=2.5) )
   expect_error(SimulNatura(file_arbre=data_arbre, file_etude=data_etude, horizon=5, ht='T'),"ht doit etre TRUE ou FALSE")
   expect_error(SimulNatura(file_arbre=data_arbre, file_etude=data_etude, horizon=5, vol='T'),"vol doit etre TRUE ou FALSE" )
   expect_error(SimulNatura(file_arbre=data_arbre, file_etude=data_etude, horizon=5, climat='T'),"climat doit etre TRUE ou FALSE" )

})


test_that("La fonction principale SimulNatura() fonction avec climat=T et un fichier d'arbres", {

  data_arbre <- readRDS(test_path("fixtures", "fic_arbre_ex.rds")) %>%
    mutate(latitude=47, longitude=-76) %>%
    dplyr::select(-P_TOT, -t_ma)
  data_etude <- readRDS(test_path("fixtures", "fic_etude_ex.rds"))

  data <- SimulNatura(file_arbre=data_arbre, file_etude=data_etude, horizon=4, climat=T)

  expect(is.data.frame(data),TRUE)
})

test_that("La fonction principale SimulNatura() fonction avec climat=T et un fichier compile", {

  data_compile <- readRDS(test_path("fixtures", "fic_compil_ex.rds")) %>%
    mutate(latitude=47, longitude=-76) %>%
    dplyr::select(-P_TOT, -t_ma)

  data <- SimulNatura(file_compile=data_compile, horizon=4, climat=T)

  expect(is.data.frame(data),TRUE)

})

