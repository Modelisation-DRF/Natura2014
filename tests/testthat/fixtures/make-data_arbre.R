# Fichier test: fichier d'exemple arbre et etude de Capsis, et résultats de simulation par Capsis.
# fichier des arbres
fic_arbre_ex <- read_delim("tests/testthat/fixtures/inventaire_Natura2.csv", delim = ';')
fic_arbre_ex <- fic_arbre_ex %>%
  mutate(tige_ha = NB_TIGE*25) %>%
  rename(placette=ID_PLACETTE, t_ma=T_MOY, dhpcm=DHP_CM) %>%
  dplyr::select(-ID_STRATE, -REG_ECO, -CL_DRAI, -LATITUDE, -LONGITUDE, -NB_TIGE)

# ficxhier des études d'arbre
fic_etude_ex <- read_delim("tests/testthat/fixtures/arbres_etudes_Natura2.csv", delim = ';')
fic_etude_ex <- fic_etude_ex %>%
  rename(placette=ID_PLACETTE, dhpcm=DHP_CM) %>%
  mutate(hauteur = HAUTEUR_DM/10) %>%
  dplyr::select(-ID_STRATE, -NO_ARBRE, -NIVLECTAGE, -HAUTEUR_DM)

# fichier des résultats de simulation de capsis, sur 40 ans
fic_result50ans_capsis <- read_delim("tests/testthat/fixtures/export_capsis.csv", delim = ';')
names(fic_result50ans_capsis) <- tolower(names(fic_result50ans_capsis))
fic_capsis_50ans <- fic_result50ans_capsis %>%
  dplyr::select(placetteid, annee, age, tbe, perturb, horslimite, hdom, indshannon,
                contains("stm2ha"), contains("volm3ha"), contains("diamq"), contains("nbha")
                ) %>%
  dplyr::select(-contains("pig"), -contains("epn"), -contains("epb"), -contains("epr"), -contains("tho"), -contains("pib"), -contains("bop"),
                -contains("peu"), -contains("mel"), -contains("pir")) %>%
  rename(placette = placetteid,
         indice_tbe = tbe,
         presence_perturb = perturb,
         hd = hdom,
         is = indshannon,
         Warning = horslimite,
         ntot = totnbha, sttot=totstm2ha, vtot=totvolm3ha, dqtot=totdiamq,
         stfi=stm2hafi, stft=stm2haft, stri=stm2hari, strt=stm2hart, stsab=stm2hasab,
         nfi=nbhafi, nft=nbhaft, nri=nbhari, nrt=nbhart, nsab=nbhasab,
         vfi=volm3hafi, vft=volm3haft, vri=volm3hari, vrt=volm3hart, vsab=volm3hasab) %>%
  mutate(dqfi = ifelse(diamqfi==0,NA,diamqfi),
         dqft = ifelse(diamqft==0,NA,diamqft),
         dqri = ifelse(diamqri==0,NA,diamqri),
         dqrt = ifelse(diamqrt==0,NA,diamqrt),
         dqsab = ifelse(diamqsab==0,NA,diamqsab)) %>%
  filter(annee<=40)


saveRDS(fic_capsis_50ans, "tests/testthat/fixtures/fic_capsis_50ans.rds")
saveRDS(fic_etude_ex, "tests/testthat/fixtures/fic_etude_ex.rds")
saveRDS(fic_arbre_ex, "tests/testthat/fixtures/fic_arbre_ex.rds")


#######################################################################################


# Fichier test: fichier d'exemple compilé à la placette de Capsis, et résultats de simulation par Capsis.
# fichier des placettes compilées
fic_compil_ex <- read_delim("tests/testthat/fixtures/inventaire_Natura_compile2.csv", delim = ';')
names(fic_compil_ex)
fic_compil_ex <- fic_compil_ex %>%
  rename(placette=ID_PLACETTE, t_ma=T_MOY, is=IndShannon, hd=HDom,
         nfi=NbHaFi, nft=NbHaFt, nri=NbHaRi, nrt=NbHaRt, nsab=NbHaSab,
         stfi=stm2HaFi, stft=stm2HaFt, stri=stm2HaRi, strt=stm2HaRt, stsab=stm2haSab,
         vfi=volm3HaFi, vft=volm3HaFt, vri=volm3HaRi, vrt=volm3HaRt, vsab=volm3haSab) %>%
  dplyr::select(-ID_STRATE, -REG_ECO, -CL_DRAI, -LATITUDE, -LONGITUDE)


# fichier des résultats de simulation de capsis, sur 40 ans
fic_result40ans_capsis <- read_delim("tests/testthat/fixtures/export_compile_capsis.csv", delim = ';')
names(fic_result40ans_capsis) <- tolower(names(fic_result40ans_capsis))
names(fic_result40ans_capsis)
fic_capsis_compile_40ans <- fic_result40ans_capsis %>%
  dplyr::select(placetteid, annee, age, tbe, perturb, hdom, indshannon,
                contains("stm2ha"), contains("volm3ha"), contains("diamq"), contains("nbha")
  ) %>%
  dplyr::select(-contains("pig"), -contains("epn"), -contains("epb"), -contains("epr"), -contains("tho"), -contains("pib"), -contains("bop"),
                -contains("peu"), -contains("mel"), -contains("pir")) %>%
  rename(placette = placetteid,
         indice_tbe = tbe,
         presence_perturb = perturb,
         hd = hdom,
         is = indshannon,
         #Warning = horslimite,
         ntot = totnbha, sttot=totstm2ha, vtot=totvolm3ha, dqtot=totdiamq,
         stfi=stm2hafi, stft=stm2haft, stri=stm2hari, strt=stm2hart, stsab=stm2hasab,
         nfi=nbhafi, nft=nbhaft, nri=nbhari, nrt=nbhart, nsab=nbhasab,
         vfi=volm3hafi, vft=volm3haft, vri=volm3hari, vrt=volm3hart, vsab=volm3hasab) %>%
  mutate(dqfi = ifelse(diamqfi==0,NA,diamqfi),
         dqft = ifelse(diamqft==0,NA,diamqft),
         dqri = ifelse(diamqri==0,NA,diamqri),
         dqrt = ifelse(diamqrt==0,NA,diamqrt),
         dqsab = ifelse(diamqsab==0,NA,diamqsab)) %>%
  filter(annee<=40)


saveRDS(fic_capsis_compile_40ans, "tests/testthat/fixtures/fic_capsis_compile_40ans.rds")
saveRDS(fic_compil_ex, "tests/testthat/fixtures/fic_compil_ex.rds")

