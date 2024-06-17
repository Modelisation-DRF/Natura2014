# créer des fichiers d'exemples à ajouter au package


##############################################
# Fichiers à l'échelle de l'arbre avec climat
fichier_arbres <- read_delim(file="data-raw/fichier_arbres.csv", delim = ';')
fichier_arbres_etudes <- read_delim(file="data-raw/fichier_arbres_etudes.csv", delim = ';')

# names(fichier_arbres)
fichier_arbres <- fichier_arbres %>%
  mutate(sdom_bio=ifelse(SDOM_BIO==4, '4EST', '5EST')) %>%
  dplyr::select(ID_PE, essence, dhpcm, ETAT, TIGE_HA, sdom_bio, REG_ECO, type_eco, ALTITUDE, p_tot, t_ma) %>%
  rename(placette=ID_PE)

# names(fichier_arbres_etudes)
fichier_arbres_etudes <- fichier_arbres_etudes %>%
  rename(placette=ID_PE) %>%
  mutate(age = ifelse(placette=="0319801702", 50, 60))

# sauvegarder le fichier en rda sous /data
usethis::use_data(fichier_arbres, fichier_arbres_etudes, overwrite = TRUE)

# Fichiers à l'échelle de l'arbre sans climat
fichier_arbres <- read_delim(file="data-raw/fichier_arbres.csv", delim = ';')
# names(fichier_arbres)
fichier_arbres_sans_climat <- fichier_arbres %>%
  mutate(sdom_bio=ifelse(SDOM_BIO==4, '4EST', '5EST')) %>%
  dplyr::select(ID_PE, LATITUDE, LONGITUDE, essence, dhpcm, ETAT, TIGE_HA, sdom_bio, REG_ECO, type_eco, ALTITUDE) %>%
  rename(placette=ID_PE)

usethis::use_data(fichier_arbres_sans_climat, overwrite = TRUE)



############################################
# Fichier compilé à la placette
fichier_compile <- read_delim(file="data-raw/fichier_compile.csv", delim = ';')
# names(fichier_compile)
fichier_compile <- fichier_compile %>%
  rename(placette=id_pe, age=temps) %>%
  mutate(nfi = nbop+npeu,
         stfi = stbop+stpeu,
         vfi = vbop+vpeu,
         nrt = nrt+nepn+nepx,
         strt = strt+stepn+stepx,
         vrt = vrt+vepn+vepx) %>%
  dplyr::select(placette, sdom_bio, reg_eco, type_eco, age, is, hd, p_tot, t_ma, altitude,
                nfi, nft, nrt, nri, nsab,
                stfi, stft, strt, stri, stsab,
                vfi, vft, vrt, vri, vsab)

# sauvegarder le fichier en rda sous /data
usethis::use_data(fichier_compile, overwrite = TRUE)


