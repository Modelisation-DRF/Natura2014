# Tous les fichiers internes excel/csv/sas7bdat doivent être convertis en en seul fichier rda nommé sysdata.rda sous /R
# Tous les fichiers d'exemples doivent être convertis individuellement en rda et mis sous /data
# le fichier avec le code pour créer le fichier sysdata.rda doit être sauvegardé sous R/data-raw

# param_tarif = read.sas7bdat("c:/Mes docs/ data/ beta_volume.sas7bdat")
# param_ht = read.sas7bdat("c:/Mes docs/ data/ beta_ht.sas7bdat")
# Puis utiliser la ligne de code suivant (toujours dans le projet du package)
# usethis::use_data(param_tarif, param_ht, internal=TRUE): ça fonctionne seulement si le projet est un package

# library(readxl)
# library(sas7bdat)
# library(tidyverse)



########################################################################################

# lire le fichier d'association des essences
espece <- read_delim("data-raw/Especes.csv", delim = ';')  %>%
    select(Essence, Groupe_ess) %>%
    filter(Groupe_ess != 'NC')
names(espece) <- tolower(names(espece))


########################################################################################

# lire le fichier des veg_pot retenues
vp <- read_delim("data-raw/Vegpot.csv", delim = ';') %>%
    select(VegPotName) %>%
    rename(veg_pot=VegPotName)


########################################################################################

# fichier des paramètres pour le calcul des ht des arbres pour la hdom, dans la fct param_hdom0_ess_stoch()
# paramètres pour l'equation de la hauteur dominante
param_hd <- read.sas7bdat("data-raw/parmshd_random_arbre_tous.sas7bdat") %>%
  filter(Parameter=='b2') %>%
  rename(b2=Estimate) %>%
  select(b2)
b2 <- as.vector(param_hd$b2)


########################################################################################




########################################################################################

# Fichiers pour l'évolution de N-St-V, fichiers par essence
ParmsFi <- read.sas7bdat("data-raw/parms_fi_20140506.sas7bdat") %>%
  select(-X_NAME_, -X_TYPE_, -X_STATUS_, -X_NUSED_)

ParmsFt <- read.sas7bdat("data-raw/parms_ft_20140506.sas7bdat") %>%
  select(-X_NAME_, -X_TYPE_, -X_STATUS_, -X_NUSED_)

ParmsRi <- read.sas7bdat("data-raw/parms_ri_20140506.sas7bdat") %>%
  select(-X_NAME_, -X_TYPE_, -X_STATUS_, -X_NUSED_)

ParmsRt <- read.sas7bdat("data-raw/parms_rt_20140506.sas7bdat") %>%
  select(-X_NAME_, -X_TYPE_, -X_STATUS_, -X_NUSED_)

ParmsSab<-read.sas7bdat("data-raw/parms_sab_20141119.sas7bdat") %>%
  select(-X_NAME_, -X_TYPE_, -X_STATUS_, -X_NUSED_)


########################################################################################


# Fichiers pour l'évolution de is et hd et moyenne
ParmsIsd <- read.sas7bdat("data-raw/parms_shannon_20140424.sas7bdat") %>% mutate(sdom_bio=substr(SDOM_BIO,1,2)) %>% dplyr::select(-SDOM_BIO)
ParmsHd <- read.sas7bdat("data-raw/parms_hd_20140424.sas7bdat") %>% mutate(sdom_bio=substr(SDOM_BIO,1,2)) %>%
  dplyr::select(-'X_NAME_', -s2u, -var, -SDOM_BIO)

# moyennes pour le calcul de la hauteur dominante
ParmsMoy<-read.sas7bdat("data-raw/moyenne_20140424.sas7bdat") %>% mutate(sdom_bio=substr(SDOM_BIO,1,2)) %>%
  dplyr::select(-'X_TYPE_', -'X_FREQ_', -SDOM_BIO)


########################################################################################

# tous les fichier à mettre dans le rda
usethis::use_data(espece, vp,
                  b2, ParmsMoy,
                  ParmsFi, ParmsFt, ParmsRi, ParmsRt, ParmsSab,
                  ParmsIsd, ParmsHd,
                  internal=TRUE, overwrite = TRUE)


