#' Fonction principale pour une simulation de la croissance d'un peuplement avec le simulateur Natura-2014
#'
#' @description \code{SimulNatura()} est la fonction principale pour exécuter une simulation de la croissance d'un peuplement avec le simulateur Natura-2014.
#' Le simulateur s'utilise à partir d'un fichier d'inventaire d'arbres ou d'un fichier à l'échelle de la placette dans lequel les caractéristiques dendrométriques sont fournies.
#'
#' @details Les simulations ne se font qu'en mode déterministe. Les 5 groupes d'essences de Natura-2014 sont:
#' \itemize{
#'    \item fi: feuillus intolérants (BOG, BOP, CAC, CET, CHR, ERA, NOC, PEB, PED, PEG, PET)
#'    \item ft: feuillus tolérants (BOJ, CAF, CHB, CHE, CHG, ERN, ERR, ERS, FRA, FRN, FRP, HEG, ORA, ORR, ORT, OSV, TIL
#'    \item ri: résineux intolérants (MEL, PIG, PIR)
#'    \item rt: résineux tolérants (EPB, EPN, EPO, EPR, PIB, PRU, THO)
#'    \item sab: sapin baumier
#'    }
#'
#' Pour plus de détails sur le simulateur Natura-2014, voir la note de recherche forestière 147:
#'
#'  Auger I., 2017. Natura-2014 : Mise à jour et évaluation du modèle de croissance forestière à l’échelle du peuplement. Gouvernement du Québec,
#'  ministère des Forêts, de la Faune et des Parcs, Direction de la recherche forestière. Note de recherche forestière no 147. 31 p.
#'
#' @param file_arbre Nom du fichier contenant les informations sur les arbres et les placettes (table, csv ou xlsx). Le fichier doit contenir une ligne par arbre ou par classe de dhp/essence.
#' Si ce paramètre n'est pas utilisé, \code{file_compile} doit l'être. Le nom des colonnes doit être:
#'  \itemize{
#'    \item placette: identifiant unique de la placette
#'    \item sdom_bio: code du sous-domaine bioclimatique, en majuscule (ex: 2EST, 4OUEST ou 2E, 4O)
#'    \item altitude: altitude (m)
#'    \item type_eco: code du type écologique (ex: MS22)
#'    \item p_tot: précipitation totale annuelle, moyenne sur la période 1980-2010 (mm)
#'    \item t_ma: température moyenne annuelle sur la période 1980-2010 (Celcius)
#'    \item essence: code de l'essence de l'arbre (ex: SAB, EPN, BOP)
#'    \item dhpcm: dhp (cm) de l'arbre ou classe de dhp (seuls les arbres de plus de 9 cm sont retenus)
#'    \item tige_ha: nombre d'arbres de l'essence et de la classe de dhp, à l'hectare
#'    \item etat: code d'état de l'arbre, seuls les vivants sont retenus, codes 10, 12, 30, 32, 40, 42, 50, 52
#'    \item hauteur_pred: optionnel. hauteur totale de l'arbre (m). Si non fourni, la hauteur sera estimé à partir de la relation hauteur-diamètre de Auger (2016)
#'    \item vol_dm3: optionnel. volume marchand de l'arbre ou d'un arbre dans la classe de dhp (dm3). Si non fourni, le volume sera estimé à partir du tarif de cubage de Fortin et al. (2007)
#'  }
#' @param file_etude Nom du fichier contenant les arbres-études (table, csv ou xlsx) pour l'estimation de la hauteur dominante et l'âge du peuplement. Le fichier doit contenir une ligne par arbre-étude.
#'  Si ce paramètre n'est pas utilisé, \code{file_compile} doit l'être. Le nom des colonnes doit être:
#'  \itemize{
#'    \item placette: identifiant unique de la placette
#'    \item etage: code d'étage de l'arbre: C, D, O, I, V, seuls les C et D sont retenus
#'    \item essence: code de l'essence de l'arbre-étude (ex: SAB, EPN, BOP)
#'    \item dhpcm: dhp de l'arbre-étude (cm)
#'    \item hauteur: hauteur totale de l'arbre-étude (m)
#'    \item age: âge de l'arbre-étude, mesuré à 1 m (années)
#'  }
#' @param file_compile Optionnel. Nom du fichier contenant les caractéristiques dendrométriques des placettes (table, csv ou xlsx). Une ligne par placette.
#'  Si ce paramètre n'est pas utilisé, \code{file_arbre} et \code{file_etude} doivent l'être. Le nom des colonnes doit être:
#'  \itemize{
#'    \item placette: identifiant unique de la placette
#'    \item sdom_bio: code du sous-domaine bioclimatique, en majuscule (ex: 2EST, 4OUEST ou 2E, 4O)
#'    \item altitude: altitude (m)
#'    \item type_eco: code du type écologique (ex: MS22)
#'    \item p_tot: précipitation totale annuelle, moyenne sur la période 1980-2010 (mm)
#'    \item t_ma: température moyenne annuelle sur la période 1980-2010 (Celcius)
#'    \item age: âge du peuplement (années)
#'    \item is: indice de structure diamétrale de Shannon (valeur entre 0 et 1; 0 : toutes les tiges sont dans la même classe de dhp; 1 : les tiges sont également distribuées dans les classes de dhp)
#'    \item hd: hauteur dominante, hauteur moyenne des 100 plus gros arbres à l'ha (m)
#'    \item nfi, nft, nri, nrt, nsab : nombre d'arbres de plus de 9 cm du groupe d'essences à l'hectare (nb/ha), mettre 0 si absent.
#'    \item stfi, stft, stri, strt, stsab: surface terrière marchande du groupe d'essences (m2/ha), mettre 0 si absent.
#'    \item vfi, vft, vri, vrt, vsab: volume marchand brut du groupe d'essences (m3/ha), mettre 0 si absent. Si aucun volume, mettre les 5 groupes à 0 et l'évolution du volume ne sera pas effectué.
#' }
#' @param file_export  Nom du fichier à exporter contenant les résultats de la simulation (format csv), optionnel.
#' @param horizon Nombre de décennies à simuler, un chiffre de 1 à 15
#' @param ht Booléen
#' \itemize{
#'   \item \code{TRUE} (par défaut): la hauteur de chacun des arbres doit être estimée
#'   \item \code{FALSE} : la variable hauteur_pred est fournie dans le fichier \code{file_arbre} (en m)
#'   }
#' @param vol Booléen
#' \itemize{
#'   \item \code{TRUE} (par défaut): le volume de chacun des arbres doit être estimé
#'   \item \code{FALSE}: la variable vol_dm3 est fournie dans le fichier \code{file_arbre} (en dm3 par tige), si le volume est fourni, la hauteur ne sera pas estimée.
#' }
#' @param dec_perturb Numéro de la décennie avec une perturbation partielle (chablis partiel, brulis partiel), mettre 0 si aucune perturbation (0 par défaut)
#' @param dec_tbe1 Numéro de la première décennie avec une épidémie de TBE, mettre 0 si aucune d'épidémie (0 par défaut)
#' @param tbe1 Sévérité de la première épidémie, un entier de 1 à 5, mettre 0 si pas de TBE (0 par défaut)
#' @param dec_tbe2 Numéro de la deuxième décennie avec une épidémie de TBE, mettre 0 si pas de 2e épidémie (0 par défaut)
#' @param tbe2 Sévérité de la deuxième épidémie, un entier de 1 à 5, mettre 0 si pas de 2e épidémie (par défaut)
#'
#' @return Une table contenant les résultats de la simulation, une ligne par placette/décennie
#'  \itemize{
#'    \item placette: identifiant unique de la placette
#'    \item sdom_bio: code du sous-domaine bioclimatique, en majuscule (ex: 2EST, 4OUEST ou 2E, 4O)
#'    \item altitude: altitude (m)
#'    \item type_eco: code du type écologique (ex: MS22)
#'    \item p_tot: précipitation totale annuelle (mm)
#'    \item t_ma: température moyenne annuelle  (Celcius)
#'    \item age: âge du peuplement (années)
#'    \item is: indice de structure diamétrale de Shannon
#'    \item hd: hauteur dominante (m)
#'    \item ntot: nombre total de d'arbres de plus de 9 cm (somme des 5 groupes d'essences) (nb/ha)
#'    \item sttot: surface terrière marchande totale (somme des 5 groupes d'essences) (m2/ha)
#'    \item vtot: volume marchand brut total (somme des 5 groupes d'essences) (m3/ha)
#'    \item dqtot: dhp moyen quadratique toutes essences (calculé à partir de ntot et sttot) (cm)
#'    \item nfi, nft, nri, nrt, nsab : nombre d'arbres de plus de 9 cm du groupe d'essences (nb/ha)
#'    \item stfi, stft, stri, strt, stsab: surface terrière marchande du groupe d'essences (m2/ha)
#'    \item vfi, vft, vri, vrt, vsab: volume marchand brut du groupe d'essences (m3/ha)
#'    \item dqfi, dqft, dqri, dqrt, dqsab : dhp moyen quadratique du groupe d'essences, calculé à partir de n et st (cm)
#'    \item indice_tbe: Sévérité de l'épidémie de TBE à la décennie simulée
#'    \item presence_perturb: 1 à la décennie où une perturbation a été simulée
#'    \item Warning: Prend la valeur de 1 si le nombre de tiges à l’hectare d’un groupe d’essences dépasse 5000 tiges/ha ou si la surface terrière d’un groupe dépasse 100 m2/ha pour la placette.
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' # Simulation  sur 50 ans à partir d'un fichier à l'échelle de l'arbre
#' data_simul <- SimulNatura(file_arbre=fichier_arbres, file_etude=fichier_arbres_etudes, horizon=5)
#'
#' # Simulation déterministe sur 50 ans à partir d'un fichier à l'échelle de la placette
#' data_simul <- SimulNatura(file_compile=fichier_compile, horizon=5)
#' }
SimulNatura <- function(file_arbre, file_etude, file_compile, file_export, horizon, ht=TRUE, vol=TRUE, dec_perturb=0, dec_tbe1=0, tbe1=0, dec_tbe2=0, tbe2=0){


  # file_arbre=fichier_arbres; file_etude=fichier_arbres_etudes; horizon=5; ht=TRUE; vol=TRUE; dec_perturb=0; dec_tbe1=0; tbe1=0; dec_tbe2=0; tbe2=0;
  # file_compile=fichier_compile; horizon=5; ht=TRUE; vol=TRUE; dec_perturb=0; dec_tbe1=0; tbe1=0; dec_tbe2=0; tbe2=0;
  # file_arbre=data_arbre; file_etude=data_etude; horizon=5; ht=TRUE; vol=TRUE; dec_perturb=0; dec_tbe1=0; tbe1=0; dec_tbe2=0; tbe2=0;

#############################################################################################
################### Vérification des arguments de la fonction        #######################
############################################################################################

  # Vérification des arguments de la fonction principale
  Erreur <- CheckArguments(file_arbre=file_arbre, file_etude=file_etude, file_compile=file_compile, horizon=horizon, ht=ht, vol=vol, dec_perturb=dec_perturb, dec_tbe1=dec_tbe1, tbe1=tbe1, dec_tbe2=dec_tbe2, tbe2=tbe2)
  if (Erreur != 'ok') {stop(Erreur)}



  #############################################################################################
  ################### Importation des fichiers et préparation des données ######################
  ############################################################################################


  if (!missing(file_arbre)) {
    # Lescture du fichier des arbres
    Arbres <- Lecture_arbres(file_arbre=file_arbre, ht=ht, vol=vol)
    if (is.character(Arbres)) {stop(Arbres)} # Erreur lors de la lecture du fichier des arbres

    # Lecture du fichier des études d'arbres
    EtudeA<-Lecture_etudes(file_etude=file_etude)
    if (is.character(EtudeA)) {stop(EtudeA)}

    # Exécuter la fonction qui prépare le fichier des arbres, et le compile à l'échelle des groupes d'essences
    if (isFALSE(vol)) ht <- FALSE  # si le volume est fourni, on bypass le calcul de la hauteur car on en n'a pas besoin
    DataCompile <- Prep_arbres(fic_arbre=Arbres, ht=ht, vol=vol) # si tout es ok, on prépare les arbres et on compile à la placette

    # appliquer la fonction qui calcule la hdom et l'age du la placette à partir des études d'arbres
    AgeHD <- Prep_etude(fic_etude=EtudeA, compile_arbre=DataCompile)

    # ajouter l'age et hdom au fichier compilé
    DataCompile <- inner_join(DataCompile, AgeHD, by = "placette") %>%
      dplyr::select(-dhp4_moy)

  }

  # si fichier compile a la placette
  else {

    # Lectude du fichier compilé à la placette
    DataCompile0 <- Lecture_compile(file_compile=file_compile)
    if (is.character(DataCompile0)) {stop(DataCompile0)}

      # filtrer les placettes (je le fais à l'extérieur de la fonction pour avoir acces au fichier non filtré)
      DataCompile <- DataCompile0 %>%
        dplyr::select(placette, sdom_bio, altitude, p_tot, t_ma, type_eco, age, is, hd, nfi, nft, nri, nrt, nsab, stfi, stft, stri, strt, stsab, vfi, vft, vri, vrt, vsab) %>%
        mutate(sdom_bio=substr(sdom_bio,1,2),
               nfi = nfi/25,
               nft = nft/25,
               nri = nri/25,
               nrt = nrt/25,
               nsab = nsab/25,
               ncom = nfi+nft+nri+nrt+nsab,
               stcom = stfi+stft+stri+strt+stsab,
               vcom = vfi+vft+vri+vrt+vsab,
               veg_pot = substr(type_eco,1,3),
               milieu = as.character(substr(type_eco,4,4))) %>%
        filter(sdom_bio %in% c('1','2E','2O','3E','3O','4E','4O','5E','5O','6E','6O')) %>%
        filter(milieu %in% c("0","1","2","3","4","5","6","7","8","9"))
        #filter(veg_pot %in% c('FE1','FE2','FE3','FE4','FE5','FE6','FC1','FO1','ME1','MF1','MJ1','MJ2','MS1','MS2','MS4','MS6','RB1','RB2','RB5','RC3','RE1','RE2','RE3','RE4','RP1','RS1','RS2','RS3','RS4','RS5','RT1'))


      # Ne garder que les vp de natura-2014
      DataCompile <- inner_join(DataCompile, vp, by='veg_pot')

  }


# ajouter les paramètres des équations de Natura au fichier compilé
Data_compile2 <- Prep_parametre(data=DataCompile)


# Simulation
Data_simule <- Natura(data=Data_compile2, horizon=horizon, dec_perturb=dec_perturb, dec_tbe1=dec_tbe1, tbe1=tbe1, dec_tbe2=dec_tbe2, tbe2=tbe2)

# ajouter les placettes non simulées aux placettes simulées
# if (!missing(file_arbre)) { # créer une liste des placettes soumises
#   liste <- Arbres %>%
#     dplyr::select(placette, type_eco) %>%
#     unique()
# }
# else if (!missing(file_compile)) {
#   liste <- DataCompile0 %>%
#     dplyr::select(placette, type_eco) %>%
#     unique()
# }
# liste_simul <- Data_simule %>% # créer une liste des placettes simulées
#   dplyr::select(placette, sdom_bio, altitude, ptot, tmoy, type_eco) %>%
#   unique()
# liste_non_simul <- anti_join(liste, liste_simul, by=c("placette", "type_eco")) # liste des placettes non simulées
# Data_simule_tous <- bind_rows(Data_simule, liste_non_simul)


# Exporter la simulation
if (!missing(file_export)) {
write_delim(Data_simule, file_export, delim = ';')
}



return(Data_simule)

}


