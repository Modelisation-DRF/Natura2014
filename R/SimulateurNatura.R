#' Fonction principale pour une simulation de la croissance d'un peuplement avec le simulateur Natura-2014
#'
#' @description \code{SimulNatura2014()} est la fonction principale pour exécuter une simulation de la croissance d'un peuplement avec le simulateur Natura-2014.
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
#'    \item altitude: altitude (m), optionnel : nécessaire si \code{climat=T}
#'    \item type_eco: code du type écologique (ex: MS22)
#'    \item p_tot: précipitation totale annuelle, moyenne sur la période 1980-2010 (mm). Optionnel: si non fourni, mettre \code{climat=T}.
#'    \item t_ma: température moyenne annuelle sur la période 1980-2010 (Celcius). Optionnel: si non fourni, mettre \code{climat=T}.
#'    \item essence: code de l'essence de l'arbre (ex: SAB, EPN, BOP)
#'    \item dhpcm: dhp (cm) de l'arbre ou classe de dhp (seuls les arbres de plus de 9 cm sont retenus)
#'    \item tige_ha: nombre d'arbres de l'essence et de la classe de dhp, à l'hectare
#'    \item etat: code d'état de l'arbre, seuls les vivants sont retenus, codes 10, 12, 30, 32, 40, 42, 50, 52
#'    \item hauteur_pred: optionnel. hauteur totale de l'arbre (m). Si non fourni, la hauteur sera estimé à partir de la relation hauteur-diamètre de Auger (2016)
#'    \item vol_dm3: optionnel. volume marchand de l'arbre ou d'un arbre dans la classe de dhp (dm3). Si non fourni, le volume sera estimé à partir du tarif de cubage de Fortin et al. (2007)
#'    \item latitude, longitude: coordonnées des placettes, en degré décimal. Optionel: nécessaires si \code{climat=T}
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
#'    \item type_eco: code du type écologique (ex: MS22)
#'    \item p_tot: précipitation totale annuelle, moyenne sur la période 1980-2010 (mm). Optionnel: si non fourni, mettre \code{climat=T}.
#'    \item t_ma: température moyenne annuelle sur la période 1980-2010 (Celcius). Optionnel: si non fourni, mettre \code{climat=T}.
#'    \item age: âge du peuplement (années)
#'    \item is: indice de structure diamétrale de Shannon (valeur entre 0 et 1; 0 : toutes les tiges sont dans la même classe de dhp; 1 : les tiges sont également distribuées dans les classes de dhp)
#'    \item hd: hauteur dominante, hauteur moyenne des 100 plus gros arbres à l'ha (m)
#'    \item nfi, nft, nri, nrt, nsab : nombre d'arbres de plus de 9 cm du groupe d'essences à l'hectare (nb/ha), mettre 0 si absent.
#'    \item stfi, stft, stri, strt, stsab: surface terrière marchande du groupe d'essences (m2/ha), mettre 0 si absent.
#'    \item vfi, vft, vri, vrt, vsab: volume marchand brut du groupe d'essences (m3/ha), mettre 0 si absent. Si aucun volume, mettre les 5 groupes à 0 et l'évolution du volume ne sera pas effectué.
#'    \item latitude, longitude: coordonnées des placettes, en degré décimal. Optionel: nécessaires si \code{climat=T}
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
#' @param climat Booléen
#' \itemize{
#'   \item \code{TRUE} (par défaut): si les variables climatiques doivent être extraites des cartes. Les colonnes latitude et longitude de la placette doivent alors être fournies dans \code{file_arbre} ou dans \code{file_compile}.
#'   \item \code{FALSE}: si les variables climatiques sont fournies dans le fichier \code{file_arbre} ou \code{file_compile}.
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
#'    \item type_eco: code du type écologique (ex: MS22)
#'    \item ptot: précipitation totale annuelle (mm)
#'    \item tmoy: température moyenne annuelle  (Celcius)
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
#' # Simulation sur 50 ans à partir d'un fichier à l'échelle de l'arbre
#' data_simul <- SimulNatura2014(file_arbre=fichier_arbres, file_etude=fichier_arbres_etudes,
#'  horizon=5, climat=F)
#'
#' # Simulation sur 50 ans à partir d'un fichier à l'échelle de l'arbre,
#' # qui ne contient pas les variables climatiques
#' data_simul <- SimulNatura2014(file_arbre=fichier_arbres_sans_climat,
#' file_etude=fichier_arbres_etudes, horizon=5)
#'
#' # Simulation sur 50 ans à partir d'un fichier à l'échelle de la placette
#' data_simul <- SimulNatura2014(file_compile=fichier_compile, horizon=5, climat=F)
#' }
SimulNatura2014 <- function(file_arbre, file_etude, file_compile, file_export, horizon, ht=TRUE, vol=TRUE, climat=TRUE, dec_perturb=0, dec_tbe1=0, tbe1=0, dec_tbe2=0, tbe2=0){

  # file_arbre=fichier_arbres_sans_climat; file_etude=fichier_arbres_etudes; horizon=5; climat=TRUE; ht=TRUE; vol=TRUE; dec_perturb=0; dec_tbe1=0; tbe1=0; dec_tbe2=0; tbe2=0;
  # file_compile=fichier_compile; horizon=5; climat=TRUE; ht=TRUE; vol=TRUE; dec_perturb=0; dec_tbe1=0; tbe1=0; dec_tbe2=0; tbe2=0;
  # file_arbre=data_arbre; file_etude=data_etude; horizon=5; ht=TRUE; vol=TRUE; dec_perturb=0; dec_tbe1=0; tbe1=0; dec_tbe2=0; tbe2=0;


  # variables nécessaires à Natura et à extraire des rasters (nom dans les rasters)
  variable_climat <- c("totalprecipitation","tmean")


  #variables nécessaires à Natura et à extraire des cartes (nom utilisé dans ce package)
  variable_climat_ <- c("p_tot", "t_ma")



#############################################################################################
################### Vérification des arguments de la fonction        #######################
############################################################################################

  # Vérification des arguments de la fonction principale
  Erreur <- CheckArguments(file_arbre=file_arbre, file_etude=file_etude, file_compile=file_compile, horizon=horizon, ht=ht, vol=vol,
                           dec_perturb=dec_perturb, dec_tbe1=dec_tbe1, tbe1=tbe1, dec_tbe2=dec_tbe2, tbe2=tbe2, climat=climat)
  if (Erreur != 'ok') {stop(Erreur)}



  #############################################################################################
  ################### Importation des fichiers et préparation des données ######################
  ############################################################################################


  if (!missing(file_arbre)) {


    # enlever les variables climatique du fichier si l'extraction est demandée
    if (isTRUE(climat)){

      file_arbre <- remove_columns(file_arbre, variable_climat_) # ceci va fonctionner seulement si file_arbre est un data.frame, ne fonctionnera pas si fichier csv à importer
    }


    ##################################################################################
    ################### Lecture des fichiers arbres         ##########################
    ##################################################################################

    # si le volume est fourni, on bypass le calcul de la hauteur car on en n'a pas besoin
    if (isFALSE(vol)) ht <- FALSE

    # Lecture du fichier des arbres
    Arbres <- Lecture_arbres(file_arbre=file_arbre, ht=ht, vol=vol, climat=climat)
    if (is.character(Arbres)) {stop(Arbres)} # Erreur lors de la lecture du fichier des arbres

    # Valider le contenu des colonnes reliées aux arbres (si ht et/ou vol fournis, ils ne sont pas validés. Ils seront validés indirectement par les vxxx et vtot)
    Arbres_val <- valid_arbre(type_fic='arbres', fichier=Arbres)

    Arbres <- Arbres_val[[1]] # fichier arbres filtré
    placette_rejet1 <- Arbres_val[[2]] # liste des placettes/arbres rejetées avec le message

    # si toutes les placettes ont été rejetées, arrêter la simulation
    if (nrow(Arbres)==0) stop("Aucune placette valide dans le fichier file_arbre")

    #if (is.character(Arbres)) {stop(Arbres)} # s'il y a des erreurs, on arrête la simulation

    # ne garder que les arbres de dimension marchande et vivants
    Arbres <- Arbres %>%
      filter(dhpcm>9, etat %in% c('10', '12', '40', '42', '30', '32', '50', '52')) %>%
      mutate(no_arbre=row_number())


    # Lecture du fichier des études d'arbres
    EtudeA<-Lecture_etudes(file_etude=file_etude)
    if (is.character(EtudeA)) {stop(EtudeA)}

    # Valider le contenu des colonnes reliées aux arbres etudes
    EtudeA_val <- valid_arbre(type_fic='etudes', fichier=EtudeA)
    #if (is.character(EtudeA)) {stop(EtudeA)} # s'il y a des erreurs, on arrête la simulation

    EtudeA <- EtudeA_val[[1]] # fichier arbres-etudes filtré
    placette_rejet2 <- EtudeA_val[[2]] # liste des placettes/arbres-etudes rejetées avec le message

    EtudeA <- EtudeA %>%
      filter(dhpcm>9, toupper(etage) %in% c('C','D')) %>%
      dplyr::select(placette, essence, dhpcm, hauteur, age)

    # si toutes les placettes ont été rejetées, arrêter la simulation
    if (nrow(EtudeA)==0) stop("Aucune placette valide dans le fichier file_etude")

    ##################################################################################
    ################### Filtrer et préparer les données        #######################
    ##################################################################################

    # Filtrer le contenu des placettes
    filtre <- valid_placette(type_fic='arbres', fichier=Arbres, ht=ht, climat=climat)

    Arbres <- filtre[[1]] # fichier filtré
    placette_rejet3 <- filtre[[2]] # liste des placettes rejetées avec le message

    # si toutes les placettes ont été rejetées, arrêter la simulation
    if (nrow(Arbres)==0) stop("Aucune placette valide dans le fichier des arbres")

    # liste des placettes
    liste_place <- unique(Arbres$placette)
    # ne garder les placettes qui sont dans les arbres et dans les etude
    EtudeA <- EtudeA[EtudeA$placette %in% liste_place,]

    liste_place_etude <- unique(EtudeA$placette)
    # ne garder que les placettes qui ont des arbres études
    Arbres <- Arbres[Arbres$placette %in% liste_place_etude,]

    # Vérifier s'il reste des placettes valides
    if (nrow(Arbres)==0) {stop("Aucune placette valide dans le fichier des arbres-etudes")}


    # extraire les variables de climat pour le modèle de hauteur si nécessaire
    if (isTRUE(climat & isTRUE(ht))) {
      Arbres <- Arbres %>% rename(id_pe=placette)
      Arbres <- ExtractMap::extract_map_plot(file=Arbres, liste_raster="cartes_climat", variable=variable_climat) %>%
        rename(t_ma = tmean, p_tot = totalprecipitation, placette=id_pe)

    }


    # mettre les 3 fichiers de placette rejetées en un seul
    placette_rejet <- bind_rows(placette_rejet1, placette_rejet2, placette_rejet3)

    ##################################################################################
    ################### Compiler les arbres à la placette   ##########################
    ##################################################################################


    # Exécuter la fonction qui prépare le fichier des arbres, et le compile à l'échelle des groupes d'essences
    if (isFALSE(vol)) ht <- FALSE  # si le volume est fourni, on bypass le calcul de la hauteur car on en n'a pas besoin
    prep_arb  <- Prep_arbres(fic_arbre=Arbres, ht=ht, vol=vol) # si tout es ok, on prépare les arbres et on compile à la placette

    DataCompile <- prep_arb[[2]]      # extraire le fichier compile a la placette
    Arbres_prep <- prep_arb[[1]]      # extraire le fichier des arbres pour le calcul de la hauteur dominante

    # appliquer la fonction qui calcule la hdom et l'age du la placette à partir des études d'arbres
    AgeHD <- Prep_etude(fic_etude=EtudeA, fic_arbre=Arbres_prep)

    # ajouter l'age et hdom au fichier compilé
    DataCompile <- inner_join(DataCompile, AgeHD, by = "placette")# %>%
      #dplyr::select(-dhp4_moy)

  } # Fin de la préparation si fichier arbre et arbre-tude fournis


  ###########################################################################################################
  ################### Importation des fichiers échelle placette et préparation des données ##################
  ###########################################################################################################

  # file_compile=fichier_compile; horizon=5; climat=TRUE; ht=TRUE; vol=TRUE; dec_perturb=0; dec_tbe1=0; tbe1=0; dec_tbe2=0; tbe2=0;

  # si fichier compile a la placette
  else {

    ##################################################################################
    ################### Lecture du fichier compilé placette ##########################
    ##################################################################################


    if (isTRUE(climat)){
      file_compile <- remove_columns(file_compile, variable_climat_)
    }


    # Lectude du fichier compilé à la placette
    DataCompile0 <- Lecture_compile(file_compile=file_compile, climat=climat)
    if (is.character(DataCompile0)) {stop(DataCompile0)}


    ##################################################################################
    ################### Filtrer et préparer les données        #######################
    ##################################################################################

    # Filtrer les placettes
    filtre <- valid_placette(type_fic='compile', fichier=DataCompile0, climat=climat)

    DataCompile <- filtre[[1]] # fichier filtré
    placette_rejet <- filtre[[2]] # liste des placettes rejetées avec le message

    # si toutes les placettes ont été rejetées, arrêter la simulation
    if (nrow(DataCompile)==0) stop("Aucune placette valide dans le fichier file_compile")

    # créer et renommer les variables qui seront nécessaires
    DataCompile <- DataCompile %>%
      mutate(nfi = nfi/25, nft = nft/25, nsab = nsab/25, nri = nri/25, nrt = nrt/25)

    # extraire les variables de climat si nécessaire
    if (isTRUE(climat)) {
      DataCompile <- DataCompile %>% rename(id_pe=placette)
      DataCompile <- ExtractMap::extract_map_plot(file=DataCompile, liste_raster="cartes_climat", variable=variable_climat) %>%
        rename(t_ma = tmean, p_tot = totalprecipitation, placette=id_pe)
    }

  } # fin de la préparation du fichier compilé


  ##################################################################################
  ################### Préparer les variables nécessaires au modèle #################
  ##################################################################################

# validation des ntot, sttot et vtot
filtre <- valid_placette(type_fic='valid', fichier=DataCompile)
DataCompile <- filtre[[1]] # fichier filtré
placette_rejet2 <- filtre[[2]] # liste des placettes rejetées avec le message

# si toutes les placettes ont été rejetées, arrêter la simulation
if (nrow(DataCompile)==0) stop("Aucune placette valide selon les totaux de N, St ou V")

# Préparer les variables binaires, pour toutes les itérations, car on calcule vtot, qui peut varier s'il provient d'un fichier arbre en mode stochastique
DataCompile <- Prep_compile(fichier_compile=DataCompile)

# ajouter les paramètres des équations de Natura au fichier compilé
Data_compile2 <- Prep_parametre(data=DataCompile)


# Simulation
Data_simule <- Natura(data=Data_compile2, horizon=horizon, dec_perturb=dec_perturb, dec_tbe1=dec_tbe1, tbe1=tbe1, dec_tbe2=dec_tbe2, tbe2=tbe2)

# liste des placettes rejetées
#if (!is.null(placette_rejet) | !is.null(placette_rejet2)) {
if (nrow(placette_rejet)>0 | nrow(placette_rejet2)>0) {
  placette_rejet_tous <- bind_rows(placette_rejet, placette_rejet2) %>% arrange(placette) %>% dplyr::select(placette, message)
  # ajouter les placettes rejetées à la fin du fichier des simulations
  Data_simule <- bind_rows(Data_simule, placette_rejet_tous)
}


# Exporter la simulation
if (!missing(file_export)) {
  write_delim(Data_simule, file_export, delim = ';')
}

return(Data_simule)

}


