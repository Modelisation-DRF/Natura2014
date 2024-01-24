#' Fichier d'intrant à l'échelle de l'arbres pour Natura-2014
#'
#' Deux placettes avec leur liste d'arbres
#'
#' @format ## `fichier_arbres`
#' Table de 29 lignes et 11 colonnes:
#' \describe{
#'   \item{placette}{Identifiant de la placette}
#'   \item{essence}{Code de l'essence de l'arbre}
#'   \item{dhpcm}{Classe de DHP (cm)}
#'   \item{ETAT}{Code d'état de l'arbre}
#'   \item{TIGE_HA}{Nombre de tiges dans la classe de DHP et de l'essence, à l'hectare}
#'   \item{sdom_bio}{Code du sous-domaine bioclimatique}
#'   \item{REG_ECO}{Code de la région écologique}
#'   \item{type_eco}{Code du type écologique}
#'   \item{ALTITUDE}{Altitude (m)}
#'   \item{p_tot}{Précipitation totale annuelle (mm)}
#'   \item{t_ma}{Température moyenne annuelle (C)}
#' }
#' @examples
#' fichier_arbres
"fichier_arbres"



#' Fichier d'intrant des arbres-études pour Natura-2014
#'
#' Deux placettes avec leur liste d'arbres-études
#'
#' @format ## `fichier_arbres_etudes`
#' Table de 6 lignes et 6 colonnes:
#' \describe{
#'   \item{placette}{Identifiant de la placette}
#'   \item{ESSENCE}{Code de l'essence de l'arbre-étude}
#'   \item{etage}{Code d'étage de l'arbre-étude}
#'   \item{dhpcm}{DHP de l'arbre-étude (cm)}
#'   \item{hauteur}{Hauteur totale de l'arbre-étude (m)}
#'   \item{age}{Âge de l'arbre-étude, mesuré à 1 m (ans)}
#' }
#' @examples
#' fichier_arbres_etudes
"fichier_arbres_etudes"




#' Fichier d'intrant à l'échelle de la placette pour Natura-2014
#'
#' Deux placettes
#'
#' @format ## `fichier_compile`
#' Table de 2 lignes et 25 colonnes:
#' \describe{
#'   \item{placette}{Identifiant de la placette}
#'   \item{sdom_bio}{Code du sous-domaine bioclimatique}
#'   \item{reg_eco}{Code de la région écologique}
#'   \item{type_eco}{Code du type écologique}
#'   \item{age}{Âge du peuplement (ans)}
#'   \item{is}{Indice de structure diamétrale de Shannon}
#'   \item{hd}{Hauteur dominante (m)}
#'   \item{ptot}{Précipitation totales annuelles (mm)}
#'   \item{t_ma}{Température annuelle moyenne  (C)}
#'   \item{altitude}{Altitude (m)}
#'   \item{nfi}{Densité marchande des feuillus intolérants (tiges/ha)}
#'   \item{nft}{Densité marchande des feuillus tolérants (tiges/ha)}
#'   \item{nrt}{Densité marchande des résineux tolérants (tiges/ha)}
#'   \item{nri}{Densité marchande des résineux intolérants (tiges/ha)}
#'   \item{nsab}{Densité marchande du sapin baumier (tiges/ha)}
#'   \item{stfi}{Surface terrière marchande des feuillus intolérants (m2/ha)}
#'   \item{stft}{Surface terrière marchande des feuillus tolérants (m2/ha)}
#'   \item{strt}{Surface terrière marchandedes résineux tolérants (m2/ha)}
#'   \item{stri}{Surface terrière marchandedes résineux intolérants (m2/ha)}
#'   \item{stsab}{Surface terrière marchande du sapin baumier (m2/ha)}
#'   \item{vfi}{Volume marchand des feuillus intolérants (m3/ha)}
#'   \item{vft}{Volume marchand des feuillus tolérants (m3/ha)}
#'   \item{vrt}{Volume marchand des résineux tolérants (m3/ha)}
#'   \item{vri}{Volume marchand des résineux intolérants (m3/ha)}
#'   \item{vsab}{Volume marchand du sapin baumier (m3/ha)}
#' }
#' @examples
#' fichier_compile
"fichier_compile"
