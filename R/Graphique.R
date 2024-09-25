#' Graphique d'evolution des variables forestieres
#'
#' @description Cette fonction genere un graphique d'evolution pour differentes
#' variables forestieres sur une periode d'annee donnee, en fonction des
#' especes et des placettes specifiees.
#'
#' @param Data Un data.frame contenant les donnees forestieres. Le data.frame
#' doit inclure au moins les colonnes suivantes :
#'  placette (identifiant de la placette),
#'  annee,
#'  et les variables a tracer (par exemple, sttot, stbop, etc.).
#' @param Espece Une chaine de caracteres specifiant l'espece d'arbre pour
#' laquelle generer le graphique. Les valeurs possibles incluent
#'  "tot" (toutes essences),
#'  "bop" (bouleau a papier),
#'  "peu" (peupliers),
#'  "ft" (feuillus tolerants),
#'  "sab" (sapin baumier),
#'  "epn" (epinette noire),
#'  "epx" (autres epinettes),
#'  "ri" (resineux intolerants),
#'  "rt" (autres resineux tolerants).
#'  La valeur par defaut est "tot".
#' @param Variable Une chaine de caracteres specifiant la variable a tracer.
#' Les valeurs possibles incluent
#'  "st" (surface terriere marchande en m2/ha),
#'  "n" (nombre d'arbres marchands par ha), "
#'  v" (volume marchand en m3/ha),
#'  "hd" (hauteur dominante en m),
#'  "dq" (diametre quadratique moyen en cm).
#'  La valeur par defaut est "st".
#' @param listePlacette Une liste ou un vecteur contenant les identifiants des
#' placettes a inclure dans le graphique.
#'
#' @return Un objet ggplot representant le graphique d'evolution de la variable
#' specifiee sur la periode d'annee
#'
#' @export
generer_graphique <- function(Data, Espece = "tot", Variable = "st", listePlacette) {
  var <- paste0(paste(Variable), paste(Espece))

  # Ensure the variable column exists and is numeric
  switch(Variable,
    "st" = {
      Etiquette <- "Surface terriere marchande (m2/ha)"
    },
    "n" = {
      Etiquette <- "Nombre d'arbres marchands par ha"
    },
    "v" = {
      Etiquette <- "Volume marchand (m3/ha)"
    },
    "hd" = {
      var <- "hd"
      Etiquette <- "Hauteur dominante (m)"
    },
    "dq" = {
      Etiquette <- "Diametre quadratique moyen (cm)"
    }
  )


  if (!is.numeric(Data[[var]])) {
    Data[[var]] <- 0
  }

  Data <- Data %>%
    group_by(placette, annee) %>%
    summarise(mean_value = mean(get(var), na.rm = TRUE), .groups = "drop") %>%
    ungroup()

  anneeMin <- min(Data$annee, na.rm = TRUE)
  anneeMax <- max(Data$annee, na.rm = TRUE)

  # Calculate ymax only if there are valid numeric values
  if (any(!is.na(Data$mean_value))) {
    ymax <- max(Data$mean_value, na.rm = TRUE)
  } else {
    ymax <- NA
    warning("No valid numeric values found for mean_value.")
  }

  Essence <- switch(Espece,
    "tot" = "Toutes essences",
    "bop" = "Bouleau a papier",
    "peu" = "Peupliers",
    "ft" = "Feuillus tolerants",
    "sab" = "Sapin baumier",
    "epn" = "epinette noire",
    "epx" = "Autres epinettes",
    "ri" = "Resineux intolerants",
    "rt" = "Autres resineux tolerants",
    "Toutes essences"
  )

  dernieres_valeurs <- Data %>%
    group_by(placette) %>%
    slice(n()) %>%
    ungroup()

  GraphEvol <- ggplot(data = Data, aes(x = annee, y = mean_value, group = placette, label = placette)) +
    geom_line(show.legend = FALSE, lwd = 1.25, colour = "#008000") +
    ylim(0, ifelse(is.na(ymax), 5, ymax + 5)) +
    xlab(bquote(bold("Annees depuis la perturbation"))) +
    ylab(Etiquette) +
    scale_x_continuous(breaks = seq(anneeMin, anneeMax, by = 5)) +
    ggtitle(paste(Etiquette, "  ", Essence)) +
    theme_bw() +
    theme(
      strip.background = element_rect(fill = "white"),
      axis.title = element_text(size = 14, face = "bold"),
      strip.text.x = element_text(size = 12, face = "bold"),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
    ) +
    geom_text(data = dernieres_valeurs, aes(label = placette), hjust = 1, vjust = -0.2, size = 3)

  return(GraphEvol)
}
