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
#'  "fi" (feuillus intolerants),
#'  "ft" (feuillus tolerants),
#'  "ri" (resineux intolerants),
#'  "rt" (resineux tolerants).
#'  "sab" (sapin baumier),
#'  La valeur par defaut est "tot".
#' @param Variable Une chaine de caracteres specifiant la variable a tracer.
#' Les valeurs possibles incluent
#'  "st" (surface terriere marchande en m2/ha),
#'  "n" (nombre d'arbres marchands par ha), "
#'  "v" (volume marchand en m3/ha),
#'  "hd" (hauteur dominante en m),
#'  "dq" (diametre quadratique moyen en cm).
#'  La valeur par defaut est "st".
#'
#' @return Un objet ggplot representant le graphique d'evolution de la variable
#' specifiee sur la periode d'annee
#'
#' @export
#'
#' @import data.table
#'
generer_graphique <- function(Data, Espece = "tot", Variable = "st") {

  # Data=data_simul; Espece = "tot"; Variable = "dq";

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
      var <- "hd"  # on change hdtot pour hd
      Etiquette <- "Hauteur dominante (m)"
    },
    "dq" = {
      Etiquette <- "Diametre quadratique moyen (cm)"
    }
  )


  if (!is.numeric(Data[[var]])) {
    Data[[var]] <- 0
  }

  # ceci était fait dans natura3 pour faire la moyenne quand la simulation était stochastique, mais natura-2014 n'est pas stochastique, donc pas utile
  # et si le fihcier est gros, on prend du temps pour rien
  Data <- Data %>%
    mutate(mean_value = get(var))
    #group_by(placette, annee) %>%
    #summarise(mean_value = mean(get(var), na.rm = TRUE), .groups = "drop") %>%
    #ungroup()

  anneeMin <- min(Data$age, na.rm = TRUE)
  anneeMax <- max(Data$age, na.rm = TRUE)

  # Calculate ymax only if there are valid numeric values
  if (any(!is.na(Data$mean_value))) {
    ymax <- max(Data$mean_value, na.rm = TRUE)
  } else {
    ymax <- NA
    warning("No valid numeric values found for mean_value.")
  }

  Essence <- switch(Espece,
    "tot" = "Toutes essences",
    "fi" = "Feuillus intolerants",
    "ft" = "Feuillus tolerants",
    "ri" = "Resineux intolerants",
    "rt" = "Resineux tolerants",
    "sab" = "Sapin baumier",
    "Toutes essences"
  )

  # je ne veux pas faire affichier le nopm de placettes sur le graphique
  # dernieres_valeurs <- Data %>%
  #   group_by(placette) %>%
  #   slice(n()) %>%
  #   ungroup()
  # en data.table


  GraphEvol <- ggplot(data = Data, aes(x = age, y = mean_value, group = placette, label = placette)) +
    geom_line(show.legend = FALSE, lwd = 1.25, colour = "#008000") +
    ylim(0, ifelse(is.na(ymax), 5, ymax + 5)) +
    xlab(bquote(bold("Age moyen du peuplement (ans)"))) +
    ylab(Etiquette) +
    scale_x_continuous(breaks = seq(anneeMin, anneeMax, by = 5)) +
    ggtitle(paste(Etiquette, "  ", Essence)) +
    theme_bw() +
    theme(
      strip.background = element_rect(fill = "white"),
      axis.title = element_text(size = 14, face = "bold"),
      strip.text.x = element_text(size = 12, face = "bold"),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
    ) #+
    #geom_text(data = dernieres_valeurs, aes(label = placette), hjust = 1, vjust = -0.2, size = 3)

  return(GraphEvol)
}
