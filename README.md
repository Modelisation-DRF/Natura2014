[![License: LGPL v3](https://img.shields.io/badge/License-LGPL%20v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0) [![R-CMD-check](https://github.com/Modelisation-DRF/RNatura2014/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Modelisation-DRF/RNatura2014/actions/workflows/R-CMD-check.yaml)

Le package RNatura-2014
=======================

## Introduction

Le package RNatura est une implémentation du modèle de croissance Natura-2014 dans le langage R.

## Copyright 

(c) 2024 Ministère des Ressources naturelles et des Forêts du Québec  

## Licence

Ce package est publié avec une licence Lesser General Public License (LGPL-3). 

## Comment l'utiliser

Le package peut être installé à l'aide du package remotes:

~~~R
library(remotes)
remotes::install_github("Modelisation-DRF/RNatura2014")
~~~

## Historique des versions

| Date |  Version  | Features et bugs | Détails |
|:-----|:---------:|:-----------------|:--------|
| 2025-01-16 | 2.2.1 |  | Correction de la fonction genere_graphique |
| 2024-08-22 | 2.2.0 |  | Modifications pour fonctionner comme Natura3 et ajout de validations pour la shiny et renommer fct principale pour eviter confusion avec Natura3 |
| 2024-06-14 | 2.1.0 |  | Ajout de l'utilisation du package ExtractMap pour climat |
| 2024-06-11 | 2.0.1 |  | Fixing issues in package evaluation, modif description |
| 2024-01-25 | 2.0.0 |  | Première version stable du modèle en R |
