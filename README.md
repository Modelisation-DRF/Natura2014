[![License: LGPL v3](https://img.shields.io/badge/License-LGPL%20v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0) [![R-CMD-check](https://github.com/Modelisation-DRF/Natura2014/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Modelisation-DRF/Natura2014/actions/workflows/R-CMD-check.yaml)

Le package Natura-2014
=======================

## Introduction

Le package Natura est une implémentation du modèle de croissance Natura-2014 dans le langage R.

## Copyright 

(c) 2024 Ministère des Ressources naturelles et des Forêts du Québec  

## Licence

Ce package est publié avec une licence Lesser General Public License (LGPL-3). 

## Dépendences
Ce package dépend des packages OutilsDRF et ExtractMap.

OutilsDRF est disponible ici: https://github.com/Modelisation-DRF/OutilsDRF

ExtractMap est disponible ici: https://github.com/Modelisation-DRF/ExtractMAp

## Comment l'utiliser

Le package peut être installé à l'aide du package remotes:

~~~R
library(remotes)
remotes::install_github("Modelisation-DRF/Natura2014")
~~~

## Historique des versions

| Date |  Version  | Features et bugs | Détails |
|:-----|:---------:|:-----------------|:--------|
| 2025-10-24 | 2.2.5 |  | Mettre les tables arbre, étude et compile en data.frame dès le départ |
| 2025-09-22 | 2.2.4 |  | Simulation n'arrête plus pour valeurs hors limite et placettes non simulées ajoutées au fichier des simulations |
| 2025-09-19 | 2.2.3 |  | Ménage dans Imports et Depends et ajout package.R pour lister les @importFrom |
| 2025-05-09 | 2.2.2 |  | Changer le package TarifQC pour OutilsDRF et renommer le package |
| 2025-01-16 | 2.2.1 |  | Correction de la fonction genere_graphique |
| 2024-08-22 | 2.2.0 |  | Modifications pour fonctionner comme Natura3 et ajout de validations pour la shiny et renommer fct principale pour eviter confusion avec Natura3 |
| 2024-06-14 | 2.1.0 |  | Ajout de l'utilisation du package ExtractMap pour climat |
| 2024-06-11 | 2.0.1 |  | Fixing issues in package evaluation, modif description |
| 2024-01-25 | 2.0.0 |  | Première version stable du modèle en R |

