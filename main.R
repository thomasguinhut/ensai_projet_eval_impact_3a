################################################################################
############################ CHARGEMENT DES PACKAGES ###########################
################################################################################



# Définition des packages
packages_requis <- c("aws.s3", "dplyr", "lubridate", "stringr", "tidyr",
                     "ggplot2", "scales", "MatchIt", "cobalt", "plm")

# Chargement de pacman
if (!"pacman" %in% installed.packages()) {
  install.packages("pacman")
}
library(pacman)

# Chargement via le vecteur
pacman::p_load(char = packages_requis)



################################################################################
############################ IMPORTATION DES DONNÉES ###########################
################################################################################


# source("R/A-preparation_donnees/A1-import_catnat.R")
# source("R/A-preparation_donnees/A2-import_dvf.R")
# source("R/A-preparation_donnees/A3-import_cog.R")

source("R/A-preparation_donnees/A-import_donnees.R")

# import_donnees(
#   catnat = TRUE,
#   dvf = TRUE,
#   cog = TRUE,
#   cog2025 = TRUE,
#   bdd = FALSE
# )

# source("R/A-preparation_donnees/A4-jointure.R")

import_donnees(
  catnat = FALSE,
  dvf = FALSE,
  cog = FALSE,
  cog2025 = FALSE,
  bdd = TRUE
)



################################################################################
############################ DOUBLES DIFFERENCES ###############################
################################################################################


source("R/B-inference_causale/1-did.R")

did_analyse("prix_total_maison", "Valeur totale des ventes de maisons (€)")
did_analyse("nb_ventes_maison", "Nombre moyen de ventes de maisons")
did_analyse("moy_prix_m2_maison", "Prix moyen au m² des maisons (€)")

did_analyse("prix_total_appartement", "Valeur totale des ventes d'appartements (€)")
did_analyse("nb_ventes_appartement", "Nombre de ventes d'appartements")
did_analyse("moy_prix_m2_appartement", "Prix moyen au m² des appartements (€)")

did_analyse("prix_total_apt_maison", "Valeur totale des ventes d'appartements et de maisons (€)")
did_analyse("nb_ventes_apt_maison", "Nombre de ventes des appartements et maisons")
did_analyse("moy_prix_m2_apt_maison", "Prix moyen au m² des appartements et maisons (€)")
