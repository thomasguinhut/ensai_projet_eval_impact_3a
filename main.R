################################################################################
############################ CHARGEMENT DES PACKAGES ###########################
################################################################################

# Importation de la fonction
source("R/chargement_packages.R")

# Définition des packages
packages_requis <- c("aws.s3", "dplyr", "lubridate", "stringr", "tidyr",
                     "ggplot2", "scales")

chargement_packages(packages_requis)


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
