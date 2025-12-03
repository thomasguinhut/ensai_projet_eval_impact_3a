################################################################################
############################ CHARGEMENT DES PACKAGES ###########################
################################################################################

# Importation de la fonction
source("R/chargement_packages.R")

# Définition des packages
packages_requis <- c("aws.s3", "dplyr", "lubridate", "stringr", "tidyr")

chargement_packages(packages_requis)


################################################################################
############################ IMPORTATION DES DONNÉES ###########################
################################################################################

source("R/A-preparation_donnees/A-import_donnees.R")
