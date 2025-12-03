# source("R/A-preparation_donnees/A1-import_catnat.R")
# source("R/A-preparation_donnees/A2-import_dvf.R")

catnat <-
  aws.s3::s3read_using(
    FUN = readRDS,
    object = "projet_eval_impact/diffusion/donnees_nettoyees/catnat.rds",
    bucket = "thomasguinhut",
    opts = list("region" = "")
  )

dvf <-
  aws.s3::s3read_using(
    FUN = readRDS,
    object = "projet_eval_impact/diffusion/donnees_nettoyees/dvf.rds",
    bucket = "thomasguinhut",
    opts = list("region" = "")
  )

cat("✅ Bases de données catnat et dvf chargées\n")
