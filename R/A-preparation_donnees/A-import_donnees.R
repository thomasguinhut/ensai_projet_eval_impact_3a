# source("R/A-preparation_donnees/A1-import_catnat.R")
# source("R/A-preparation_donnees/A2-import_dvf.R")
# source("R/A-preparation_donnees/A3-import_cog.R")

catnat <-
  aws.s3::s3read_using(
    FUN = readRDS,
    object = "diffusion/projet_eval_impact/donnees_nettoyees/catnat.rds",
    bucket = "thomasguinhut",
    opts = list("region" = "")
  )

dvf <-
  aws.s3::s3read_using(
    FUN = readRDS,
    object = "diffusion/projet_eval_impact/donnees_nettoyees/diffusion/dvf.rds",
    bucket = "thomasguinhut",
    opts = list("region" = "")
  )

cog <-
  aws.s3::s3read_using(
    FUN = readRDS,
    object = "diffusion/projet_eval_impact/donnees_nettoyees/diffusion/cog.rds",
    bucket = "thomasguinhut",
    opts = list("region" = "")
  )

cog2025 <-
  aws.s3::s3read_using(
    FUN = readRDS,
    object = "diffusion/projet_eval_impact/donnees_nettoyees/diffusion/cog2025.rds",
    bucket = "thomasguinhut",
    opts = list("region" = "")
  )

cat("✅ Bases de données catnat, dvf et cog chargées\n")
