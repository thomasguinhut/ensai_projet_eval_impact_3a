# source("R/A-preparation_donnees/A1-import_catnat.R")

catnat <-
  aws.s3::s3read_using(
    FUN = readRDS,
    object = "projet_eval_impact/donnees_nettoyees/catnat.rds",
    bucket = "thomasguinhut",
    opts = list("region" = "")
  )
