import_donnees <- function(catnat = FALSE, dvf = FALSE, cog = FALSE, cog2025 = FALSE, bdd = FALSE) {
  bucket <- "thomasguinhut"
  prefixe <- "diffusion/projet_eval_impact/donnees_nettoyees/"
  statuts_import <- c()
  
  # Fonction interne pour éviter la répétition
  importer <- function(nom, chemin) {
    tryCatch({
      base <- aws.s3::s3read_using(FUN = readRDS, object = chemin, bucket = bucket, opts = list("region" = ""))
      assign(nom, base, envir = .GlobalEnv)
      statuts_import[[nom]] <<- TRUE
      message("✅ ", nom, " chargée")
    }, error = function(e) {
      statuts_import[[nom]] <<- FALSE
      message("❌ ", nom, " : ", e$message)
    })
  }
  
  if (catnat) importer("catnat", paste0(prefixe, "catnat.rds"))
  if (dvf) importer("dvf", paste0(prefixe, "dvf.rds"))
  if (cog) importer("cog", paste0(prefixe, "cog.rds"))
  if (cog2025) importer("cog2025", paste0(prefixe, "cog2025.rds"))
  if (bdd) importer("bdd", paste0(prefixe, "bdd.rds"))

}
