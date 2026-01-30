import_donnees <- function(onyxia = FALSE, catnat = FALSE, dvf = FALSE, cog = FALSE, cog2025 = FALSE, bdd = FALSE, infos_locales = FALSE) {
  
  statuts_import <- c()
  
  # Configuration selon l'environnement
  if (onyxia) {
    bucket <- "thomasguinhut"
    prefixe <- "diffusion/projet_eval_impact/donnees_nettoyees/"
  } else {
    # Chemin local
    chemin_local <- getwd() #
  }
  
  # Fonction interne pour importer
  importer <- function(nom, chemin_relatif) {
    tryCatch({
      if (onyxia) {
        # Import depuis S3
        chemin_complet <- paste0(prefixe, chemin_relatif)
        base <- aws.s3::s3read_using(FUN = readRDS, object = chemin_complet, bucket = bucket, opts = list("region" = ""))
      } else {
        # Import depuis fichier local
        chemin_complet <- file.path(chemin_local, chemin_relatif)
        base <- readRDS(chemin_complet)
      }
      
      assign(nom, base, envir = .GlobalEnv)
      statuts_import[[nom]] <<- TRUE
      message("✅ ", nom, " chargée")
    }, error = function(e) {
      statuts_import[[nom]] <<- FALSE
      message("❌ ", nom, " : ", e$message)
    })
  }
  
  # Appels à la fonction importer selon les paramètres
  if (catnat) importer("catnat", "catnat.rds")
  if (dvf) importer("dvf", "dvf.rds")
  if (cog) importer("cog", "cog.rds")
  if (cog2025) importer("cog2025", "cog2025.rds")
  if (bdd) importer("bdd", "bdd.rds")
  if (infos_locales) importer("infos_locales", "infos_locales.rds")
  
  # Retourner les statuts
  return(invisible(statuts_import))
}