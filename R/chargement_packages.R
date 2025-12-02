#' Configure et charge l'environnement R avec renv
#' 
#' Cette fonction initialise ou restaure un environnement renv, installe les packages
#' manquants, met à jour le snapshot automatiquement, et charge tous les packages.
#' 
#' @param packages_requis Vecteur de noms de packages à installer et charger
#' 
#' @examples
#' chargement_packages(c("dplyr", "ggplot2", "data.table"))

chargement_packages <- function(packages_requis) {
  
  # Configuration du chemin de bibliothèque et désactivation des prompts interactifs
  .libPaths(c("/home/onyxia/work/userLibrary", .libPaths()))
  options(
    renv.config.sandbox.enabled = FALSE,
    ask = FALSE,
    install.packages.check.source = "no"
  )
  
  # Installation de renv si nécessaire
  if (!requireNamespace("renv", quietly = TRUE)) {
    cat("Installation de renv...\n")
    install.packages("renv", quiet = TRUE)
  }
  
  # Branche 1: Si renv.lock existe, on restaure et on complète
  if (file.exists("renv.lock")) {
    cat("Restauration de l'environnement renv...\n")
    
    sink("/dev/null")
    renv::restore(prompt = FALSE)
    sink()
    
    # Détection des packages manquants dans renv.lock
    cat("Vérification des nouveaux packages...\n")
    lock_content <- jsonlite::fromJSON("renv.lock")
    packages_in_lock <- names(lock_content$Packages)
    missing_from_lock <- setdiff(packages_requis, packages_in_lock)
    
    # Installation et enregistrement des packages manquants
    if (length(missing_from_lock) > 0) {
      cat("⚠️ Nouveaux packages détectés :", paste(missing_from_lock, collapse = ", "), "\n")
      cat("Installation automatique...\n")
      
      sink("/dev/null")
      renv::install(missing_from_lock, prompt = FALSE)
      sink()
      
      # Enregistrement explicite dans renv.lock
      cat("Enregistrement dans renv.lock...\n")
      for (pkg in missing_from_lock) {
        renv::record(pkg)
      }
      
      cat("✅ Packages enregistrés dans renv.lock\n")
    }
    
    # Branche 2: Si renv.lock n'existe pas, on initialise tout
  } else {
    cat("Initialisation du projet avec renv...\n")
    renv::init(bare = TRUE, restart = FALSE, 
               settings = list(snapshot.type = "implicit"))
    
    cat("Installation des packages requis...\n")
    sink("/dev/null")
    renv::install(packages_requis, prompt = FALSE)
    sink()
    
    # Enregistrement de tous les packages dans renv.lock
    cat("Enregistrement dans renv.lock...\n")
    for (pkg in packages_requis) {
      renv::record(pkg)
    }
  }
  
  # Vérification finale: tous les packages doivent être présents
  cat("Vérification de la présence de tous les packages nécessaires au projet...\n")
  installed_final <- rownames(installed.packages(lib.loc = renv::paths$library()))
  missing_final <- setdiff(packages_requis, installed_final)
  
  if (length(missing_final) > 0) {
    stop(
      "❌ Erreur : Packages manquants après installation : ",
      paste(missing_final, collapse = ", ")
    )
  }
  
  # Chargement des packages avec affichage des versions
  cat("Chargement des packages...\n")
  invisible(lapply(packages_requis, function(pkg) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    cat(sprintf("✓ %s, version: %s\n", pkg, packageVersion(pkg)))
  }))
  
  cat("✅ Environnement de travail prêt\n")
  
}
