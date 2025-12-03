chargement_packages <- function(packages_requis) {
  
  # Configuration du chemin de bibliothèque (adapté à l'environnement)
  if (dir.exists("/home/onyxia/work/userLibrary")) {
    .libPaths(c("/home/onyxia/work/userLibrary", .libPaths()))
  }
  
  options(
    renv.config.sandbox.enabled = FALSE,
    ask = FALSE,
    install.packages.check.source = "no"
  )
  
  # Fonction de suppression d'affichage multi-plateforme
  quiet_sink <- function() {
    if (.Platform$OS.type == "windows") {
      sink("NUL")
    } else {
      sink("/dev/null")
    }
  }
  
  # Installation de renv si nécessaire
  if (!requireNamespace("renv", quietly = TRUE)) {
    cat("Installation de renv...\n")
    install.packages("renv", quiet = TRUE)
  }
  
  # Branche 1: Si renv.lock existe
  if (file.exists("renv.lock")) {
    cat("Restauration de l'environnement renv...\n")
    
    quiet_sink()
    renv::restore(prompt = FALSE)
    sink()
    
    cat("Vérification des nouveaux packages...\n")
    lock_content <- jsonlite::fromJSON("renv.lock")
    packages_in_lock <- names(lock_content$Packages)
    missing_from_lock <- setdiff(packages_requis, packages_in_lock)
    
    if (length(missing_from_lock) > 0) {
      cat("⚠️ Nouveaux packages détectés :", paste(missing_from_lock, collapse = ", "), "\n")
      cat("Installation automatique...\n")
      
      quiet_sink()
      renv::install(missing_from_lock, prompt = FALSE)
      sink()
      
      cat("Enregistrement dans renv.lock...\n")
      for (pkg in missing_from_lock) {
        renv::record(pkg)
      }
      
      cat("✅ Packages enregistrés dans renv.lock\n")
    }
    
    # Branche 2: Si renv.lock n'existe pas
  } else {
    cat("Initialisation du projet avec renv...\n")
    renv::init(bare = TRUE, restart = FALSE, 
               settings = list(snapshot.type = "implicit"))
    
    cat("Installation des packages requis...\n")
    quiet_sink()
    renv::install(packages_requis, prompt = FALSE)
    sink()
    
    cat("Enregistrement dans renv.lock...\n")
    for (pkg in packages_requis) {
      renv::record(pkg)
    }
  }
  
  # Vérification finale
  cat("Vérification de la présence de tous les packages...\n")
  installed_final <- rownames(installed.packages(lib.loc = renv::paths$library()))
  missing_final <- setdiff(packages_requis, installed_final)
  
  if (length(missing_final) > 0) {
    stop(
      "❌ Erreur : Packages manquants : ",
      paste(missing_final, collapse = ", ")
    )
  }
  
  # Chargement des packages
  cat("Chargement des packages...\n")
  invisible(lapply(packages_requis, function(pkg) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    cat(sprintf("✓ %s, version: %s\n", pkg, packageVersion(pkg)))
  }))
  
  cat("✅ Environnement de travail prêt\n")
}
