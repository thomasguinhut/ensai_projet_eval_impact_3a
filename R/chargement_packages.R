chargement_packages <- function(packages_requis, packages_onyxia = NULL) {
  
  # Configuration du chemin de bibliothèque
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
  
  # Vérification si renv.lock ou renv/ sont absents et initialisation
  if (!file.exists("renv.lock") || !dir.exists("renv")) {
    cat("Réinitialisation du projet avec renv...\n")
    renv::init(bare = TRUE, restart = FALSE, settings = list(snapshot.type = "implicit"))
    
    # Installation des packages requis, y compris les packages Onyxia
    cat("Installation des packages requis...\n")
    quiet_sink()
    renv::install(packages_requis, prompt = FALSE)
    if (!is.null(packages_onyxia)) {
      renv::install(packages_onyxia, prompt = FALSE)  # Installation forcée des packages spécifiques Onyxia
    }
    sink()
    
    # Création du snapshot (lockfile) après installation
    cat("Création du renv.lock...\n")
    renv::snapshot(prompt = FALSE, type = "implicit")
    
  } else {
    # Si renv.lock existe déjà, on restaure l'environnement
    cat("Restauration de l'environnement renv...\n")
    quiet_sink()
    renv::restore(prompt = FALSE)
    sink()
  }
  
  # Vérification des packages manquants
  installed_final <- rownames(installed.packages(lib.loc = renv::paths$library()))
  missing_final <- setdiff(packages_requis, installed_final)
  
  if (length(missing_final) > 0) {
    cat("Installation des packages manquants :", paste(missing_final, collapse = ", "), "\n")
    quiet_sink()
    renv::install(missing_final, prompt = FALSE)
    sink()
    
    # Mise à jour du lockfile après installation des packages manquants
    cat("Mise à jour de renv.lock...\n")
    renv::snapshot(prompt = FALSE, type = "implicit")
  }
  
  # Installation forcée des packages Onyxia, si nécessaire
  if (!is.null(packages_onyxia) && length(packages_onyxia) > 0) {
    cat("Installation des packages spécifiques Onyxia...\n")
    quiet_sink()
    renv::install(packages_onyxia, prompt = FALSE)
    sink()
    
    # Mise à jour du lockfile après installation des packages Onyxia
    cat("Mise à jour de renv.lock...\n")
    renv::snapshot(prompt = FALSE, type = "implicit")
  }
  
  # Vérification finale de la présence de tous les packages
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
