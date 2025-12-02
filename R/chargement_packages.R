chargement_packages <- function(packages_requis) {
  
  # Configuration de base
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
  
  # Consentement pour éviter les questions interactives
  renv::consent(provided = TRUE)
  
  # --- Branche 1 : Initialisation si renv.lock n'existe pas ---
  if (!file.exists("renv.lock")) {
    cat("Initialisation du projet avec renv...\n")
    renv::init(bare = TRUE, restart = FALSE, settings = list(snapshot.type = "implicit"))
    
    cat("Installation des packages requis...\n")
    sink("/dev/null")
    renv::install(packages_requis, prompt = FALSE)
    sink()
    
    cat("Création du lockfile...\n")
    renv::snapshot(prompt = FALSE)
  }
  # --- Branche 2 : Restauration si renv.lock existe ---
  else {
    cat("Restauration de l'environnement renv...\n")
    sink("/dev/null")
    renv::restore(prompt = FALSE)
    sink()
  }
  
  # --- Vérification des packages manquants (APRÈS restauration) ---
  installed <- rownames(installed.packages(lib.loc = renv::paths$library()))
  missing <- setdiff(packages_requis, installed)
  
  if (length(missing) > 0) {
    cat("⚠️ Packages manquants détectés :", paste(missing, collapse = ", "), "\n")
    cat("Installation automatique...\n")
    sink("/dev/null")
    renv::install(missing, prompt = FALSE)
    sink()
    
    cat("Mise à jour du lockfile...\n")
    renv::snapshot(prompt = FALSE)
  }
  
  # --- Chargement des packages ---
  cat("Chargement des packages...\n")
  invisible(lapply(packages_requis, function(pkg) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    cat(sprintf("✓ %s, version: %s\n", pkg, packageVersion(pkg)))
  }))
  
  cat("✅ Environnement de travail prêt\n")
}
