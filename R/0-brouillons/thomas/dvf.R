dvf_1 <-
  aws.s3::s3read_using(
    FUN = read.csv2,
    object = "diffusion/projet_eval_impact/sources/stats_dvf.csv",
    bucket = "thomasguinhut",
    sep = ",",
    opts = list("region" = "")
  )

glimpse(dvf_1)

dvf_2 <- dvf_1 %>% 
  filter(echelle_geo == "commune",
         substr(code_geo, 1, 2) %in% c("22", "29", "35", "56")) %>% 
  dplyr::select(code_geo, libelle_geo, annee_mois, everything(), -c("code_parent", "echelle_geo")) %>% 
  mutate(annee_mois = as.Date(paste0(annee_mois, "-01"), format = "%Y-%m-%d"))

glimpse(dvf_2)




# Liste des départements bretons
departements_bretons <- c("22", "29", "35", "56")
# 22 = Côtes-d'Armor, 29 = Finistère, 35 = Ille-et-Vilaine, 56 = Morbihan

# Années à récupérer
annees <- 2020:2025

# Initialisation d'une liste vide pour stocker les données
toutes_les_donnees <- list()

# Boucle sur les départements et les années
for (departement in departements_bretons) {
  for (annee in annees) {
    # Construire l'URL pour chaque fichier
    url <- paste0("https://files.data.gouv.fr/geo-dvf/latest/csv/", annee, "/departements/", departement, ".csv.gz")
    
    # Charger les données avec fread
    data <- fread(url, sep = ",", encoding = "UTF-8")
    
    # Ajouter les données dans la liste
    toutes_les_donnees[[paste(departement, annee, sep = "_")]] <- data
  }
}

# Combiner toutes les bases en une seule
base_finale <- rbindlist(toutes_les_donnees, use.names = TRUE, fill = TRUE)
