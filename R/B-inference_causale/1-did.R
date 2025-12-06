# ----------------------------
# Création des groupes
# ----------------------------
herminia_test     <- bdd %>% filter(herminia == TRUE)
herminia_controle <- bdd %>% filter(herminia == FALSE)

# ----------------------------
# Fonction pour agréger les données mensuelles par groupe
# ----------------------------
aggreger_donnees <- function(variable){
  
  traite_agrege <- herminia_test %>%
    group_by(date) %>%
    summarise(
      moyenne = mean(.data[[variable]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Base = "Traitement")
  
  controle_agrege <- herminia_controle %>%
    group_by(date) %>%
    summarise(
      moyenne = mean(.data[[variable]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Base = "Contrôle")
  
  bind_rows(traite_agrege, controle_agrege) %>%
    mutate(date_format = as.Date(paste0(date, "-01")))
}


# ----------------------------
# Fonction pour visualiser la comparaison entre groupes
# ----------------------------
plot_comparatif <- function(donnees, variable, legende_y){
  
  titre_auto <- paste0("Évolution de ", variable, " (Traitement vs Contrôle)")
  sous_titre_auto <- "Moyenne mensuelle agrégée"
  
  donnees %>%
    ggplot(aes(x = date_format, y = moyenne, color = Base)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    labs(
      x = "\nDate",
      y = paste0(legende_y, "\n"),
      color = "Groupe"
    ) +
    scale_x_date(
      date_labels = "%b %Y",  # Affiche "Jan 2024", "Feb 2024", etc.
      date_breaks = "2 months" # Espace les labels tous les 3 mois
    ) +
    scale_y_continuous(labels = scales::label_number(big.mark = " ")) +
    geom_vline(
      xintercept = as.Date("2025-03-01"),
      linetype = "dashed",
      color = "red",
      linewidth = 1
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotation et taille
      plot.title = element_blank(),  # Supprime le titre
      plot.subtitle = element_blank(),  # Supprime le sous-titre
      legend.position = "right"  # Légende à droite (optionnel)
    )
  
}


# ----------------------------
# Fonction pour estimer l'effet du traitement (DiD)
# ----------------------------
modele_did <- function(variable){
  form <- as.formula(paste0(variable, " ~ time * herminia"))
  summary(lm(form, bdd))
}


# ----------------------------
# Fonction pour exécuter l'analyse complète (agrégation + plot + modèle)
# ----------------------------
analyse <- function(variable, legende_y){
  
  donnees <- aggreger_donnees(variable)
  
  graph <- plot_comparatif(
    donnees = donnees,
    variable = variable,
    legende_y = legende_y
  )
  
  did <- modele_did(variable)
  
  list(
    donnees_agregees = donnees,
    graphique = graph,
    modele_DiD = did
  )
}

# ----------------------------
# Liste des variables à analyser
# ----------------------------
analyse("prix_total_maison", "Valeur totale des ventes de maisons (€)")
analyse("nb_ventes_maison", "Nombre moyen de ventes de maisons")
analyse("moy_prix_m2_maison", "Prix moyen au m² des maisons (€)")

analyse("prix_total_appartement", "Valeur totale des ventes d'appartements (€)")
analyse("nb_ventes_appartement", "Nombre de ventes d'appartements")
analyse("moy_prix_m2_appartement", "Prix moyen au m² des appartements (€)")

analyse("prix_total_apt_maison", "Valeur totale des ventes d'appartements et de maisons (€)")
analyse("nb_ventes_apt_maison", "Nombre de ventes des appartements et maisons")
analyse("moy_prix_m2_apt_maison", "Prix moyen au m² des appartements et maisons (€)")
