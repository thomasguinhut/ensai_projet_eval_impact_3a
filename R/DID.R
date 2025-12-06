

# Calculer la moyenne pour la base TRAITEMENT et ajouter une étiquette
traite_agrege <- traites %>%
  group_by(date) %>%
  summarise(
    moyenne_prix_total = mean(nb_ventes_apt_maison, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(Base = "Traitement")

# Calculer la moyenne pour la base CONTRÔLE et ajouter une étiquette
controle_agrege <- controle %>%
  group_by(date) %>%
  summarise(
    moyenne_prix_total = mean(nb_ventes_apt_maison, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(Base = "Contrôle")

# Combiner les deux résultats en une seule base de données
donnees_combinees <- bind_rows(traite_agrege, controle_agrege)

# Convertir la colonne 'date' en format Date pour une visualisation correcte (ajout du 1er jour)
donnees_combinees$date_format <- as.Date(paste0(donnees_combinees$date, "-01"))




graph_combine_prix_moyen <- donnees_combinees %>%
  ggplot(aes(x = date_format, y = moyenne_prix_total, color = Base)) +
  
  # Ligne de tendance, couleur différenciée par la variable 'Base'
  geom_line(linewidth = 1) + 
  
  # Points pour marquer chaque observation mensuelle
  geom_point(size = 3) +
  
  # Titres et étiquettes
  labs(
    title = "Évolution du nombre de ventes (Traitement vs. Contrôle)",
    subtitle = "Nombre de venets de maison ou appartement ",
    x = "Date",
    y = "Nombre de vente NON harmonisé",
    color = "Base de données" # Titre de la légende de couleur
  ) +
  
  # Mise en forme
  theme_minimal() +
  scale_y_continuous(labels = scales::label_number(big.mark = " ", suffix = " ventes")) + # Format des milliers
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotation des étiquettes X

# Afficher le graphique
print(graph_combine_prix_moyen)

