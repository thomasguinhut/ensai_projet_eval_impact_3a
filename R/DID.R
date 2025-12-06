
#fonction pour aggreger les données

aggreger_donnees <- function(variable){
  
  traite_agrege <- traites %>%
    group_by(date) %>%
    summarise(
      moyenne = mean(.data[[variable]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Base = "Traitement")
  
  controle_agrege <- controle %>%
    group_by(date) %>%
    summarise(
      moyenne = mean(.data[[variable]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Base = "Contrôle")
  
  bind_rows(traite_agrege, controle_agrege) %>%
    mutate(date_format = as.Date(paste0(date, "-01")))
}


#fonction pour ploter les données

plot_comparatif <- function(donnees, variable, legende_y){
  
  titre_auto <- paste0("Évolution de ", variable, " (Traitement vs Contrôle)")
  sous_titre_auto <- "Moyenne mensuelle agrégée"
  
  donnees %>%
    ggplot(aes(x = date_format, y = moyenne, color = Base)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    labs(
      title = titre_auto,
      subtitle = sous_titre_auto,
      x = "Date",
      y = legende_y,
      color = "Base"
    ) +
    geom_vline(
      xintercept = as.Date("2025-03-01"),
      linetype = "dashed",
      color = "red",
      linewidth = 1
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


#fonction pour estimer l'effet du traitement 

modele_did <- function(variable){
  form <- as.formula(paste0(variable, " ~ time * herminia"))
  summary(lm(form, resultat))
}


#fonction pour faire les 3 d'un coup

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

analyse("prix_total_maison", "Prix total (€)")
analyse("nb_ventes_maison", "Nombre de ventes")
analyse("moy_prix_m2_maison", "Prix moyen au m² (€)")

analyse("prix_total_appartement", "Prix total (€)")
analyse("nb_ventes_appartement", "Nombre de ventes")
analyse("moy_prix_m2_appartement", "Prix moyen au m² (€)")

analyse("prix_total_apt_maison", "Prix total (€)")
analyse("nb_ventes_apt_maison", "Nombre de ventes")
analyse("moy_prix_m2_apt_maison", "Prix moyen au m² (€)")