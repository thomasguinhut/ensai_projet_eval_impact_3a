# ----------------------------
# Création des groupes
# ----------------------------
did_herminia_test     <- bdd %>% filter(herminia == TRUE)
did_herminia_controle <- bdd %>% filter(herminia == FALSE)

# ----------------------------
# Fonction pour agréger les données mensuelles par groupe
# ----------------------------
did_aggreger_donnees <- function(variable){
  
  traite_agrege <- did_herminia_test %>%
    group_by(date) %>%
    summarise(
      moyenne = mean(.data[[variable]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Base = "Traitement")
  
  controle_agrege <- did_herminia_controle %>%
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
did_plot_comparatif <- function(donnees, variable, legende_y){
  
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
      date_breaks = "2 months",
      labels = function(x) {
        mois_fr <- c("janv.", "févr.", "mars", "avr.", "mai", "juin", 
                     "juil.", "août", "sept.", "oct.", "nov.", "déc.")
        paste(mois_fr[as.numeric(format(x, "%m"))], format(x, "%Y"))
      }
    ) + 
    scale_y_continuous(labels = scales::label_number(big.mark = " ")) + 
    geom_vline(
      xintercept = as.Date("2025-03-01"), 
      linetype = "dashed", 
      color = "black", 
      linewidth = 1
    ) + 
    theme_minimal() + 
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      legend.position = "right"
    )
}


# ----------------------------
# Fonction pour estimer l'effet du traitement (DiD)
# ----------------------------
did_modele <- function(variable){
  form <- as.formula(paste0(variable, " ~ time * herminia"))
  summary(lm(form, bdd))
}


# ----------------------------
# Fonction pour exécuter l'analyse complète (agrégation + plot + modèle)
# ----------------------------
did_analyse <- function(variable, legende_y){
  
  donnees <- did_aggreger_donnees(variable)
  
  graph <- did_plot_comparatif(
    donnees = donnees,
    variable = variable,
    legende_y = legende_y
  )
  
  did <- did_modele(variable)
  
  list(
    donnees_agregees = donnees,
    graphique = graph,
    modele_DiD = did
  )
}
