catnat %>% 
  filter(substr(dat_deb, 1, 4) > 2020) %>% 
  group_by(id) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

# On garde la catnat la plus importante de ces cinq dernières années : Herminia,
# qui a touché le sud-est de la Bretagne entre le 23 janvier et 5 février.

herminia <- catnat %>%
  filter(id == "INTE2503788A") %>% 
  pull(cod_commune)

bdd <- cog %>%
  filter(is.na(DATE_FIN)) %>% # on retrouve bien le même nombre de lignes que cog2025
  dplyr::select(COM, LIBELLE) %>% 
  mutate(HERMINIA = ifelse(COM %in% herminia, TRUE, FALSE))

length(unique(dvf$code_geo)) # != 1202

cog %>% 
  arrange(desc(DATE_FIN)) %>% 
  filter(year(DATE_FIN) > 2019)
# 12 communes ont disparu entre 2020 et 2025. C'est important d'en tenir compte
# pour corriger la base DVF.

dvf_cog2025 <- dvf %>% 
  mutate(code_geo = case_when(
    code_geo == "22027" ~ "22241",
    code_geo == "22043" ~ "22241",
    code_geo == "22200" ~ "22237",
    code_geo == "22309" ~ "22147",
    code_geo == "35112" ~ "35062",
    code_geo == "56049" ~ "56213",
    .default = code_geo
    ),
         libelle_geo = case_when(
    code_geo == "22027" ~ "Plumieux",
    code_geo == "22043" ~ "Plumieux",
    code_geo %in% c("22200", "22237") ~ "Val-d'Arguenon",
    code_geo == "22309" ~ "Merdrignac",
    code_geo == "22084" ~ "Jugon-les-Lacs",
    code_geo %in% c("35062", "35112") ~ "La Chapelle-Fleurigné",
    code_geo == "56262" ~ "Le Bono",
    code_geo == "35329" ~ "Sougeal",
    code_geo %in% c("56049", "56213") ~ "Saint-Gérand-Croixanvec",
    .default = libelle_geo
    )
  )

length(unique(dvf_cog2025$code_geo)) # != 1202
# Peut-être parce que dans deux communes il n'y a eu aucune transaction ?

cog2025 %>% 
  filter(!(COM %in% dvf_cog2025$code_geo))
# A Île-de-Sein et à Île-Molène, il n'y a eu aucune transaction sur la période

dvf_cog2025_herminia <- dvf_cog2025 %>%
  filter(annee_mois >= as.Date("2024-08-01") & annee_mois <= as.Date("2025-06-30"))

cog2025 %>% 
  filter(!(COM %in% dvf_cog2025_herminia$code_geo))
# Finalement, 17 communes bretonnes n'ont pas eu de transactions entre août 2024
# et juin 2025

# Créer une séquence complète de mois pour la période couverte par dvf_cog2025_herminia
mois_complets <- dvf_cog2025_herminia %>%
  distinct(annee_mois) %>%
  arrange(annee_mois)

# Créer le produit cartésien : toutes les communes × tous les mois
bdd_complete <- bdd %>%
  crossing(mois_complets)

# Effectuer la jointure avec les données DVF
resultat <- bdd_complete %>%
  left_join(
    dvf_cog2025_herminia %>%
      select(-libelle_geo),  # On garde libelle_geo de bdd (nommé LIBELLE)
    by = c("COM" = "code_geo", "annee_mois")
  ) %>%
  # Réorganiser les colonnes pour plus de clarté
  select(
    code_geo = COM,
    libelle_geo = LIBELLE,
    herminia = HERMINIA,
    annee_mois,
    everything()
  ) %>%
  arrange(code_geo, annee_mois)




dvf %>%
  filter(code_geo == "35126") %>%
  arrange(annee_mois) %>%
  ggplot(aes(x = annee_mois)) +
  geom_line(aes(y = moy_prix_m2_maison, color = "Prix moyen"), size = 1) +
  geom_line(aes(y = med_prix_m2_maison, color = "Prix médian"), size = 1) +
  labs(
    title = "Évolution des prix au m² des maisons (Allineuc)",
    x = "Date",
    y = "Prix au m² (€)",
    color = "Type de prix"
  ) +
  theme_minimal()






dvf <- dvf %>%
  mutate(
    annee_mois = as.Date(annee_mois),
    date = format(annee_mois, "%Y-%m")
  )
base_jointe<- full_join(catnat, dvf, 
                        by = c("cod_commune" = "code_geo", "date" = "date"))
base_econometrie <- base_jointe %>%
  filter(dat_fin >= as.Date("2020-01-01"))



plot(dvf %>%
  group_by(annee_mois) %>%
  summarise(moyenne_ventes = mean(moy_prix_m2_appartement, na.rm = TRUE)))
