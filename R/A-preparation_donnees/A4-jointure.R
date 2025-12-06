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

resultat <- resultat %>% 
  rename(
    date = annee_mois
  )

resultat <- resultat %>%
  mutate(
    across(
      c(nb_ventes_maison, nb_ventes_appartement, nb_ventes_local, nb_ventes_apt_maison), # Liste des colonnes ciblées
      ~ replace_na(.x, 0)                  # La fonction ~ remplace les NA par 0
    )
  ) %>% select(,-med_prix_m2_appartement, -med_prix_m2_maison,
               -med_prix_m2_local, -med_prix_m2_apt_maison)


#il n'y a pas un nombre d'observations multiple de 11 on regèle cela
verification_villes <- resultat %>%
  group_by(libelle_geo) %>%
  summarise(# n() compte le nombre de lignes dans chaque groupe
  ) %>%
  ungroup() 

villes_anormales <- verification_villes %>%
  filter(nombre_observations != 11)
#deux villes ont plusieurs valeurs dûes à des fusions. 
#Onfait la moyenne des deux. 
villes_a_agreger <- c("Plumieux", "Val-d'Arguenon")
lignes_a_agreger <- resultat %>%
  filter(libelle_geo %in% villes_a_agreger)

resultat_cibles_agrege <- lignes_a_agreger %>%
  group_by(libelle_geo, date) %>%
  summarise(
    # Moyenne pour les colonnes numériques
    nb_ventes_maison = mean(nb_ventes_maison, na.rm = TRUE),
    moy_prix_m2_maison = mean(moy_prix_m2_maison, na.rm = TRUE),
    
    # Conservation de la première valeur pour les autres colonnes (assumées constantes)
    code_geo = first(code_geo),
    herminia = first(herminia),
    
    .groups = 'drop' 
  )
resultat_autres_villes <- resultat %>%
  filter(!libelle_geo %in% villes_a_agreger)
resultat <- bind_rows(resultat_autres_villes, resultat_cibles_agrege)

#On calcule le prix total par mois par commune
resultat$prix_total_maison<-resultat$nb_ventes_maison*resultat$moy_prix_m2_maison
resultat <- resultat %>% relocate(prix_total_maison, .after = 6)

resultat$prix_total_appartement<-resultat$nb_ventes_appartement*resultat$moy_prix_m2_appartement
resultat <- resultat %>% relocate(prix_total_appartement, .after = 9)

resultat$prix_total_local<-resultat$nb_ventes_local*resultat$moy_prix_m2_local
resultat <- resultat %>% relocate(prix_total_local, .after = 12)

resultat$prix_total_apt_maison<-resultat$nb_ventes_apt_maison*resultat$moy_prix_m2_apt_maison
resultat <- resultat %>% relocate(prix_total_apt_maison, .after = 15)

date_intervention_str <- "2025-02"

resultat <- resultat %>%
  mutate(
    time = case_when(
      date > date_intervention_str ~ 1, 
      TRUE ~ 0
    ) 
  )

date_intervention_str <- "2025-03"

resultat <- resultat %>%
  mutate(
    date_format = as.Date(paste0(date, "-01")), 
    time = case_when(
      date_format > as.Date(paste0(date_intervention_str, "-01")) ~ 1, 
      TRUE ~ 0
    )
  ) %>%
  select(-date_format) %>%
  relocate(time, .after = 4)


traites<-resultat %>% filter(herminia==TRUE)
controle<-resultat %>% filter(herminia==FALSE)
#94*11=1034 on est bon. 





