################################################################################
################################## Nettoyage ###################################
################################################################################


objets_initiaux <- ls()

catnat %>% 
  filter(substr(dat_deb, 1, 4) > 2020) %>% 
  group_by(id) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

# On garde la catnat la plus importante de ces cinq dernières années : Herminia,
# qui a touché le sud-est de la Bretagne entre le 23 janvier et 5 février 2025.

herminia <- catnat %>%
  filter(id == "INTE2503788A") %>% 
  pull(cod_commune)

communes <- cog %>%
  filter(is.na(DATE_FIN)) %>% # on retrouve bien le même nombre de lignes que cog2025
  dplyr::select(COM, LIBELLE) %>% 
  mutate(HERMINIA = ifelse(COM %in% herminia, TRUE, FALSE))

length(unique(dvf$code_geo)) # != 1202

cog %>% 
  arrange(desc(DATE_FIN)) %>% 
  filter(year(DATE_FIN) > 2019)
# 12 communes ont disparu entre 2020 et 2025. C'est important d'en tenir compte
# pour corriger la base DVF. Il faut exprimer cette base dans le COG 2025.

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
# En effet, à Île-de-Sein et Île-Molène, il n'y a eu aucune transaction sur la période

# On se restreint à :
date_deb <- as.Date("2023-08-01")
date_fin <- as.Date("2025-06-30")

dvf_cog2025_herminia <- dvf_cog2025 %>%
  filter(annee_mois >= date_deb & annee_mois <= date_fin)

cog2025 %>% 
  filter(!(COM %in% dvf_cog2025_herminia$code_geo))
# Finalement, 3 communes bretonnes n'ont pas eu de transactions entre août 2023
# et juin 2025

# Créer une séquence complète de mois pour la période couverte par dvf_cog2025_herminia
mois_complets <- dvf_cog2025_herminia %>%
  distinct(annee_mois) %>%
  arrange(annee_mois)

# Créer le produit cartésien : toutes les communes × tous les mois + jointure
bdd_1 <- communes %>%
  crossing(mois_complets) %>% 
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
    date = annee_mois,
    everything()
  ) %>%
  arrange(code_geo, date) %>% 
  mutate(
    across(
      c(nb_ventes_maison, nb_ventes_appartement, nb_ventes_local, nb_ventes_apt_maison), # Liste des colonnes ciblées
      ~ replace_na(.x, 0)                  # La fonction ~ remplace les NA par 0
    )
  ) %>% select(,-med_prix_m2_appartement, -med_prix_m2_maison,
               -med_prix_m2_local, -med_prix_m2_apt_maison)

nbr_normal <- interval(date_deb, date_fin) %/% months(1) + 1

# Il y a un problème dans certaines communes. C'est dû aux fusion et suppressions
communes_anormales <- bdd_1 %>%
  group_by(libelle_geo, code_geo) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n)) %>%
  filter(n > nbr_normal) %>%
  pull(code_geo)

# Pour ces communes, on fait la moyenne (deux termes généralement) des valeurs
# numériques sur les mois concernés

communes_a_agreger <- bdd_1 %>%
  filter(code_geo %in% communes_anormales)

communes_agregees <- communes_a_agreger %>%
  group_by(libelle_geo, date) %>%
  summarise(
    # Moyenne pour toutes les colonnes numériques
    across(where(is.numeric), mean, na.rm = TRUE),
    # Conservation de la première valeur pour les autres colonnes
    code_geo = first(code_geo),
    herminia = first(herminia),
    .groups = 'drop'
  ) %>% 
  dplyr::select(names(bdd_1))

autres_communes <- bdd_1 %>%
  filter(!code_geo %in% communes_anormales)

bdd_2 <- bind_rows(communes_agregees, autres_communes) %>% 
  arrange(code_geo)

# Vérifications
bdd_2 %>%
  group_by(libelle_geo, code_geo) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n)) %>%
  filter(n > nbr_normal) %>%
  pull(code_geo)
nrow(bdd_2) / nbr_normal # On tombe bien sur un entier

# On calcule le prix total par mois par commune
bdd_2$prix_total_maison<-bdd_2$nb_ventes_maison*bdd_2$moy_prix_m2_maison
bdd_2 <- bdd_2 %>% relocate(prix_total_maison, .after = 6)
bdd_2$prix_total_appartement<-bdd_2$nb_ventes_appartement*bdd_2$moy_prix_m2_appartement
bdd_2 <- bdd_2 %>% relocate(prix_total_appartement, .after = 9)
bdd_2$prix_total_local<-bdd_2$nb_ventes_local*bdd_2$moy_prix_m2_local
bdd_2 <- bdd_2 %>% relocate(prix_total_local, .after = 12)
bdd_2$prix_total_apt_maison<-bdd_2$nb_ventes_apt_maison*bdd_2$moy_prix_m2_apt_maison
bdd_2 <- bdd_2 %>% relocate(prix_total_apt_maison, .after = 15)

date_intervention <- as.Date("2025-02-01")

bdd_3 <- bdd_2 %>%
  mutate(
    time = case_when(
      date > date_intervention ~ 1,
      TRUE ~ 0
    ), .after = date
  )

bdd <- bdd_3



################################################################################
################################ Export ########################################
################################################################################


aws.s3::s3write_using(
  bdd,
  FUN = function(data, file) saveRDS(data, file = file),
  object = "diffusion/projet_eval_impact/donnees_nettoyees/bdd.rds",
  bucket = "thomasguinhut",
  opts = list(region = "")
)

nouveaux_objets <- setdiff(ls(), objets_initiaux)
rm(nouveaux_objets, list = nouveaux_objets)
