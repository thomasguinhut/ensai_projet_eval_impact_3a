###########################################
#### Import des données et traitements ####
###########################################

# Chargement de la base avec les données locales sur la population, niveau de vie...
info_locales <-
  aws.s3::s3read_using(
    FUN = read.csv2,
    object = "/diffusion/bdd_info_locales.csv",
    bucket = "romaiw",
    opts = list("region" = "")
  )
info_locales <- info_locales %>%
  mutate(population = as.numeric(Population.des.ménages.2022),
         police = as.numeric(Police...Gendarmerie..en.nombre..2024),
         piscine = as.numeric(Bassin.de.natation..en.nombre..2024), 
         niv_vie = as.numeric(Médiane.du.niveau.de.vie.2021))
info_locales <- info_locales %>%
  select(Code, Libellé, population, police, piscine, niv_vie)

bdd <- bdd %>%
  mutate(
    annee_mois = format(date, "%Y-%m"),
    herminia = as.factor(herminia)
  )

# Séparer les données avant/après mars 2025
bdd_avant <- bdd %>%
  filter(date < as.Date("2025-03-01"))  

bdd_apres <- bdd %>%
  filter(date >= as.Date("2025-04-01"))  

# calcul des moyenne avant la tempête
moyenne <- bdd_avant %>%
  group_by(code_geo) %>%
  summarise(
    nb_ventes_pre = mean(nb_ventes_maison, na.rm = TRUE),
    prix_m2_pre = mean(moy_prix_m2_maison, na.rm = TRUE),
    .groups = "drop"
  )

bdd_apres <- bdd_apres %>%
  left_join(moyenne, by = "code_geo") %>%
  na.omit()

# Ajout des covariables pour le PSM
bdd_apres <- bdd_apres %>%
  left_join(info_locales, by = c("libelle_geo" = "Libellé"))

summary(bdd_apres)

#############
#### PSM ####
#############
ps_model <- glm(herminia ~ population + niv_vie,
                family = binomial(),
                data = bdd_apres)
summary(ps_model)

# Prédire le score de propension
bdd_apres$pscore <- predict(ps_model, type = "response")

ggplot(bdd_apres, aes(x = pscore, fill = herminia)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution du score de propension par groupe Herminia")

# Support commun
ggplot(bdd_apres, aes(x = pscore, color = herminia)) +
  geom_density() +
  labs(title = "Support commun des scores de propension")

ps_match <- matchit(herminia ~ population + niv_vie,
                    data = bdd_apres,
                    method = "nearest",
                    ratio = 1,
                    caliper = 0.2)
summary(ps_match)

# Love plot
cobalt::love.plot(ps_match)


match_data <- match.data(ps_match)

# Test de Student apparié pour le prix au m² de maison
treated <- match_data$moy_prix_m2_maison[match_data$herminia == "TRUE"]
control <- match_data$moy_prix_m2_maison[match_data$herminia == "FALSE"]
t.test(treated, control, paired = TRUE)

# Test de Student apparié pour le nombre de ventes de maison
treated <- match_data$nb_ventes_maison[match_data$herminia == "TRUE"]
control <- match_data$nb_ventes_maison[match_data$herminia == "FALSE"]
t.test(treated, control, paired = TRUE)

# Test de Student apparié pour le prix au m² d'appartement
treated <- match_data$moy_prix_m2_appartement[match_data$herminia == "TRUE"]
control <- match_data$moy_prix_m2_appartement[match_data$herminia == "FALSE"]
t.test(treated, control, paired = TRUE)

# Test de Student apparié pour le nombre de ventes
treated <- match_data$nb_ventes_appartement[match_data$herminia == "TRUE"]
control <- match_data$nb_ventes_appartement[match_data$herminia == "FALSE"]
t.test(treated, control, paired = TRUE)


#############
#### DID ####
#############
bdd_did <- bdd %>%
  mutate(
    post_herminia = ifelse(date >= as.Date("2025-04-01"), 1, 0),
    herminia = as.numeric(herminia)
  )

did_model <- plm(moy_prix_m2_maison ~ herminia + post_herminia + herminia:post_herminia,
                 data = bdd_did,
                 index = c("code_geo", "annee_mois"),
                 model = "within")
summary(did_model)

ggplot(bdd_did, aes(x = factor(post_herminia), y = moy_prix_m2_maison, fill = factor(herminia))) +
  geom_boxplot() +
  labs(title = "Prix au m² avant/après Herminia par groupe")
