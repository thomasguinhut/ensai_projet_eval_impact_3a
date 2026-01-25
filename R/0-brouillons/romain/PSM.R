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

covariables <- bdd_avant %>%
  group_by(code_geo) %>%
  summarise(
    nb_ventes_pre = mean(nb_ventes_maison, na.rm = TRUE),
    prix_m2_pre = mean(moy_prix_m2_maison, na.rm = TRUE),
    .groups = "drop"
  )

bdd_apres <- bdd_apres %>%
  left_join(covariables, by = "code_geo") %>%
  na.omit()

#### PSM ####
ps_model <- glm(herminia ~ nb_ventes_pre + prix_m2_pre,
                family = binomial(),
                data = bdd_apres)

# Prédire le score de propension
bdd_apres$pscore <- predict(ps_model, type = "response")

ggplot(bdd_apres, aes(x = pscore, fill = herminia)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution du score de propension par groupe Herminia")

# Support commun
ggplot(bdd_apres, aes(x = pscore, color = herminia)) +
  geom_density() +
  labs(title = "Support commun des scores de propension")

ps_match <- matchit(herminia ~ nb_ventes_pre + prix_m2_pre,
                    data = bdd_apres,
                    method = "nearest",
                    ratio = 1,
                    caliper = 0.2)
summary(ps_match)

# Love plot
cobalt::love.plot(ps_match)


match_data <- match.data(ps_match)
treated <- match_data$moy_prix_m2_maison[match_data$herminia == "TRUE"]
control <- match_data$moy_prix_m2_maison[match_data$herminia == "FALSE"]
# Test de Student apparié pour le prix au m²
t.test(treated, control, paired = TRUE)

treated <- match_data$nb_ventes_maison[match_data$herminia == "TRUE"]
control <- match_data$nb_ventes_maison[match_data$herminia == "FALSE"]
# Test de Student apparié pour le nombre de ventes
t.test(treated, control, paired = TRUE)


#### DID ####
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
