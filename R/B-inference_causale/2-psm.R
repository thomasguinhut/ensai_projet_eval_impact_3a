bdd_psm <- bdd %>%
  filter(date == as.Date("2025-04-01"))

ggplot(bdd_psm, aes(x=herminia, y= population, color = herminia)) + geom_violin()
ggplot(bdd_psm, aes(x=herminia, y= niv_vie, color = herminia)) + geom_boxplot()

#############
#### PSM ####
#############
ps_model <- glm(herminia ~ population + niv_vie,
                family = binomial(),
                data = bdd_psm)
summary(ps_model)

stargazer(ps_model,
          title = "Résultats de la régression GLM",
          dep.var.labels = "Variable dépendante",
          report = "vc*p",
          covariate.labels = c("population" = "Population",
                               "niv_vie" = "Médiane des niveau de vie"),
          out = "resultats_glm.tex",
          header = FALSE,
          style = "default",
          digits = 3,
          single.row = TRUE)
# Prédire le score de propension
bdd_psm$pscore <- predict(ps_model, type = "response")

ggplot(bdd_psm, aes(x = pscore, fill = herminia)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution du score de propension par groupe Herminia")

# Support commun
ggplot(bdd_psm, aes(x = pscore, color = herminia)) +
  geom_density() +
  labs(title = "Support commun des scores de propension")

ps_match <- matchit(herminia ~ population + niv_vie,
                    data = bdd_psm,
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
