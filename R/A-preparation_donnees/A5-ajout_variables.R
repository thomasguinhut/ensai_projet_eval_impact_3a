info_locales <- aws.s3::s3read_using(
  FUN = read.csv2,
  object = "diffusion/projet_eval_impact/sources/bdd_info_locales.csv",
  bucket = "thomasguinhut",
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

# calcul des moyenne avant la tempête
moyenne <- bdd_avant %>%
  group_by(code_geo) %>%
  summarise(
    nb_ventes_pre = mean(nb_ventes_maison, na.rm = TRUE),
    prix_m2_pre = mean(moy_prix_m2_maison, na.rm = TRUE),
    .groups = "drop"
  )

new_bdd <- bdd %>%
  left_join(moyenne, by = "code_geo")

info_locales <- info_locales %>%
  mutate(Code = ifelse(nchar(Code) == 4, paste0("0", Code), Code))

# Ajout des covariables pour le PSM
new_bdd <- new_bdd %>%
  left_join(info_locales, by = c("code_geo" = "Code")) %>% 
  filter(!is.na(niv_vie))

glimpse(new_bdd)


################################################################################
################################ Export ########################################
################################################################################


aws.s3::s3write_using(
  new_bdd,
  FUN = function(data, file) saveRDS(data, file = file),
  object = "diffusion/projet_eval_impact/donnees_nettoyees/bdd.rds",
  bucket = "thomasguinhut",
  opts = list(region = "")
)

nouveaux_objets <- setdiff(ls(), objets_initiaux)
rm(nouveaux_objets, list = nouveaux_objets)
