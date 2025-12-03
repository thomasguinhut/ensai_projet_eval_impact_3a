################################################################################
################################# Importation ##################################
################################################################################


objets_initiaux <- ls()

catnat_1 <-
  aws.s3::s3read_using(
    FUN = read.csv2,
    object = "projet_eval_impact/diffusion/sources/catnat_gaspar.csv",
    bucket = "thomasguinhut",
    opts = list("region" = "")
  )

glimpse(catnat_1)



################################################################################
################################## Nettoyage ###################################
################################################################################


catnat_2 <- catnat_1 %>%
  dplyr::select(-c("dat_pub_arrete", "dat_pub_jo", "dat_maj"))

# Il apparaît que dans les deux variables dates retenues, il y a deux
# formatages : AAAA-MM-JJ et AAAA-MM-JJ HH-MM-SS. On vérifie qu'il n'y a pas
# d'autres formatages.
est_format_date <- Vectorize(function(x) {
  !is.na(x) && (
    str_detect(x, "^\\d{4}-\\d{2}-\\d{2}$") ||
      str_detect(x, "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$")
  )
})

catnat_2 %>%
  filter(!est_format_date(dat_deb)) %>%
  distinct(dat_deb) %>%
  pull(dat_deb)

catnat_2 %>%
  filter(!est_format_date(dat_fin)) %>%
  distinct(dat_fin) %>%
  pull(dat_fin)

# Pas d'autres formats, on convertie en format Date en prenant les premiers
# XXXX-XX-XX + on ne garde que la Bretagne + on trie de façon chronologique +
# on crée une variable duree
catnat_3 <- catnat_2 %>% 
  mutate(
    dat_deb = as.Date(substr(dat_deb, 1, 10)), 
    dat_fin = as.Date(substr(dat_fin, 1, 10))
  ) %>% 
  filter(substr(cod_commune, 1, 2) %in% c("29", "22", "56", "35")) %>% 
  arrange(dat_deb) %>% 
  mutate(duree_jours = as.numeric(dat_fin - dat_deb) + 1)

# On vérifie qu'il n'y a pas de modalité étrange
unique(catnat_3$lib_risque_jo)

summary(catnat_3)

# On vérifie qu'il n'y a pas de NA
catnat_3 %>%
  summarise(across(everything(), ~ sum(is.na(.x))))

catnat <- catnat_3



################################################################################
################################ Export ########################################
################################################################################


aws.s3::s3write_using(
  catnat,
  FUN = function(data, file) saveRDS(data, file = file),
  object = "projet_eval_impact/diffusion/donnees_nettoyees/catnat.rds",
  bucket = "thomasguinhut",
  opts = list(region = "")
)

nouveaux_objets <- setdiff(ls(), objets_initiaux)
rm(nouveaux_objets, list = nouveaux_objets)
