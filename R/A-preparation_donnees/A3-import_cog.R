################################################################################
################################# Importation ##################################
################################################################################


objets_initiaux <- ls()

cog_1 <-
  aws.s3::s3read_using(
    FUN = read.csv2,
    object = "diffusion/projet_eval_impact/sources/v_commune_depuis_1943.csv",
    bucket = "thomasguinhut",
    sep = ",",
    opts = list("region" = "")
  )

glimpse(cog_1)

cog2025_1 <-
  aws.s3::s3read_using(
    FUN = read.csv2,
    object = "diffusion/projet_eval_impact/sources/v_commune_2025.csv",
    bucket = "thomasguinhut",
    sep = ",",
    opts = list("region" = "")
  )

glimpse(cog2025_1)



################################################################################
################################## Nettoyage ###################################
################################################################################


cog_2 <- cog_1 %>% 
  filter(substr(COM, 1, 2) %in% c("22", "29", "35", "56")) %>% 
  dplyr::select(COM, LIBELLE, DATE_DEBUT, DATE_FIN) %>% 
  mutate(DATE_DEBUT = as.Date(DATE_DEBUT),
         DATE_FIN = as.Date(DATE_FIN))

glimpse(cog_2)

cog2025_2 <- cog2025_1 %>% 
  filter(TYPECOM == "COM",
         substr(COM, 1, 2) %in% c("22", "29", "35", "56")) %>% 
  dplyr::select(COM, LIBELLE)

glimpse(cog2025_2)

cog <- cog_2
cog2025 <- cog2025_2


################################################################################
################################ Export ########################################
################################################################################


aws.s3::s3write_using(
  cog,
  FUN = function(data, file) saveRDS(data, file = file),
  object = "diffusion/projet_eval_impact/donnees_nettoyees/diffusion/cog.rds",
  bucket = "thomasguinhut",
  opts = list(region = "")
)

aws.s3::s3write_using(
  cog2025,
  FUN = function(data, file) saveRDS(data, file = file),
  object = "diffusion/projet_eval_impact/donnees_nettoyees/diffusion/cog2025.rds",
  bucket = "thomasguinhut",
  opts = list(region = "")
)

nouveaux_objets <- setdiff(ls(), objets_initiaux)
rm(nouveaux_objets, list = nouveaux_objets)
