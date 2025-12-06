################################################################################
################################# Importation ##################################
################################################################################


objets_initiaux <- ls()

dvf_1 <-
  aws.s3::s3read_using(
    FUN = read.csv2,
    object = "diffusion/projet_eval_impact/sources/stats_dvf.csv",
    bucket = "thomasguinhut",
    sep = ",",
    opts = list("region" = "")
  )

glimpse(dvf_1)



################################################################################
################################## Nettoyage ###################################
################################################################################


dvf_2 <- dvf_1 %>% 
  filter(echelle_geo == "commune",
         substr(code_geo, 1, 2) %in% c("22", "29", "35", "56")) %>% 
  dplyr::select(code_geo, libelle_geo, annee_mois, everything(), -c("code_parent", "echelle_geo")) %>% 
  mutate(annee_mois = as.Date(paste0(annee_mois, "-01"), format = "%Y-%m-%d"))

glimpse(dvf_2)

dvf <- dvf_2



################################################################################
################################ Export ########################################
################################################################################


aws.s3::s3write_using(
  dvf,
  FUN = function(data, file) saveRDS(data, file = file),
  object = "diffusion/projet_eval_impact/donnees_nettoyees/dvf.rds",
  bucket = "thomasguinhut",
  opts = list(region = "")
)

nouveaux_objets <- setdiff(ls(), objets_initiaux)
rm(nouveaux_objets, list = nouveaux_objets)
