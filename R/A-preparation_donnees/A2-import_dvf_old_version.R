################################################################################
################################# Importation ##################################
################################################################################


objets_initiaux <- ls()

dvf_1 <-
  aws.s3::s3read_using(
    FUN = read.csv2,
    object = "diffusion/projet_eval_impact/sources/dvf_plus.csv",
    bucket = "thomasguinhut",
    sep = "|",
    opts = list("region" = "")
  )

glimpse(dvf_1)



################################################################################
################################## Nettoyage ###################################
################################################################################


dvf_2 <- dvf_1 %>% 
  dplyr::select(idmutation,
                idmutinvar,
                idopendata,
                idnatmut,
                datemut, coddep, libnatmut, valeurfonc, nblot, nbcomm,
                l_codinsee, sterr, nbvolmut, nblocmut, nblocmai:smai5pp)

unique(dvf_2$nblot)

glimpse(dvf_2)

dvf_3 <- dvf_2 %>% 
  mutate(datemut = as.Date(datemut),
         coddep = as.character(coddep),
         valeurfonc = as.numeric(valeurfonc),
         nblot = as.integer(nblot),
         across(starts_with("s"), as.numeric),
         sterr = as.numeric(sterr)) %>% 
  filter(!str_detect(l_codinsee, ","))

glimpse(dvf_3)

# On vérifie qu'il n'y a pas de NA
dvf_3 %>%
  summarise(across(everything(), ~ sum(is.na(.x))))

# On retire les NA, faibles au regard de la taille de la base (0,3 %) + on trie
# par ordre chronologique
dvf_4 <- dvf_3 %>% 
  filter(!is.na(valeurfonc)) %>% 
  arrange(datemut)

dvf <- dvf_4



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
