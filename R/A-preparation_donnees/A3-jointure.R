catnat <- catnat %>%
  mutate(
    dat_fin = as.Date(dat_fin),
    date = format(dat_fin, "%Y-%m")
  )

dvf <- dvf %>%
  mutate(
    annee_mois = as.Date(annee_mois),
    date = format(annee_mois, "%Y-%m")
  )
base_jointe<- full_join(catnat, dvf, 
                        by = c("cod_commune" = "code_geo", "date" = "date"))
base_econometrie <- base_jointe %>%
  filter(dat_fin >= as.Date("2020-01-01"))

catnat %>% 
  filter(substr(dat_deb, 1, 4) > 2020) %>% 
  group_by(id) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))
