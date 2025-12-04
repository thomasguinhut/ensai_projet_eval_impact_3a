dvf_communes <- dvf %>%
  mutate(annee = year(datemut)) %>%
  group_by(l_codinsee, annee) %>%
  summarise(across(where(is.numeric), mean), .groups = "drop")
