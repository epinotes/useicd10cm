icd10cm_mech_regex <- icd10cm_inj_matrix %>%
  group_by(mechanism) %>%
  summarise(icd10cm_regex = icd_make_regex(icd10cm))

icd10cm_mech_regex <- icd10cm_mech_regex %>%
  mutate(intent_mechanism = icd_clean_mech_names(mechanism))


usethis::use_data(icd10cm_mech_regex, compress = "xz", overwrite = T)
