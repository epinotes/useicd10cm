icd10cm_intent_mech_regex <- icd10cm_inj_matrix %>%
  group_by(intent, mechanism) %>%
  summarise(icd10cm_regex = icd_make_regex(icd10cm))


icd10cm_intent_mech_regex <- icd10cm_intent_mech_regex %>%
  mutate(mechanism = icd_clean_mech_names(mechanism))

icd10cm_intent_mech_regex <- icd10cm_intent_mech_regex %>%
  mutate(intent_mechanism = paste(intent, mechanism, sep = "_"))

icd10cm_intent_mech_regex <- icd10cm_intent_mech_regex %>%
  ungroup()

usethis::use_data(icd10cm_intent_mech_regex, compress = "xz", overwrite = T)
