icd10cm_inj_regex <- icd10cm_inj_matrix %>%
  group_by(intent, mechanism) %>%
  summarise(icd10cm_regex = icd_make_regex(icd10cm))

usethis::use_data(icd10cm_inj_regex, compress = "xz", overwrite = T)
