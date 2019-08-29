# load("/.R_Cache/icd10cm_codes_2019@.RData")

icd10cm_codes_2019 <- icd10cm_codes_2019 %>%
  mutate(icd10cm_regex = icd_make_regex1(icd10cm))

usethis::use_data(icd10cm_codes_2019, compress = "xz", overwrite = T)
