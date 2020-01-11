# load("/.R_Cache/icd10cm_codes_2019@.RData")

icd10cm_codes_2019_regex <- readr::read_rds("icd10cm_codes_2019.rds") %>%
  set_names(c("icd10cm", "description")) %>%
  mutate(icd10cm_regex = icd_make_regex1(icd10cm))

usethis::use_data(icd10cm_codes_2019_regex, compress = "xz", overwrite = T)
