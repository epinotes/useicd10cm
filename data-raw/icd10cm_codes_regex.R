

icd10cm_codes_regex <- readr::read_rds("icd10cm_codes_2021.rds") %>%
  mutate(icd10cm_regex = icd_make_regex1(icd10cm))

usethis::use_data(icd10cm_codes_regex, compress = "xz", overwrite = T)
