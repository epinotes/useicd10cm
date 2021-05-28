icd10cm_mdc_regex <- readr::read_rds("icd10cm_mdc_39.rds")

usethis::use_data(icd10cm_mdc_regex, compress = "xz", overwrite = T)
