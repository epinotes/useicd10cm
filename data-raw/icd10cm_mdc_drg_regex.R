
icd10cm_mdc_drg_regex <- readr::read_rds("icd10cm_mdc_drg39.rds")

usethis::use_data(icd10cm_mdc_drg_regex, compress = "xz", overwrite = T)
