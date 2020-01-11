icd10cm_inj_regex <- readr::read_rds("injury_matrix_all.rds") %>%
  group_by(intent, mechanism) %>%
  summarise_at(vars(icd10cm_regex), icd_make_regex) %>% ungroup

usethis::use_data(icd10cm_inj_regex, compress = "xz", overwrite = T)

