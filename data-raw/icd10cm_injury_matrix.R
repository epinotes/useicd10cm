
icd10cm_injury_matrix <- readr::read_rds("injury_matrix_all.rds") %>%
  select(intent, everything())

usethis::use_data(icd10cm_injury_matrix, compress = "xz", overwrite = T)
