# injury_matrix_all <- readr::read_rds("injury_matrix_all.rds")
# injury_matrix_all <- injury_matrix_all %>%
#   mutate(mechanism = gsub("Bites and Stings", "Bites/Stings", mechanism))

injury_matrix_all <- injury_matrix_all %>%
  mutate(mechanism = gsub("Natural Environmental", "Natural/Environmental", mechanism))

# readr::write_rds(injury_matrix_all, "injury_matrix_all.rds", compress = "xz")


icd10cm_injury_matrix <- readr::read_rds("injury_matrix_all.rds") %>%
  select(intent, everything())

usethis::use_data(icd10cm_injury_matrix, compress = "xz", overwrite = T)
