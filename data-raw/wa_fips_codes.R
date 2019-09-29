
wa_fips_codes <- tidycensus::fips_codes %>%
  as_tibble %>%
  filter(state == "WA") %>%
  mutate(county = gsub(" County", "", county),
         county_fips_code = paste0(state_code, county_code),
         countyres = stringr::str_pad(1:39, 2, "left", 0 ))

usethis::use_data(wa_fips_codes, compress = "xz", overwrite = T)
