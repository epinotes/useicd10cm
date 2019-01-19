icd_first_valid <- function(data, colvec, pattern){
  colvec <- enquo(colvec)
  f0 <- function(x) grepl(pattern = pattern, x, ignore.case = T, perl = T)
  f1 <- function(x) detect(x, f0)
  data %>%
    select(!!colvec) %>%
    map_dfr(as.character) %>%
    transpose() %>%
    map(f1) %>%
    map_if(is.null, ~NA_character_) %>%
    unlist()
}
