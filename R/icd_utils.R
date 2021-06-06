
# icd_clean_mech_names ----------------------------------------------------

#' clean the mechanism categories replacing space and other symbols
#'  with _.
#'
#'
#' @param x input variable

#' @return return cleaner mechanism categories
#'
#' @export
#' @importFrom purrr compose
#'
#' @examples
#' library(dplyr)
#' icd_clean_mech_names(c("Natural/Environmental, Other"))
#'
icd_clean_mech_names <- purrr::compose(
  # remove repeat "_" and extreme "_"
  function(x) gsub("(_)(?=_*\\1)|^_|_$", "", x, perl = T),
  # not [A-Za-z0-9_] and replace with "_"
  function(x) gsub("\\W", "_", x),
  # parenthesis and its contents
  function(x) gsub("\\(.+\\)", "", x)
)

# icd_age_group10 ---------------------------------------------------------

#' Cut age into ten age groups
#'
#'
#' @param data input data
#' @param age age as continuous variable

#' @return return an additional age_group10 fields of ten age intervals
#'
#' @export
#' @importFrom classInt classIntervals findCols
#'
#' @examples
#' library(dplyr)
#' library(classInt)
#' set.seed(10)
#' dat <- tibble(age = sample(c(1:100, NA), 110, replace = TRUE))
#' dat <- dat %>% icd_age_group10(age = age)
#' dat
#'

icd_age_group10 <- function(data, age){

  age <- data %>% pull({{age}}) %>% unlist()
  age_max <- ifelse(max(age, na.rm = T) > 84, max(age, na.rm = T), 120)
  agecut10 <- c(0, 10, 14, 24, 34, 44, 54, 64, 74, 84, age_max)
  int10 <- classIntervals(age, n = 10, style = "fixed",
                          fixedBreaks = agecut10, intervalClosure = "right")
  agegrp10 <- as.factor(findCols(int10))
  data %>% mutate(agegrp10 = agegrp10,
                  age_group10 = forcats::fct_recode(agegrp10,
                                                    `0-10` = "1",
                                                    `11-14` = "2",
                                                    `15-24` = "3",
                                                    `25-34` = "4",
                                                    `35-44` = "5",
                                                    `45-54` = "6",
                                                    `55-64` = "7",
                                                    `65-74` = "8",
                                                    `75-84` = "9",
                                                    `85+` = "10")) %>%
    mutate(age_group10 = forcats::fct_explicit_na(age_group10)) %>%
    select(-agegrp10)
}

# icd_first_valid_index ---------------------------------------------------

#' Row operation that creates a vector of indices the first match of a pattern
#'
#' @param data input data
#' @param colvec selected columns to match
#' @param pattern the pattern to match
#'
#' @return return the vector of the indices of the matches with 0 for no match
#' @export
#' @importFrom purrr transpose detect_index map_dfr map_int
#'
#' @examples
#'
#' dat <- data.frame(x1 = letters[1:3], x2 = c("d", "a", "e"))
#' library(dplyr)
#' library(purrr)
#' dat %>% mutate(x3 = icd_first_valid_index(., colvec = c(1:2), pattern = "a"))
#'
icd_first_valid_index <- function(data, colvec, pattern) {

  requireNamespace("dplyr", quietly = T)

  f0 <- function(x) grepl(pattern = pattern, x, ignore.case = T, perl = T)
  f1 <- function(x) purrr::detect_index(x, f0)
  data %>%
    select({{colvec}}) %>%
    purrr::map_dfr(as.character) %>%
    purrr::transpose() %>%
    purrr::map_int(f1)
}

# icd_first_valid ---------------------------------------------------------


#' a row operation that will form a vector of the first match of a pattern.
#'
#'
#' @param data input data
#' @param colvec selected columns to match
#' @param pattern the pattern to match

#' @return return the vector of the matched characters with NA for a no match
#'
#' @export
#' @importFrom purrr transpose detect map map_if
#'
#' @examples
#'
#' dat <- data.frame(x1 = letters[1:3], x2 = c("d", "a", "e"))
#' library(dplyr)
#' library(purrr)
#' dat %>% mutate(x3 = icd_first_valid(., colvec = c(1:2), pattern = "a"))
icd_first_valid <- function(data, colvec, pattern) {

  requireNamespace("dplyr", quietly = T)
  requireNamespace("purrr", quietly = T)

  # colvec <- enquo(colvec)
  f0 <- function(x) grepl(pattern = pattern, x, ignore.case = T, perl = T)
  f1 <- function(x) detect(x, f0)
  data %>%
    select({{colvec}}) %>%
    map_dfr(as.character) %>%
    transpose() %>%
    map(f1) %>%
    map_if(is.null, ~NA_character_) %>%
    unlist()
}


# icd_make_regex ----------------------------------------------------------

#' transform the ICD-10-CM codes into an operational regular expression.
#'
#'
#' @param x input variable

#' @return collapse the grouped icd10cm as regular expressions with all the "x" not at the beginning of the icd10cm code replaced by '.' to match any character.
#'
#' @export
#' @importFrom purrr compose
#'
#' @examples
#' library(dplyr)
#' icd_make_regex(c("X45x2", "Y65xx"))
#'
icd_make_regex <- purrr::compose(
  function(x) gsub("X", "^X", x),
  function(x) paste(x, collapse = "|"),
  function(x) gsub("(?<!^)x", ".", x, ignore.case = T, perl = T)
)

# icd_make_regex1 ---------------------------------------------------------

#' transform the ICD-10-CM codes into an operational regular expression.
#'
#'
#' @param x input variable

#' @return return the icd10cm as regular expressions with all the "x" not at the beginning of the icd10cm code replaced by '.' to match any character.
#'
#' @export
#' @importFrom purrr compose
#'
#' @examples
#'
#' library(dplyr)
#' icd_make_regex1(c("X45x2", "Y65xx"))
icd_make_regex1 <- purrr::compose(
  function(x) gsub("X", "^X", x),
  function(x) gsub("(?<!^)x", ".", x, ignore.case = T, perl = T)
)

# icd_new_diag ------------------------------------------------------------

#' Create a new variable based on pattern in the argument expr
#'
#' @param data input data
#' @param expr regular expression describing the pattern of interest
#' @param colvec indices of variables of interest
#' @param ignore.case logical
#' @param perl logical
#'
#' @return new variable matching the pattern described in the regular expression
#' @export
#' @importFrom purrr flatten_dbl map_dfr
#'
#' @examples
#'
#' library(dplyr)
#' library(purrr)
#' icd10cm_data150 %>%
#'   mutate(hero = icd_new_diag(., expr = "T401.[1-4]", colvec = c(2:6))) %>%
#'   count(hero)
icd_new_diag <- function(data, expr, colvec, ignore.case = T, perl = T) {

  requireNamespace("dplyr", quietly = T)

  # colvec <- enquo(colvec)
  # assign '1' if the regular expression matched
  f1 <- function(x) grepl(expr, x, ignore.case = ignore.case, perl = perl)
  # any 1 in the diagnosis field suffices
  f2 <- function(x){
    sign(rowSums(x, na.rm = TRUE))
  }

  data %>% as_tibble() %>%
    select({{colvec}}) %>%
    mutate_all(as.character) %>%
    purrr::map_dfr(f1) %>%
    transmute(new_diag = f2(.)) %>%
    flatten_dbl()
}


# icd_create_indicator ----------------------------------------------------

#' Create a new indicator based on pattern in the argument expr
#'
#' @param data input data
#' @param new_name proposed name for the indicator
#' @param expr regular expression describing the pattern of interest
#' @param colvec indices or names of variables (where are the pattern) without quotes
#' @param ignore.case logical
#' @param perl logical
#'
#' @return new indicator matching the pattern described in the regular expression
#' @export
#' @importFrom dplyr if_any
#'
#' @examples
#'
#' library(dplyr)
#' library(useicd10cm)
#' icd10cm_data150 %>%
#'   icd_create_indicator(new_name = "hero", expr = "T401.[1-4]", colvec = c(2:6))) %>%
#'   count(hero)
icd_create_indicator <- function(data, new_name, expr,
                                 colvec, ignore.case = T, perl = T) {
  requireNamespace("dplyr", quietly = T)

  data %>%
    mutate({{new_name}} := case_when(
      if_any({{colvec}},
             function(x) grepl(expr, x, ignore.case = ignore.case, perl = perl)) ~ 1,
      TRUE ~ 0))
}



