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
#' @importFrom furrr future_map_dfr
#' @importFrom purrr flatten_dbl
#'
#' @examples
#'
#' library(dplyr)
#' library(furrr)
#' # plan(multiprocess) # To parallelize
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
    furrr::future_map_dfr(f1) %>%
    transmute(new_diag = f2(.)) %>%
    flatten_dbl()
}

