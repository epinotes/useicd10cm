#' Create a new variable without mutate based on pattern in the argument expr
#'
#' @param data input data
#' @param expr regular expression describing the pattern of interest
#' @param colvec indices of variables of interest
#' @param var_name the proposed name of the new variable. The default is "new_var"
#' @param ignore.case logical
#' @param perl logical
#'
#' @return new variable matching the pattern described in the regular expression
#' @export
#'
#' @examples
#'
#' library(dplyr)
#' library(purrr)
#' set.seed(2)
#' icd10cm_data150 %>%
#' icd_new_var(expr = "T401.[1-4]", colvec = c(2:6), var_name = "hero") %>%
#' sample_n(10)
#'
icd_new_var <- function(data, expr, colvec, var_name = "new_var", ignore.case = T, perl = T) {

  requireNamespace("dplyr", quietly = T)

  colvec <- enquo(colvec)
  var_name <- quo_name(var_name)

  # assign '1' if the regular expression matched
  f1 <- function(x) grepl(expr, x, ignore.case = ignore.case, perl = perl)
  # any 1 in the diagnosis field suffices
  f2 <- function(x) {
    sign(rowSums(x, na.rm = TRUE))
  }

  new_col <- data %>% as_tibble() %>%
    select(!!colvec) %>%
    mutate_all(as.character) %>%
    purrr::map_dfr(f1) %>%
    transmute(!!var_name := f2(.)) %>%
    flatten_dbl()

  data %>% bind_cols(new_col)

}

