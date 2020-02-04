#' Row operation that creates a vector of indices the first match of a pattern
#'
#' @param data input data
#' @param colvec selected columns to match
#' @param pattern the pattern to match
#'
#' @return return the vector of the indices of the matches with 0 for no match
#' @export
#' @importFrom purrr transpose detect_index
#' @importFrom furrr future_map_dfr future_map_int
#'
#' @examples
#'
#' dat <- data.frame(x1 = letters[1:3], x2 = c("d", "a", "e"))
#' library(dplyr)
#' library(purrr)
#' library(furrr)
#' # plan(multiprocess) # To parallelize
#' dat %>% mutate(x3 = icd_first_valid_index(., colvec = c(1:2), pattern = "a"))
#'
icd_first_valid_index <- function(data, colvec, pattern) {

  requireNamespace("dplyr", quietly = T)

  colvec <- enquo(colvec)
  f0 <- function(x) grepl(pattern = pattern, x, ignore.case = T, perl = T)
  f1 <- function(x) purrr::detect_index(x, f0)
  data %>%
    select(!!colvec) %>%
    furrr::future_map_dfr(as.character) %>%
    purrr::transpose() %>%
    furrr::future_map_int(f1)
}
