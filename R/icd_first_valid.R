
#' a row operation that will form a vector of the first match of a pattern.
#'
#'
#' @param data input data
#' @param colvec selected columns to match
#' @param pattern the pattern to match

#' @return return the vector of the matched characters with NA for a no match
#'
#' @export
#' @importFrom purrr transpose detect
#' @importFrom furrr future_map future_map_if
#'
#' @examples
#'
#' dat <- data.frame(x1 = letters[1:3], x2 = c("d", "a", "e"))
#' library(dplyr)
#' library(furrr)
#' # plan(multiprocess)
#' dat %>% mutate(x3 = icd_first_valid(., colvec = c(1:2), pattern = "a"))
icd_first_valid <- function(data, colvec, pattern) {

  requireNamespace("dplyr", quietly = T)
  requireNamespace("purrr", quietly = T)

  # colvec <- enquo(colvec)
  f0 <- function(x) grepl(pattern = pattern, x, ignore.case = T, perl = T)
  f1 <- function(x) detect(x, f0)
  data %>%
    select({{colvec}}) %>%
    future_map_dfr(as.character) %>%
    transpose() %>%
    future_map(f1) %>%
    future_map_if(is.null, ~NA_character_) %>%
    unlist()
}
