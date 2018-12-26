#' Find drug types from ICD-10-CM.
#'
#' Find any drug and selected opioids
#'
#' @param data input data
#' @param diag_ecode_col column indices
#'
#' @return any_drug, any_opioid, non_heroin_opioid, and
#'   heroin
#' @export
#'
#' @examples to be added
#' library(tidyverse)
#' filter(hosp_set, year == 2016, quarters(discharge_date) == "Q4" ) %>% od_drug_apr_icd10cm(diag_ecode_col = c(3, 6)) %>% sample_n(5)
#'
icd_drug_opioid <- function(data, diag_ecode_col) {
  drugs_icd10cm_ <- "^(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..[1-4](A|$)|((T3[679]9|T414|T427|T4[3579]9)[1-4].(A|$))"

  opioid_icd10cm_ <- "(T40[01234].|T406[09])[1-4](A|$)"

  non_heroin_opioid_icd10cm_ <- "(T40[0234].|T406[09])[1-4](A|$)"

  heroin_icd10cm_ <- "T401.[1-4](A|$)"


  data %>%
    mutate(
      any_drug = icd_new_diag(.,
        expr = drugs_icd10cm_,
        colvec = diag_ecode_col
      ),

      any_opioid = icd_new_diag(.,
        expr = opioid_icd10cm_,
        colvec = diag_ecode_col
      ),

      non_heroin_opioid = icd_new_diag(.,
        expr = non_heroin_opioid_icd10cm_,
        colvec = diag_ecode_col
      ),

      heroin = icd_new_diag(.,
        expr = heroin_icd10cm_,
        colvec = diag_ecode_col
      )
    ) %>%
    mutate(non_heroin_opioid = ifelse(heroin == 1, 0, non_heroin_opioid))
}
