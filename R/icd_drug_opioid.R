#' Find drug types from ICD-10-CM.
#'
#' Find any drug, selected opioids and stimulants
#'
#' @param data input data
#' @param diag_ecode_col column indices
#'
#' @return any_drug, any_opioid, non_heroin_opioid,
#'   heroin, stimulant, cocaine, stimulant_not_cocaine
#' @export
#'
#' @examples
#' library(tidyverse)
#' icd10cm_data150 %>% icd_drug_opioid(diag_ecode_col = c(2:6)) %>% sample_n(10)
#'
icd_drug_opioid <- function(data, diag_ecode_col) {
  drugs_icd10cm_ <-
    "(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..[1-4](A|$|\\s)|((T3[679]9|T414|T427|T4[3579]9)[1-4].(A|$|\\s))"

  opioid_icd10cm_ <- "(T40[0-4].|T406[09])[1-4](A|$|\\s)"

  non_heroin_opioid_icd10cm_ <- "(T40[0234].|T406[09])[1-4](A|$|\\s)"

  heroin_icd10cm_ <- "T401.[1-4](A|$|\\s)"

  stimulant_icd10cm_ <- "((T405.|T436[0-49])[1-4])(A|$|\\s)"

  cocaine_icd10cm_ <- "T405.[1-4](A|$|\\s)"

  non_cocaine_stimulant_icd10cm_ <- "436[0-49][1-4](A|$|\\s)"


  data %>%
    mutate(
      any_drug = icd_new_diag(.,
                              expr = drugs_icd10cm_,
                              colvec = diag_ecode_col),

      any_opioid = icd_new_diag(.,
                                expr = opioid_icd10cm_,
                                colvec = diag_ecode_col),

      non_heroin_opioid = icd_new_diag(.,
                                       expr = non_heroin_opioid_icd10cm_,
                                       colvec = diag_ecode_col),

      heroin = icd_new_diag(.,
                            expr = heroin_icd10cm_,
                            colvec = diag_ecode_col),

      stimulant = icd_new_diag(.,
                            expr = stimulant_icd10cm_,
                            colvec = diag_ecode_col),
      cocaine = icd_new_diag(.,
                               expr = cocaine_icd10cm_,
                               colvec = diag_ecode_col),
      non_cocaine_stimulant = icd_new_diag(.,
                               expr = non_cocaine_stimulant_icd10cm_,
                               colvec = diag_ecode_col)
    ) %>%
    mutate(non_heroin_opioid = ifelse(heroin == 1, 0, non_heroin_opioid),
           non_cocaine_stimulant = ifelse(cocaine == 1, 0, non_cocaine_stimulant))
}
