#' Find intentional self-harm drug types from ICD-10-CM.
#'
#' Find any drug, selected opioids and stimulants
#'
#' @param data input data
#' @param diag_ecode_col column indices
#'
#' @return any_drug_isf, any_opioid_isf, non_heroin_opioid_isf,
#'   heroin_isf, stimulant_isf, cocaine_isf, stimulant_not_cocaine_isf
#' @export
#'
#' @examples
#'
#' library(dplyr)
#' library(purrr)
#' icd10cm_data150 %>%
#'   icd_drug_opioid(diag_ecode_col = c(2:6)) %>%
#'   sample_n(10)
icd_drug_opioid_isf <- function(data, diag_ecode_col) {

  requireNamespace("dplyr", quietly = T)

  drugs_icd10cm_isf_ <-
    "(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..[2](A|$)|((T3[679]9|T414|T427|T4[3579]9)[2].(A|$))"

  opioid_icd10cm_isf_ <- "(T40[0-4].|T406[09])[2](A|$)"

  non_heroin_opioid_icd10cm_isf_ <- "(T40[0234].|T406[09])[2](A|$)"

  heroin_icd10cm_isf_ <- "T401.[2](A|$)"

  stimulant_icd10cm_isf_ <- "((T405.|T436[0-49])[2])(A|$)"

  cocaine_icd10cm_isf_ <- "T405.[2](A|$)"

  non_cocaine_stimulant_icd10cm_isf_ <- "436[0-49][2](A|$)"


  data %>%
    mutate(
      any_drug = icd_new_diag(.,
                              expr = drugs_icd10cm_isf_,
                              colvec = diag_ecode_col
      ),

      any_opioid = icd_new_diag(.,
                                expr = opioid_icd10cm_isf_,
                                colvec = diag_ecode_col
      ),

      non_heroin_opioid = icd_new_diag(.,
                                       expr = non_heroin_opioid_icd10cm_isf_,
                                       colvec = diag_ecode_col
      ),

      heroin = icd_new_diag(.,
                            expr = heroin_icd10cm_isf_,
                            colvec = diag_ecode_col
      ),

      stimulant = icd_new_diag(.,
                               expr = stimulant_icd10cm_isf_,
                               colvec = diag_ecode_col
      ),
      cocaine = icd_new_diag(.,
                             expr = cocaine_icd10cm_isf_,
                             colvec = diag_ecode_col
      ),
      non_cocaine_stimulant = icd_new_diag(.,
                                           expr = non_cocaine_stimulant_icd10cm_isf_,
                                           colvec = diag_ecode_col
      )
    ) %>%
    mutate(
      non_heroin_opioid = ifelse(heroin == 1, 0, non_heroin_opioid),
      non_cocaine_stimulant = ifelse(cocaine == 1, 0, non_cocaine_stimulant)
    )
}
