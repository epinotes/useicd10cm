#' Find unintetional and undetermined drug types from ICD-10-CM.
#'
#' Find any drug, selected opioids and stimulants
#'
#' @param data input data
#' @param diag_ecode_col column indices
#'
#' @return any_drug_uu, any_opioid_uu, non_heroin_opioid_uu,
#'   heroin_uu, stimulant_uu, cocaine_uu, stimulant_not_cocaine_uu
#' @export
#'
#' @examples
#'
#' library(dplyr)
#' library(purrr)
#' icd10cm_data150 %>%
#'   icd_drug_opioid(diag_ecode_col = c(2:6)) %>%
#'   sample_n(10)
icd_drug_opioid_uu <- function(data, diag_ecode_col) {

  requireNamespace("dplyr", quietly = T)

  drugs_icd10cm_uu_ <-
    "(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..[14](A|$)|((T3[679]9|T414|T427|T4[3579]9)[14].(A|$))"

  opioid_icd10cm_uu_ <- "(T40[0-4].|T406[09])[14](A|$)"

  non_heroin_opioid_icd10cm_uu_ <- "(T40[0234].|T406[09])[14](A|$)"

  heroin_icd10cm_uu_ <- "T401.[14](A|$)"

  stimulant_icd10cm_uu_ <- "((T405.|T436[0-49])[14])(A|$)"

  cocaine_icd10cm_uu_ <- "T405.[14](A|$)"

  non_cocaine_stimulant_icd10cm_uu_ <- "T436[0-49][14](A|$)"


  data %>%
    mutate(
      any_drug_uu = icd_new_diag(.,
                              expr = drugs_icd10cm_uu_,
                              colvec = diag_ecode_col
      ),

      any_opioid_uu = icd_new_diag(.,
                                expr = opioid_icd10cm_uu_,
                                colvec = diag_ecode_col
      ),

      non_heroin_opioid_uu = icd_new_diag(.,
                                       expr = non_heroin_opioid_icd10cm_uu_,
                                       colvec = diag_ecode_col
      ),

      heroin_uu = icd_new_diag(.,
                            expr = heroin_icd10cm_uu_,
                            colvec = diag_ecode_col
      ),

      stimulant_uu = icd_new_diag(.,
                               expr = stimulant_icd10cm_uu_,
                               colvec = diag_ecode_col
      ),
      cocaine_uu = icd_new_diag(.,
                             expr = cocaine_icd10cm_uu_,
                             colvec = diag_ecode_col
      ),
      non_cocaine_stimulant_uu = icd_new_diag(.,
                                           expr = non_cocaine_stimulant_icd10cm_uu_,
                                           colvec = diag_ecode_col
      )
    ) %>%
    mutate(
      non_heroin_opioid_uu = ifelse(heroin_uu == 1, 0, non_heroin_opioid_uu),
      non_cocaine_stimulant_uu = ifelse(cocaine_uu == 1, 0, non_cocaine_stimulant_uu)
    )
}
