#' Find drug types from ICD-10-CM.
#'
#' Classify injuries from ICD-10-CM codes
#'
#' @param data input data
#' @param diag_ecode_col column indices
#'
#' @return any_drug, any_opioid, non_heroin_opioid,
#'   heroin, stimulant, cocaine, stimulant_not_cocaine
#' @export
#'
#' @examples
#'
#' library(dplyr)
#' library(purrr)
#' icd10cm_data150 %>%
#'   icd_drug_opioid(diag_ecode_col = c(2:6)) %>%
#'   sample_n(10)
icd_injury <- function(data, diag_ecode_col) {

  requireNamespace("dplyr", quietly = T)

  "T51-T65 T66-T76 T79 O9A.2-O9A.5 T84.04 M97"

icd_injury_diag <- "(S[0-9][0-9]|T0[7-9]|T[1-2][0-9]|T3[0-4]|T5[1-9]|T6[0-5]|T6[6-9]|T7[0-69])...(A|B|C|$)|O9A[2-5]..(A|B|C|$)|T8404|.(A|B|C|$)|M97...(A|B|C|$)|(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..[1-4](A|$)|((T3[679]9|T414|T427|T4[3579]9)[1-4].(A|$))"

drugs_icd10cm_ <-
    "(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..[1-4](A|$)|((T3[679]9|T414|T427|T4[3579]9)[1-4].(A|$))"



drowning_icd10cm_ <- "(T751..|W16[49]1.|(?!W16[49])W16..1|W22041|(V9[02]|W6[5-9]|W7.|X71|X92|Y21)...)(A|$)"

  non_heroin_opioid_icd10cm_ <- "(T40[0234].|T406[09])[1-4](A|$)"

  heroin_icd10cm_ <- "T401.[1-4](A|$)"

  stimulant_icd10cm_ <- "((T405.|T436[0-49])[1-4])(A|$)"

  cocaine_icd10cm_ <- "T405.[1-4](A|$)"

  non_cocaine_stimulant_icd10cm_ <- "436[0-49][1-4](A|$)"


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
      ),

      stimulant = icd_new_diag(.,
        expr = stimulant_icd10cm_,
        colvec = diag_ecode_col
      ),
      cocaine = icd_new_diag(.,
        expr = cocaine_icd10cm_,
        colvec = diag_ecode_col
      ),
      non_cocaine_stimulant = icd_new_diag(.,
        expr = non_cocaine_stimulant_icd10cm_,
        colvec = diag_ecode_col
      )
    ) %>%
    mutate(
      non_heroin_opioid = ifelse(heroin == 1, 0, non_heroin_opioid),
      non_cocaine_stimulant = ifelse(cocaine == 1, 0, non_cocaine_stimulant)
    )
}
