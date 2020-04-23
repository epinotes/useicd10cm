#' Selected injury ICD 10 CM regular expressions
#'
#' @return Return objects of regular expressions in the global environment
#' @export
#'
#' @examples
#'
#' icd_injury_regex()
#' grep("^icd10cm.+_$", objects(), value = TRUE)
#'
icd_injury_regex <- function() {
  icd10cm_any_drug_ <<- "(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..[1-4](A|$)|((T3[679]9|T414|T427|T4[3579]9)[1-4].(A|$))"

  icd10cm_opioid_ <<- "(T40[0-4].|T406[09])[1-4](A|$)"

  icd10cm_non_heroin_opioid_ <<- "(T40[0234].|T406[09])[1-4](A|$)"

  icd10cm_heroin_ <<- "T401.[1-4](A|$)"

  icd10cm_stimulant_ <<- "((T405.|T436[0-49])[1-4])(A|$)"

  icd10cm_cocaine_ <<- "T405.[1-4](A|$)"

  icd10cm_non_cocaine_stimulant_ <<- "T436[0-49][1-4](A|$)"

  icd10cm_drowning_ <<- "(T751..|W16[49]1.|(?!W16[49])W16..1|W22041|(V9[02]|W6[5-9]|W7[0-4]|X71|X92|Y21)...)(A|$)"

  icd10cm_unintentional_falls_ <<- "((?!V000)V00..1|(W0.|W1[0-579])...|W18[123]..|W16[49]2.|(?!W16[49])W16..2)(A|$)"

  icd10cm_hip_fracture_ <<- "(S72[012]..|M970..|T8404[01])(A|B|C|$)"

  icd10cm_unintentional_fire_ <<- "(X0[0-8]...)(A|$)"

  icd10cm_firearm_ <<- "((W3[23]0|(^X74|X95|Y24)[89]|Y350|Y384)..|((^X7|Y2)[23]|^X9[34])...|W340[09].)(A|$)"

  icd10cm_assault_ <<- "((T3[679]9|T414|T427|T4[3579]9)3.|(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..3|((T5[1-46-9]|T6[0-35])9|(T58|T61)[01]|T64[08])3.|(?!((T5[1-46-9]|T6[0-35])9|(T58|T61)[01]|T64[08]))(T5[1-9]|T6[0-5])..3|T71..3|(X9[2-9]|Y0[0-68])...|Y07.{1,3}|Y09)(A|$)"

  icd10cm_unintentional_mvt_ <<- "((V0[234][19]|V092|V1[2-4][3-9]|V19[4-6]|V2[0-8][3-9]|V29[4-9]|V[3-7].[4-9]|V8[3-6][0-3]|V87[0-8]|V892|V80[3-5]|V8[12]1)..)(A|$)"

  icd10cm_non_drug_poisoning_ <<- "((T5[1-9]|T6[0-5])...|(T38[67]|Y352)..)(A|$)"

  icd10cm_intentional_self_harm <<- "((T3[679]9|T414|T427|T4[3579]9)2.|(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..2|((T5[1-46-9]|T6[0-35])9|(T58|T61)[01]|T64[08])2.|(?!((T5[1-46-9]|T6[0-35])9|(T58|T61)[01]|T64[08]))(T5[1-9]|T6[0-5])..2|(^X7[1-9]|^X8[0-3])...|T71..2|T1491.)(A|$)"

  icd10cm_unintentional_struck_by_against_ <<- "(V000..|(?!V000)V00..2|W180..|(?!W22041)W[25][0-2]...)(A|$)"

  icd10cm_tbi_ <<- "((S02[018].|S0291|S040[2-4]|S06..|S071.|T744.).)(A|B|$)"

  icd10cm_spinal_cord_injury_ <<- "((S[123]4)...)(A|$)"

  icd10cm_valid_injury_ <<- "(S.....|(T3[679]9|T414|T427|T4[3579]9)[1-4].|(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..[1-4]|(T[012].|T3[34]|T5[1-9]|T6.|T7[0-6]|T79|M97)...|T8404.|O9A[2-5]..|T3[0-2].)(A|B|C|$)" # Diagnosis codes included for injury surveillance

  icd10cm_external_cause_injury_ <<- "((V|W|^X).....|Y[0-3]....|(T3[679]9|T414|T427|T4[3579]9)[1-4].|(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..[1-4]|T1491.|(T1[5-9]|T5[1-9]|T6[0-5]|T7[1346])...|T75[023]..)(A|$)" # External cause codes and diagnoses that include mechanism and intent

  # the following are not included in the injury surveillance

  icd10cm_adverse_effects_drug_ <<- "((T3[679]9|T414|T427|T4[3579]9)5.|(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..5 )(A|$)"

  icd10cm_underdosing_drug_ <<- "((T3[679]9|T414|T427|T4[3579]9)6.|(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..6 )(A|$)"
}
