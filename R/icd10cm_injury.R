
# icd10cm core svipp valid injury subset ----------------------------------

#' Find core svipp valid injury cases from ICD-10-CM.
#'
#' Find nonfatal injury cases
#'
#' @param data input data
#' @param diag_col principal diagnosis code column
#'
#' @return core svipp nonfatal injury cases
#' @export
#' @source
#' \url{https://www.cdc.gov/injury/pdfs/2019_state_injury_indicator_instructions-508.pdf}
#'
#' @examples
#'
#' library(dplyr)
#' library(purrr)
#' icd10cm_data150 %>%
#'   icd_svip_injury(diag_col = c(2:6)) %>%
#'   sample_n(10)
icd_svip_injury <- function(data, diag_col) {
  requireNamespace("dplyr", quietly = T)

  icd10cm_valid_injury_ <- "(S.....|(T3[679]9|T414|T427|T4[3579]9)[1-4].|(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..[1-4]|(T[012].|T3[34]|T5[1-9]|T6.|T7[0-6]|T79|M97)...|T8404.|O9A[2-5]..|T3[0-2].)(A|B|C|$)" # Diagnosis codes


  data %>%
    icd_create_indicator(
      new_name = "svip_valid_injury",
      expr = icd10cm_valid_injury_,
      colvec = diag_col
    )
}


# icd10cm core svipp non fatal injury indicators ----------------------------------

#' Find core svipp indicators from ICD-10-CM.
#'
#' Find nonfatal injury indicators
#'
#' @param data input data
#' @param diag_ecode_col column indices
#'
#' @return selected core svipp nonfatal injury indicators
#' @export
#' @source
#' \url{https://www.cdc.gov/injury/pdfs/2019_state_injury_indicator_instructions-508.pdf}
#'
#' @examples
#'
#' library(dplyr)
#' library(purrr)
#' icd10cm_data150 %>%
#'   icd_svip(diag_ecode_col = c(2:6)) %>%
#'   sample_n(10)
icd_svip <- function(data, diag_ecode_col) {
  requireNamespace("dplyr", quietly = T)

  icd10cm_valid_injury_ <- "(S.....|(T3[679]9|T414|T427|T4[3579]9)[1-4].|(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..[1-4]|(T[012].|T3[34]|T5[1-9]|T6.|T7[0-6]|T79|M97)...|T8404.|O9A[2-5]..|T3[0-2].)(A|B|C|$)" # Diagnosis codes

  icd10cm_drowning_ <- "(T751..|W16[49]1.|(?!W16[49])W16..1|W22041|(V9[02]|W6[5-9]|W7[0-4]|X71|X92|Y21)...)(A|$)"

  icd10cm_unintentional_falls_ <- "((?!V000)V00..1|(W0.|W1[0-579])...|W18[123]..|W16[49]2.|(?!W16[49])W16..2)(A|$)"

  icd10cm_hip_fracture_ <- "(S72[012]..|M970..|T8404[01])(A|B|C|$)"

  icd10cm_unintentional_fire_ <- "(X0[0-8]...)(A|$)"

  icd10cm_firearm_ <- "((W3[23]0|(^X74|X95|Y24)[89]|Y350|Y384)..|((^X7|Y2)[23]|^X9[34])...|W340[09].)(A|$)"

  icd10cm_assault_ <- "((T3[679]9|T414|T427|T4[3579]9)3.|(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..3|((T5[1-46-9]|T6[0-35])9|(T58|T61)[01]|T64[08])3.|(?!((T5[1-46-9]|T6[0-35])9|(T58|T61)[01]|T64[08]))(T5[1-9]|T6[0-5])..3|T71..3|(X9[2-9]|Y0[0-68])...|Y07.{1,3}|Y09)(A|$)"

  icd10cm_unintentional_mvt_ <- "((V0[234][19]|V092|V1[2-4][3-9]|V19[4-6]|V2[0-8][3-9]|V29[4-9]|V[3-7].[4-9]|V8[3-6][0-3]|V87[0-8]|V892|V80[3-5]|V8[12]1)..)(A|$)"

  icd10cm_non_drug_poisoning_ <- "((T5[1-9]|T6[0-5])...|(T38[67]|Y352)..)(A|$)"

  icd10cm_intentional_self_harm_ <- "((T3[679]9|T414|T427|T4[3579]9)2.|(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..2|((T5[1-46-9]|T6[0-35])9|(T58|T61)[01]|T64[08])2.|(?!((T5[1-46-9]|T6[0-35])9|(T58|T61)[01]|T64[08]))(T5[1-9]|T6[0-5])..2|(^X7[1-9]|^X8[0-3])...|T71..2|T1491.)(A|$)"

  icd10cm_tbi_ <- "((S02[018].|S0291|S040[2-4]|S06..|S071.|T744.).)(A|B|$)"

  icd10cm_S0990_ <- "S0990" # Unspecified injury of head

  data %>%
    icd_create_indicator(
      new_name = "svip_valid_injury",
      expr = icd10cm_valid_injury_,
      colvec = diag_ecode_col
    ) %>%
    icd_create_indicator(
      new_name = "svip_drowning",
      expr = icd10cm_drowning_,
      colvec = diag_ecode_col
    ) %>%
    icd_create_indicator(
      new_name = "svip_falls",
      expr = icd10cm_unintentional_falls_,
      colvec = diag_ecode_col
    ) %>%
    icd_create_indicator(
      new_name = "svip_hip_fracture",
      expr = icd10cm_hip_fracture_,
      colvec = diag_ecode_col
    ) %>%
    icd_create_indicator(
      new_name = "svip_fire",
      expr = icd10cm_unintentional_fire_,
      colvec = diag_ecode_col
    ) %>%
    icd_create_indicator(
      new_name = "svip_firearm",
      expr = icd10cm_firearm_,
      colvec = diag_ecode_col
    ) %>%
    icd_create_indicator(
      new_name = "svip_assault",
      expr = icd10cm_assault_,
      colvec = diag_ecode_col
    ) %>%
    icd_create_indicator(
      new_name = "svip_mvt",
      expr = icd10cm_unintentional_mvt_,
      colvec = diag_ecode_col
    ) %>%
    icd_create_indicator(
      new_name = "svip_non_drug_poisoning",
      expr = icd10cm_non_drug_poisoning_,
      colvec = diag_ecode_col
    ) %>%
    icd_create_indicator(
      new_name = "svip_intentional_self_harm",
      expr = icd10cm_intentional_self_harm_,
      colvec = diag_ecode_col
    ) %>%
    icd_create_indicator(
      new_name = "svip_tbi",
      expr = icd10cm_tbi_,
      colvec = diag_ecode_col
    ) %>%
    icd_create_indicator(
      new_name = "svip_S0990",
      expr = icd10cm_S0990_,
      colvec = diag_ecode_col
    ) %>%
    mutate(
      svip_tbi_S0990 = ifelse((svip_tbi == 0 & svip_S0990 == 1), 1, 0),
    )
}

# icd_injury_regex --------------------------------------------------------

#' Selected injury ICD 10 CM regular expressions
#'
#' @return Return objects of regular expressions in the global environment
#' @export
#' @source
#'   \url{https://resources.cste.org/Injury-Surveillance-Methods-Toolkit/Home/GeneralInjuryIndicators}
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

  icd10cm_intentional_self_harm_ <<- "((T3[679]9|T414|T427|T4[3579]9)2.|(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..2|((T5[1-46-9]|T6[0-35])9|(T58|T61)[01]|T64[08])2.|(?!((T5[1-46-9]|T6[0-35])9|(T58|T61)[01]|T64[08]))(T5[1-9]|T6[0-5])..2|(^X7[1-9]|^X8[0-3])...|T71..2|T1491.)(A|$)"

  icd10cm_unintentional_struck_by_against_ <<- "(V000..|(?!V000)V00..2|W180..|(?!W22041)W[25][0-2]...)(A|$)"

  icd10cm_tbi_ <<- "((S02[018].|S0291|S040[2-4]|S06..|S071.|T744.).)(A|B|$)"

  icd10cm_spinal_cord_injury_ <<- "((S[123]4)...)(A|$)"

  icd10cm_valid_injury_ <<- "(S.....|(T3[679]9|T414|T427|T4[3579]9)[1-4].|(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..[1-4]|(T[012].|T3[34]|T5[1-9]|T6.|T7[0-6]|T79|M97)...|T8404.|O9A[2-5]..|T3[0-2].)(A|B|C|$)" # Diagnosis codes included for injury surveillance

  icd10cm_external_cause_injury_ <<- "((V|W|^X).....|Y[0-3]....|(T3[679]9|T414|T427|T4[3579]9)[1-4].|(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..[1-4]|T1491.|(T1[5-9]|T5[1-9]|T6[0-5]|T7[1346])...|T75[023]..)(A|$)" # External cause codes and diagnoses that include mechanism and intent

  # the following are not included in the injury surveillance

  icd10cm_adverse_effects_drug_ <<- "((T3[679]9|T414|T427|T4[3579]9)5.|(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..5 )(A|$)"

  icd10cm_underdosing_drug_ <<- "((T3[679]9|T414|T427|T4[3579]9)6.|(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..6 )(A|$)"
}

# icd_intent_mech ---------------------------------------------------------

#' Add intent and mechanism combined fields for injury ICD-10-CM.
#'
#' , reference
#' @param data input data
#' @param inj_col ecode and diagnosis column indices
#' @param reference injury matrix reference with three possible choices:
#' "both"(the default) adding intent and mechanism combined variables
#' "intent" adding intent only
#' "mechanism" adding mechanism only
#'
#' @return return the input with additional variables (93 intent_mechanism combinations, five intent variables, or 32 mechanism variable depending on the reference choice
#'
#' @export
#' @importFrom purrr map2_dfc
#'
#' @examples
#' library(dplyr)
#' library(purrr)
#' dat <- data.frame(
#'   d1 = c("T63023", "X92821", "X99100", "T360x"),
#'   d2 = c("T65823", "Y030x0", "T17200", "V0100x")
#' )
#'
#' dat %>% icd_intent_mech(inj_col = c(1, 2), reference = "both")
#' dat %>% icd_intent_mech(inj_col = c(1, 2), reference = "intent")
#' dat %>% icd_intent_mech(inj_col = c(1, 2), reference = "mechanism")
#'
#' @seealso \code{\link{icd_matched_intent_mech}} for a more efficient approach
#'
icd_intent_mech <- function(data, inj_col, reference = c("both", "intent", "mechanism")) {

  requireNamespace("dplyr", quietly = T)

  icd10cm_inj <- switch(match.arg(reference), both = icd10cm_intent_mech_regex, intent = icd10cm_intent_regex, mechanism = icd10cm_mech_regex)

  list_int_mech <- icd10cm_inj %>% pull(intent_mechanism)
  list_expr <- icd10cm_inj %>% pull(icd10cm_regex)

  f_im <- function(data = data, inj_col, var_name, expr) {
    var_name <- quo_name(var_name)
    data %>%
      icd_create_indicator(new_name = {{var_name}}, expr = expr, colvec = inj_col) %>%
      select({{var_name}})
  }

  dat2 <- purrr::map2_dfc(.x = list_int_mech, .y = list_expr, ~ f_im(data = data, inj_col = inj_col, var_name = .x, expr = .y))

  data %>% bind_cols(dat2)
}

# icd_select_intent -------------------------------------------------------

#' Add Selected Intents
#'
#' @param data input data
#' @param inj_col ecode and diagnosis column indices
#' @param ... keyword list
#'
#' @return return the input with additional intent variables
#' @export
#' @importFrom purrr map2_dfc
#'
#' @examples
#' library(dplyr)
#' library(purrr)
#' dat <- data.frame(
#'   d1 = c("T63023", "X92821", "X99100", "T360x"),
#'   d2 = c("T65823", "Y030x0", "T17200", "V0100x")
#' )
#'
#' dat %>% icd_select_intent(inj_col = c(1, 2), "untintentional", "assault")
#'
#' @seealso \code{\link{icd_matched_intent}} for a more efficient approach
#'
icd_select_intent <- function(data, inj_col, ...) {

  requireNamespace("dplyr", quietly = T)
  # utility function making ... a regex

  select_keyword <- function(...) {

    if(!length(list(...))) {
      keywd <- ""
    }
    else {
      keywd <- paste(list(...), collapse = "|")
    }
    keywd
  }

  icd10cm_inj <- icd10cm_intent_regex %>%
    filter(grepl(select_keyword(...), intent_mechanism, ignore.case = T, perl = T))

  list_int_mech <- icd10cm_inj %>%
    pull(intent_mechanism)

  list_expr <- icd10cm_inj %>% pull(icd10cm_regex)

  # utility function to add field names

  add_field_names <- function(data = data, inj_col, var_name, expr) {
    var_name <- quo_name(var_name)
    data %>%
      icd_create_indicator(new_name = {{var_name}}, expr = expr, colvec = inj_col) %>%
      select({{var_name}})
  }

  # add the new fields to the original data

  dat2 <- map2_dfc(.x = list_int_mech, .y = list_expr,
                   ~ add_field_names(data = data, inj_col = inj_col, var_name = .x, expr = .y))

  data %>% bind_cols(dat2)
}

# icd_select_mechanism ----------------------------------------------------

#' Add Selected Mechanisms
#'
#' @param data input data
#' @param inj_col ecode and diagnosis column indices
#' @param ... keyword list
#'
#' @return return the input with additional mechanism variables
#' @export
#' @importFrom purrr map2_dfc
#'
#' @examples
#' library(dplyr)
#' dat <- data.frame(
#'   d1 = c("T63023", "X92821", "X99100", "T360x"),
#'   d2 = c("T65823", "Y030x0", "T17200", "V0100x")
#' )
#'
#' dat %>% icd_select_mechanism(inj_col = c(1, 2), "fall", "firearm") # by default all mechanisms
#'
#' @seealso \code{\link{icd_matched_mechanism}} for a more efficient approach
#'
icd_select_mechanism <- function(data, inj_col, ...) {

  requireNamespace("dplyr", quietly = T)
  # utility function making ... a regex

  select_keyword <- function(...) {

    if(!length(list(...))) {
      keywd <- ""
    }
    else {
      keywd <- paste(list(...), collapse = "|")
    }
    keywd
  }

  icd10cm_inj <- icd10cm_mech_regex %>%
    filter(grepl(select_keyword(...), intent_mechanism, ignore.case = T, perl = T))

  list_int_mech <- icd10cm_inj %>%
    pull(intent_mechanism)

  list_expr <- icd10cm_inj %>% pull(icd10cm_regex)

  # utility function to add field names

  add_field_names <- function(data = data, inj_col, var_name, expr) {
    var_name <- quo_name(var_name)
    data %>%
      icd_create_indicator(new_name = {{var_name}},
                           expr = expr, colvec = inj_col) %>%
      select({{var_name}})
  }

  # add the new fields to the original data

  dat2 <- map2_dfc(.x = list_int_mech, .y = list_expr,
                   ~ add_field_names(data = data, inj_col = inj_col, var_name = .x, expr = .y))

  data %>% bind_cols(dat2)
}



# icd_matched_intent_mech ---------------------------------------------------------

#' Add matched intent and mechanism combined fields for injury ICD-10-CM.
#'
#' , reference
#' @param data input data
#' @param inj_col ecode and diagnosis column indices

#'
#' @return return the input with additional variables ( 92 intent_mechanism combinations)
#'
#' @export
#' @importFrom fuzzyjoin regex_left_join
#'
#' @examples
#' library(tidyverse)
#' library(fuzzyjoin)
#' dat <- data.frame(
#'   d1 = c("T63023", "X92821", "X99100", "T360x"),
#'   d2 = c("T65823", "Y030x0", "T17200", "V0100x")
#' )
#'
#' dat %>% icd_matched_intent_mech(inj_col = c(1, 2))
#'
icd_matched_intent_mech <- function(data, inj_col) {

  requireNamespace("dplyr", quietly = T)
  requireNamespace("tidyr", quietly = T)
  requireNamespace("fuzzyjoin", quietly = T)

  data_names_ <- names(data)

  added_names_ <- icd10cm_intent_mech_regex$intent_mechanism

  col_to_add_ <- rep(NA_character_, length(added_names_)) %>%
    set_names(added_names_)


  dat2 <- data %>%
    add_column(u.id. = c(1:nrow(.)))

  dat3 <- dat2 %>%
    select(u.id., all_of(inj_col)) %>%
    pivot_longer(cols = -c(u.id.), names_to = "diag", values_to = "icd10cm") %>%
    fuzzyjoin::regex_left_join(icd10cm_intent_mech_regex %>%
                                 select(icd10cm_regex, intent_mechanism),
                               by = c(icd10cm = "icd10cm_regex")) %>%
    filter(!is.na(intent_mechanism)) %>%
    select(-c(diag, icd10cm, icd10cm_regex)) %>%
    distinct() %>%
    mutate(case = 1)

  dat4 <- dat3 %>%
    pivot_wider(id_cols = u.id., names_from = intent_mechanism,
                values_from = case) %>%
    add_column(!!!col_to_add_[!names(col_to_add_) %in% names(.)])

  dat2 %>%
    left_join(dat4, by = "u.id.") %>%
    dplyr::mutate(across(all_of(added_names_), replace_na, replace = 0)) %>%
    select(all_of(data_names_), all_of(added_names_))

}


# icd_matched_intent ---------------------------------------------------------

#' Add matched intent fields for injury ICD-10-CM.
#'
#' , reference
#' @param data input data
#' @param inj_col ecode and diagnosis column indices

#'
#' @return return the input with additional variables (5 intents)
#'
#' @export
#' @importFrom fuzzyjoin regex_left_join
#'
#' @examples
#' library(tidyverse)
#' library(fuzzyjoin)
#' dat <- data.frame(
#'   d1 = c("T63023", "X92821", "X99100", "T360x"),
#'   d2 = c("T65823", "Y030x0", "T17200", "V0100x")
#' )
#'
#' dat %>% icd_matched_intent(inj_col = c(1, 2))
#'
icd_matched_intent <- function(data, inj_col) {

  requireNamespace("dplyr", quietly = T)
  requireNamespace("tidyr", quietly = T)
  requireNamespace("fuzzyjoin", quietly = T)

  data_names_ <- names(data)

  added_names_ <- icd10cm_intent_regex$intent_mechanism

  col_to_add_ <- rep(NA_character_, length(added_names_)) %>%
    set_names(added_names_)



  dat2 <- data %>%
    add_column(u.id. = c(1:nrow(.)))

  dat3 <- dat2 %>%
    select(u.id., all_of(inj_col)) %>%
    pivot_longer(cols = -c(u.id.), names_to = "diag", values_to = "icd10cm") %>%
    fuzzyjoin::regex_left_join(icd10cm_intent_regex %>%
                                 select(icd10cm_regex, intent_mechanism),
                               by = c(icd10cm = "icd10cm_regex")) %>%
    filter(!is.na(intent_mechanism)) %>%
    select(-c(diag, icd10cm, icd10cm_regex)) %>%
    distinct() %>%
    mutate(case = 1)

  dat4 <- dat3 %>%
    pivot_wider(id_cols = u.id., names_from = intent_mechanism,
                values_from = case) %>%
    add_column(!!!col_to_add_[!names(col_to_add_) %in% names(.)])

  dat2 %>%
    left_join(dat4, by = "u.id.") %>%
    dplyr::mutate(across(all_of(added_names_), replace_na, replace = 0)) %>%
    select(all_of(data_names_), all_of(added_names_))

}

# icd_matched_mechanism ---------------------------------------------------------

#' Add matched mechanism fields for injury ICD-10-CM.
#'
#' , reference
#' @param data input data
#' @param inj_col ecode and diagnosis column indices

#'
#' @return return the input with additional variables (32 mechanisms)
#'
#' @export
#' @importFrom fuzzyjoin regex_left_join
#'
#' @examples
#' library(tidyverse)
#' library(fuzzyjoin)
#' dat <- data.frame(
#'   d1 = c("T63023", "X92821", "X99100", "T360x"),
#'   d2 = c("T65823", "Y030x0", "T17200", "V0100x")
#' )
#'
#' dat %>% icd_matched_mechanism(inj_col = c(1, 2))
#'
icd_matched_mechanism <- function(data, inj_col) {

  requireNamespace("dplyr", quietly = T)
  requireNamespace("tidyr", quietly = T)
  requireNamespace("fuzzyjoin", quietly = T)

  data_names_ <- names(data)

  added_names_ <- icd10cm_mech_regex$intent_mechanism

  col_to_add_ <- rep(NA_character_, length(added_names_)) %>%
    set_names(added_names_)

  dat2 <- data %>%
    add_column(u.id. = c(1:nrow(.)))

  dat3 <- dat2 %>%
    select(u.id., all_of(inj_col)) %>%
    pivot_longer(cols = -c(u.id.), names_to = "diag", values_to = "icd10cm") %>%
    fuzzyjoin::regex_left_join(icd10cm_mech_regex %>%
                                 select(icd10cm_regex, intent_mechanism),
                               by = c(icd10cm = "icd10cm_regex")) %>%
    filter(!is.na(intent_mechanism)) %>%
    select(-c(diag, icd10cm, icd10cm_regex)) %>%
    distinct() %>%
    mutate(case = 1)

  dat4 <- dat3 %>%
    pivot_wider(id_cols = u.id., names_from = intent_mechanism,
                values_from = case) %>%
    add_column(!!!col_to_add_[!names(col_to_add_) %in% names(.)])

  dat2 %>%
    left_join(dat4, by = "u.id.") %>%
    dplyr::mutate(across(all_of(added_names_), replace_na, replace = 0)) %>%
    select(all_of(data_names_), all_of(added_names_))

}

