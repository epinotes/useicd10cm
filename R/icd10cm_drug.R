
# icd_drug_opioid_ish -----------------------------------------------------


#' Find intentional self-harm drug types from ICD-10-CM.
#'
#' Find any drug, selected opioids and stimulants with intentional self-harm
#'
#' @param data input data
#' @param diag_ecode_col column indices
#'
#' @return any_drug_ish, any_opioid_ish, non_heroin_opioid_ish,
#'   heroin_ish, stimulant_ish, cocaine_ish, stimulant_not_cocaine_ish
#' @export
#'
#' @examples
#'
#' library(dplyr)
#' library(purrr)
#' icd10cm_data150 %>%
#'   icd_drug_opioid_ish(diag_ecode_col = c(2:6)) %>%
#'   sample_n(10)
icd_drug_opioid_ish <- function(data, diag_ecode_col) {

  requireNamespace("dplyr", quietly = T)

  drugs_icd10cm_ish_ <-
    "(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..[2](A|$)|((T3[679]9|T414|T427|T4[3579]9)[2].(A|$))"

  opioid_icd10cm_ish_ <- "(T40[0-4].|T406[09])[2](A|$)"

  non_heroin_opioid_icd10cm_ish_ <- "(T40[0234].|T406[09])[2](A|$)"

  heroin_icd10cm_ish_ <- "T401.[2](A|$)"

  stimulant_icd10cm_ish_ <- "((T405.|T436[0-49])[2])(A|$)"

  cocaine_icd10cm_ish_ <- "T405.[2](A|$)"

  non_cocaine_stimulant_icd10cm_ish_ <- "T436[0-49][2](A|$)"


  data %>%
    icd_create_indicator(new_name = "any_drug_ish",
                         expr = drugs_icd10cm_ish_,
                         colvec = diag_ecode_col) %>%
    icd_create_indicator(new_name = "any_opioid_ish",
                         expr = opioid_icd10cm_ish_,
                         colvec = diag_ecode_col) %>%
    icd_create_indicator(new_name = "non_heroin_opioid_ish",
                         expr = non_heroin_opioid_icd10cm_ish_,
                         colvec = diag_ecode_col) %>%
    icd_create_indicator(new_name = "heroin_ish",
                         expr = heroin_icd10cm_ish_,
                         colvec = diag_ecode_col) %>%
    icd_create_indicator(new_name = "stimulant_ish",
                         expr = stimulant_icd10cm_ish_,
                         colvec = diag_ecode_col) %>%
    icd_create_indicator(new_name = "cocaine_ish",
                         expr = cocaine_icd10cm_ish_,
                         colvec = diag_ecode_col) %>%
    icd_create_indicator(new_name = "non_cocaine_stimulant_ish",
                         expr = non_cocaine_stimulant_icd10cm_ish_,
                         colvec = diag_ecode_col) %>%
    mutate(
      non_heroin_opioid_ish = ifelse(heroin_ish == 1, 0, non_heroin_opioid_ish),
      non_cocaine_stimulant_ish = ifelse(cocaine_ish == 1, 0, non_cocaine_stimulant_ish)
    )
}



# icd_drug_opioid_uu ------------------------------------------------------



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
    icd_create_indicator(new_name = "any_drug_uu",
                         expr = drugs_icd10cm_uu_,
                         colvec = diag_ecode_col) %>%
    icd_create_indicator(new_name = "any_opioid_uu",
                         expr = opioid_icd10cm_uu_,
                         colvec = diag_ecode_col) %>%
    icd_create_indicator(new_name = "non_heroin_opioid_uu",
                         expr = non_heroin_opioid_icd10cm_uu_,
                         colvec = diag_ecode_col) %>%
    icd_create_indicator(new_name = "heroin_uu",
                         expr = heroin_icd10cm_uu_,
                         colvec = diag_ecode_col) %>%
    icd_create_indicator(new_name = "stimulant_uu",
                         expr = stimulant_icd10cm_uu_,
                         colvec = diag_ecode_col) %>%
    icd_create_indicator(new_name = "cocaine_uu",
                         expr = cocaine_icd10cm_uu_,
                         colvec = diag_ecode_col) %>%
    icd_create_indicator(new_name = "non_cocaine_stimulant_uu",
                         expr = non_cocaine_stimulant_icd10cm_uu_,
                         colvec = diag_ecode_col) %>%
    mutate(
      non_heroin_opioid_uu = ifelse(heroin_uu == 1, 0, non_heroin_opioid_uu),
      non_cocaine_stimulant_uu = ifelse(cocaine_uu == 1, 0, non_cocaine_stimulant_uu)
    )
}

# icd_drug_opioid ---------------------------------------------------------

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
#'
#' library(dplyr)
#' library(purrr)
#' icd10cm_data150 %>%
#'   icd_drug_opioid(diag_ecode_col = c(2:6)) %>%
#'   sample_n(10)
icd_drug_opioid <- function(data, diag_ecode_col) {

  requireNamespace("dplyr", quietly = T)

  drugs_icd10cm_ <-
    "(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..[1-4](A|$)|((T3[679]9|T414|T427|T4[3579]9)[1-4].(A|$))"

  opioid_icd10cm_ <- "(T40[0-4].|T406[09])[1-4](A|$)"

  non_heroin_opioid_icd10cm_ <- "(T40[0234].|T406[09])[1-4](A|$)"

  heroin_icd10cm_ <- "T401.[1-4](A|$)"

  stimulant_icd10cm_ <- "((T405.|T436[0-49])[1-4])(A|$)"

  cocaine_icd10cm_ <- "T405.[1-4](A|$)"

  non_cocaine_stimulant_icd10cm_ <- "T436[0-49][1-4](A|$)"


  data %>%
    icd_create_indicator(new_name = "any_drug",
                         expr = drugs_icd10cm_,
                         colvec = diag_ecode_col) %>%
    icd_create_indicator(new_name = "any_opioid",
                         expr = opioid_icd10cm_,
                         colvec = diag_ecode_col) %>%
    icd_create_indicator(new_name = "non_heroin_opioid",
                         expr = non_heroin_opioid_icd10cm_,
                         colvec = diag_ecode_col) %>%
    icd_create_indicator(new_name = "heroin",
                         expr = heroin_icd10cm_,
                         colvec = diag_ecode_col) %>%
    icd_create_indicator(new_name = "stimulant",
                         expr = stimulant_icd10cm_,
                         colvec = diag_ecode_col) %>%
    icd_create_indicator(new_name = "cocaine",
                         expr = cocaine_icd10cm_,
                         colvec = diag_ecode_col) %>%
    icd_create_indicator(new_name = "non_cocaine_stimulant",
                         expr = non_cocaine_stimulant_icd10cm_,
                         colvec = diag_ecode_col) %>%
    mutate(
      non_heroin_opioid = ifelse(heroin == 1, 0, non_heroin_opioid),
      non_cocaine_stimulant = ifelse(cocaine == 1, 0, non_cocaine_stimulant)
    )
}

# icd_od_definitions ------------------------------------------------------

#' Selected OD ICD 10 CM definitions
#'
#' @return Return seven definitions and their regular expressions
#' @export
#'
#' @examples
#'
#' library(crayon)
#' useicd10cm::icd_od_definitions()
#'
icd_od_definitions <- function() {

  requireNamespace("crayon", quietly = T)

  cat("The regular expression version is provided at the end of each definiton.\n\n" %+%
        blue$underline$bold("\n1. Any drug:\n") %+%
        "Any diagnosis of T36-T50\n
       AND a 6th character of 1,2,3, or 4 for T36.9, T37.9, T39.9, T41.4, T42.7, T43.9, T45.9, T47.9, and T49.9, or a 5th character of 1,2,3, or 4 for all the others T36-T50\n
       AND a 7th character of A or missing\n" %+%
        green("\n(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..[1-4](A|$)|((T3[679]9|T414|T427|T4[3579]9)[1-4].(A|$))\n") %+%

        blue$underline$bold("\n2. Any Opioid:\n") %+%
        "Any diagnosis of T40.0X, T40.1X, T40.2X, T40.3X, T40.4X, T40.60, T40.60\n
       AND a 6th character of 1,2,3, or 4 \n
       AND a 7th character of A or missing\n" %+%
        green("\n(T40[0-4].|T406[09])[1-4](A|$)\n") %+%

        blue$underline$bold("\n3. Heroin:\n") %+%
        "Any diagnosis of T40.1X\n
      AND a 6th character of 1,2,3, or 4 \n
      AND a 7th character of A or missing\n" %+%
        green("\nT401.[1-4](A|$)\n") %+%

        blue$underline$bold("\n4. Non-Heroin Opioid:\n") %+%
        "Any diagnosis of T40.0X, T40.2X, T40.3X, T40.4X,T40.60, T40.69\n\n
      AND a 6th character of 1,2,3, or 4 \n
      AND a 7th character of A or missing\n
      Then exclude any cases of heroin as defined above\n" %+%
        green("\n(T40[0234].|T406[09])[1-4](A|$)\n") %+%

        blue$underline$bold("\n5. Stimulant:\n") %+%
        "Any diagnosis of T40.5X, T43.60, T43.61, T43.62, T43.63, T43.64, T43.69\n
      AND a 6th character of 1,2,3, or 4 \n
      AND a 7th character of A or missing\n" %+%
        green("\n((T405.|T436[0-49])[1-4])(A|$)\n") %+%

        blue$underline$bold("\n6. Cocaine:\n") %+%
        "Any diagnosis of T40.5X\n
      AND a 6th character of 1,2,3, or 4 \n
      AND a 7th character of A or missing\n" %+%
        green("\nT405.[1-4](A|$)\n") %+%

        blue$underline$bold("\n7. Non-Cocaine Stimulant:\n") %+%
        "Any diagnosis of T43.60, T43.61, T43.62, T43.63, T43.64, T43.69\n
      AND a 6th character of 1,2,3, or 4 \n
      AND a 7th character of A or missing\n
      Then exclude any cases of cocaine as defined above\n" %+%
        green("\nT436[0-49][1-4](A|$)\n"))
}

