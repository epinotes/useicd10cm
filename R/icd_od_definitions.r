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
