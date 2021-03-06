#' icd10cm injury matrix. group by intent and mechanism
#'
#' Dataset of 92 rows and 3 variables.
#' grouped by intent and mechanism from original with icd10cm code in regular expressions
#'
#' @format Data frame
#' @source modified from the injury matrix "icd10cm_injury_matrix"
#' @keywords datasets
#' @examples
#' head(icd10cm_inj_regex, 10)
"icd10cm_inj_regex"

#' The complete icd10cm injury matrix.
#'
#' Dataset of 3,655 rows and 5 variables.
#' formatted from the original
#'
#' @format Data frame
#' @source
#'   \url{ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/injury/tools/}
#'
#' @keywords datasets
#' @examples
#' library(dplyr)
#' sample_n(icd10cm_injury_matrix, 10)
"icd10cm_injury_matrix"

#' icd10cm injury matrix by intent only.
#'
#' Dataset of 5 rows and 3 variables.
#'
#'
#' @format Data frame
#' @source Grouped from icd10cm_inj_regex
#'
#' @keywords datasets
#' @examples
#' icd10cm_intent_regex
"icd10cm_intent_regex"


#' icd10cm injury matrix by mechanism only.
#'
#' Dataset of 32 rows and 3 variables.
#'
#'
#' @format Data frame
#' @source Grouped from icd10cm_inj_regex.
#' @keywords datasets
#' @examples
#' library(dplyr)
#' sample_n(icd10cm_mech_regex, 10)
"icd10cm_mech_regex"

#' The icd10cm injury matrix by intent and mechanism.
#'
#' Dataset of 92 rows and 4 variables.
#' grouped by intent and mechanism from original with icd10cm code in regular expressions
#'
#' @format Data frame
#' @source modified from the injury matrix "icd10cm_injury_matrix"
#' @keywords datasets
#' @examples
#' head(icd10cm_intent_mech_regex, 10)
"icd10cm_intent_mech_regex"

#' Dataset with icd-10-cm codes.
#'
#' Dataset of 150 rows and 6 variables.
#'
#'
#' @format Data frame
#' @source created to use in examples.
#' @keywords datasets
#' @examples
#' icd10cm_data150
"icd10cm_data150"

#' icd-10-cm list and descriptions original.
#'
#' Dataset of 72,616 rows and 3 variables.
#'
#'
#' @format Data frame
#' @source
#'   \url{https://www.cms.gov/medicare/icd-10/2021-icd-10-cm}
#' @keywords datasets
#' @examples
#' tail(icd10cm_codes_regex)

"icd10cm_codes_regex"

#' Major Diagnostic Categories (MDC), diagnosis-related group (DRG), and ICD-10-CM .
#'
#' Dataset of 3594355 rows and 9 variables.
#'
#'
#' @format Data frame
#' @source
#'   \url{https://www.cms.gov/icd10m/version39-fullcode-cms/fullcode_cms/P0001.html}
#' @keywords datasets
#' @examples
#' tail(icd10cm_mdc_drg_regex)

"icd10cm_mdc_drg_regex"


#' Major Diagnostic Categories (MDC) and ICD-10-CM .
#'
#' Dataset of 65696 rows and 4 variables.
#'
#'
#' @format Data frame
#' @source
#'   \url{https://www.cms.gov/icd10m/version39-fullcode-cms/fullcode_cms/P0001.html}
#' @keywords datasets
#' @examples
#' tail(icd10cm_mdc_regex)

"icd10cm_mdc_regex"


#' County FIPS codes for WA State.
#'
#' Dataset of 39 rows and 7 variables.
#'
#'
#' @format Data frame
#' @source
#'   US Census Bureau Through the R package"tidycensus"
#' @keywords datasets
#' @examples
#' head(wa_fips_codes)
#'
"wa_fips_codes"



