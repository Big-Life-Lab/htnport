#' @title Determine the inclusion status of a respondent in the study sample
#'
#' @description This function evaluates the eligibility of a respondent for inclusion in the study sample based on their age, blood pressure,
#' and pregnancy status.
#'
#' @param agegroup2079 An integer indicating whether the respondent's age falls within the range of 20 to 79 years.
#'                     1 for "Yes", 2 for "No".
#' @param highBP14090_adj An integer indicating whether the respondent has hypertension (high blood pressure).
#'                    1 for "Yes", 2 for "No".
#' @param PRS_11 An integer indicating whether the respondent is pregnant. 1 for "Yes", 2 for "No".
#'
#' @return An integer indicating the sample inclusion status:
#'   - 1: Eligible for inclusion in the study sample.
#'   - 2: Not eligible for inclusion in the study sample.
#'
#' @details The function evaluates the input variables `agegroup2079`, `highBP14090_adj`, and `PRS_11` to determine the
#'          inclusion status of a respondent in the study sample. If the respondent's age falls within the specified range (20 to 79 years),
#'          they do not have a hypertension non-response (highBP14090_adj is not NA), and they are not pregnant (PRS_11 is not 1),
#'          the function sets `insample` to 1, indicating that the respondent is eligible for inclusion in the study sample. Otherwise,
#'          the function sets `insample` to 2, indicating that the respondent is not eligible for inclusion in the study sample.
#'
#' @examples
#'
#' # Example 1: Determine the inclusion status for a respondent who is eligible for the study sample.
#' # The respondent's age is between 20 and 79, they do not have hypertension (highBP14090_adj is 2), and they are not pregnant (PRS_11 is 2).
#' determine_inclusion_status(agegroup2079 = 1, highBP14090_adj = 2, PRS_11 = 2)
#' # Output: 1 (Inclusion status is "Yes" as the respondent meets the criteria for inclusion in the study sample).
#'
#' # Example 2: Determine the inclusion status for a respondent who is ineligible for the study sample.
#' # The respondent's age is not between 20 and 79, they may or may not have hypertension or be pregnant.
#' determine_inclusion_status(agegroup2079 = 2, highBP14090_adj = 1, PRS_11 = 2)
#' # Output: 2 (Inclusion status is "No" as the respondent does not meet the criteria for inclusion in the study sample).
#' 
#' @export
determine_inclusion_status <- function(agegroup2079, highBP14090_adj, PRS_11) {
  
  insample <- haven::tagged_na("b")
  
  if (is.na(agegroup2079) || is.na(PRS_11)) {
    return(insample)
  }
  
  if (agegroup2079 == 1 && !is.na(highBP14090_adj) && PRS_11 != 1) {
    insample <- 1
  } else {
    insample <- 2
  }
  
  return(insample)
}