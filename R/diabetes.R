#' @title Determine Inclusive Diabetes Status
#'
#' @description This function evaluates various factors to determine the inclusive diabetes status of a respondent.
#'
#' @param diab_m An integer indicating whether the respondent has diabetes based on HbA1c level. 1 for "Yes", 2 for "No".
#' @param diab_drug An integer indicating whether the respondent takes diabetes drugs. 1 for "Yes", 0 for "No".
#' @param CCC_51 An integer indicating whether the respondent has diabetes in general. 1 for "Yes", 2 for "No".
#'
#' @return An integer indicating the inclusive diabetes status. If any of the input parameters (diab_m, diab_drug, CCC_51)
#'         are non-response values (not 1 or 2), the inclusive diabetes status will be NA(b). If any of the input parameters
#'         is equal to 1, indicating a positive response, the inclusive diabetes status will be 1 ("Yes"). Otherwise, if all
#'         input parameters are 2 ("No"), the inclusive diabetes status will be 2 ("No").
#'
#' @examples
#'
#' # Example: Determine the inclusive diabetes status for a respondent who has diabetes based on HbA1c.
#' determine_inclusive_diabetes(diab_m = 1, diab_drug = 2, CCC_51 = 2)
#' # Output: 1 (Inclusive diabetes status is "Yes" due to having diabetes based on HbA1c).
#'
#' # Example: Determine the inclusive diabetes status for a respondent who does not have diabetes.
#' determine_inclusive_diabetes(diab_m = 2, diab_drug = 2, CCC_51 = 2)
#' # Output: 2 (Inclusive diabetes status is "No" as the respondent does not have diabetes based on any of the factors).
#' 
#' @export
determine_inclusive_diabetes <- function(diab_m, diab_drug, CCC_51) {
  
  diabX <- haven::tagged_na("b")
  
  if (is.na(diab_m) || is.na(diab_drug) || is.na(CCC_51)) {
    return(diabX)
  }
  else if (diab_m == 1 || diab_drug == 1 || CCC_51 == 1) {
    diabX <- 1 # "Yes" if one answered "Yes" for either of the three inputs
  }
  else {
    diabX <- 2 # "No" if all else
  }
  
  return(diabX)
}