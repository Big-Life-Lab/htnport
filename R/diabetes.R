#' @title Determine Inclusive Diabetes Status
#'
#' @description This function evaluates diabetes status based on two factors: `diab_m` and `CCC_51`.
#'
#' @param diab_m An integer indicating whether the respondent has diabetes based on HbA1c level. 1 for "Yes", 2 for "No".
#' @param CCC_51 An integer indicating whether the respondent self-reported diabetes. 1 for "Yes", 2 for "No".
#'
#' @return An integer indicating the inclusive diabetes status:
#'         - 1 ("Yes") if either `diab_m` or `CCC_51` is 1.
#'         - 2 ("No") if both `diab_m` and `CCC_51` are not 1.
#'         - `haven::tagged_na("b")` if both `diab_m` and `CCC_51` are NA.
#'
#' @examples
#'
#' # Example: Determine the inclusive diabetes status for a respondent with diabetes based on HbA1c.
#' determine_inclusive_diabetes(diab_m = 1, CCC_51 = 2)
#' # Output: 1 (Inclusive diabetes status is "Yes").
#'
#' # Example: Determine the inclusive diabetes status for a respondent without diabetes.
#' determine_inclusive_diabetes(diab_m = 2, CCC_51 = 2)
#' # Output: 2 (Inclusive diabetes status is "No").
#' 
#' @export
determine_inclusive_diabetes <- function(diab_m, CCC_51) {
  
  diabX <- haven::tagged_na("b")
  
  # Case 1: Both are not NA
  if (!is.na(diab_m) && !is.na(CCC_51)) {
    if (diab_m == 1 || CCC_51 == 1) {
      diabX <- 1 # "Yes" if either diab_m or CCC_51 equals 1
    } else {
      diabX <- 2 # "No" if both diab_m and CCC_51 equal 2
    }
  }
  # Case 2: Both are NA
  else if (is.na(diab_m) && is.na(CCC_51)) {
    diabX <- haven::tagged_na("b")
  }
  # Case 3: One is NA, check the other
  else {
    if (is.na(diab_m) && CCC_51 == 1) {
      diabX <- 1
    } else if (is.na(CCC_51) && diab_m == 1) {
      diabX <- 1
    } else if (is.na(diab_m) && CCC_51 == 2) {
      diabX <- 2
    } else if (is.na(CCC_51) && diab_m == 2) {
      diabX <- 2
    }
  }
  
  return(diabX)
}