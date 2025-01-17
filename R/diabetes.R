#' @title Determine Inclusive Diabetes Status
#'
#' @description This function evaluates inclusive diabetes status based on three factors: `diab_m`, `CCC_51`, and `diab_drug2`.
#'
#' @param diab_m An integer indicating whether the respondent has diabetes based on HbA1c level. 1 for "Yes", 2 for "No".
#' @param CCC_51 An integer indicating whether the respondent self-reported diabetes. 1 for "Yes", 2 for "No".
#' @param diab_drug2 An integer indicating whether the respondent is on diabetes medication. 1 for "Yes", 0 for "No".
#'
#' @return An integer indicating the inclusive diabetes status:
#'         - 1 ("Yes") if any of `diab_m`, `CCC_51`, or `diab_drug2` is 1.
#'         - 2 ("No") if all of `diab_m`, `CCC_51`, and `diab_drug2` are 2.
#'         - `haven::tagged_na("b")` if all three parameters are `NA`.
#'         - If two parameters are `NA`, the third non-`NA` parameter determines the result.
#'         - If one parameter is `NA`, the function checks the remaining two for a decision.
#'
#' @examples
#'
#' # Example: Determine the inclusive diabetes status for a respondent with diabetes based on HbA1c.
#' determine_inclusive_diabetes(diab_m = 1, CCC_51 = 2, diab_drug2 = 2)
#' # Output: 1 (Inclusive diabetes status is "Yes").
#'
#' # Example: Determine the inclusive diabetes status for a respondent no diabetes all around.
#' determine_inclusive_diabetes(diab_m = 2, CCC_51 = 2, diab_drug2 = 2)
#' # Output: 2 (Inclusive diabetes status is "No").
#'
#' # Example: Determine inclusive diabetes status when only one parameter is NA.
#' determine_inclusive_diabetes(diab_m = 2, CCC_51 = NA, diab_drug2 = 1)
#' # Output: 1 (Based on `diab_drug2`, inclusive diabetes status is "Yes").
#'
#' @export
determine_inclusive_diabetes <- function(diab_m, CCC_51, diab_drug2) {
  
  diabX <- haven::tagged_na("b")  # Default value as tagged NA
  
  # Case 1: All are not NA
  if (!is.na(diab_m) && !is.na(CCC_51) && !is.na(diab_drug2)) {
    if (diab_m == 1 || CCC_51 == 1 || diab_drug2 == 1) {
      diabX <- 1 # "Yes" if any of diab_m, CCC_51, or diab_drug2 is 1
    } else if (diab_m == 2 && CCC_51 == 2 && diab_drug2 == 0) {
      diabX <- 2 # "No" if all are 2
    }
  }
  
  # Case 2: All are NA
  else if (is.na(diab_m) && is.na(CCC_51) && is.na(diab_drug2)) {
    diabX <- haven::tagged_na("b")
  }
  
  # Case 3: Two values are NA, check the remaining one
  else if (is.na(diab_m) && is.na(CCC_51)) {
    if (!is.na(diab_drug2) && diab_drug2 == 1) {
      diabX <- 1 
    } else if (!is.na(diab_drug2) && diab_drug2 == 0) {
      diabX <- haven::tagged_na("b")  
    }
  } else if (is.na(diab_m) && is.na(diab_drug2)) {
    if (!is.na(CCC_51) && CCC_51 == 1) {
      diabX <- 1 
    } else if (!is.na(CCC_51) && CCC_51 == 2) {
      diabX <- 2  
    }
  } else if (is.na(CCC_51) && is.na(diab_drug2)) {
    if (!is.na(diab_m) && diab_m == 1) {
      diabX <- 1 
    } else if (!is.na(diab_m) && diab_m == 2) {
      diabX <- 2  
    }
  }
  
  # Case 4: Only one value is NA, check the other two
  else if (is.na(diab_m)) {
    if (CCC_51 == 1 || diab_drug2 == 1) {
      diabX <- 1 # "Yes" if any of the non-NA values is 1
    } else if (CCC_51 == 2 && diab_drug2 == 0) {
      diabX <- 2 # "No" if both non-NA values are 2
    }
  } else if (is.na(CCC_51)) {
    if (diab_m == 1 || diab_drug2 == 1) {
      diabX <- 1 # "Yes" if any of the non-NA values is 1
    } else if (diab_m == 2 && diab_drug2 == 0) {
      diabX <- 2 # "No" if both non-NA values are 2
    }
  } else if (is.na(diab_drug2)) {
    if (diab_m == 1 || CCC_51 == 1) {
      diabX <- 1 # "Yes" if any of the non-NA values is 1
    } else if (diab_m == 2 && CCC_51 == 2) {
      diabX <- 2 # "No" if both non-NA values are 2
    }
  }
  
  return(diabX)
}