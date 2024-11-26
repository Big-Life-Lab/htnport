#' @title Determine cardiovascular disease (CVD) personal history
#' 
#' @description This function determines a respondent's cardiovascular disease (CVD) personal history based on the presence or absence
#' of specific conditions related to heart disease, heart attack, and stroke.
#'
#' @param CCC_61 An integer representing the respondent's personal history of heart disease. 1 for "Yes" if the person has
#'               heart disease, 2 for "No" if the person does not have heart disease.
#' @param CCC_63 An integer representing the respondent's personal history of heart attack. 1 for "Yes" if the person had
#'               a heart attack, 2 for "No" if the person did not have a heart attack.
#' @param CCC_81 An integer representing the respondent's personal history of stroke. 1 for "Yes" if the person had a stroke,
#'               2 for "No" if the person did not have a stroke.
#'
#' @return An integer indicating the CVD personal history: 1 for "Yes" if the person had heart disease, heart attack,
#'         or stroke; 2 for "No" if the person had neither of the conditions; and NA if all the input variables are a
#'         non-response.
#' 
#' @examples
#' 
#' # Determine CVD personal history for a person with heart disease (CCC_61 = 1).
#' determine_CVD_Personal_History(CCC_61 = 1, CCC_63 = 2, CCC_81 = 2)
#' # Output: 1 (CVD personal history is "Yes" as heart disease is present).
#' 
#' @export
determine_CVD_Personal_History <- function(CCC_61, CCC_63, CCC_81) {
  
  cardiov <- haven::tagged_na("b")
  
  if (is.na(CCC_61) && is.na(CCC_63) && is.na(CCC_81)) {
    cardiov <- haven::tagged_na("b")
  }
  else if ((!is.na(CCC_61) && CCC_61 == 1) || (!is.na(CCC_63) && CCC_63 == 1) || (!is.na(CCC_81) && CCC_81 == 1)) {
    cardiov <- 1
  }
  else {
    cardiov <- 2
  }
  
  return(cardiov)
}

#' @title Determine Cardiovascular Disease (CVD) Family History
#'
#' @description This function evaluates a respondent's family history of cardiovascular disease (CVD), based on data about diagnoses of heart disease and stroke in immediate family members and the ages at which these diagnoses occurred. It identifies premature CVD if any diagnosis occurred before age 60.
#'
#' @param FMH_11 Integer: Indicates whether an immediate family member was diagnosed with heart disease. 
#'               - 1 for "Yes"  
#'               - 2 for "No".
#' @param FMH_12 Numeric: Represents the youngest age at diagnosis of heart disease in an immediate family member.
#' @param FMH_13 Integer: Indicates whether an immediate family member was diagnosed with stroke. 
#'               - 1 for "Yes"  
#'               - 2 for "No".
#' @param FMH_14 Numeric: Represents the youngest age at diagnosis of stroke in an immediate family member.
#'
#' @return An integer indicating the CVD family history:
#'   - 1: "Yes" — Family history of premature CVD exists (diagnosis before age 60).  
#'   - 2: "No" — No family history of premature CVD.  
#'   - `NA(b)`: Missing/unknown — Due to non-responses, invalid inputs, or unknown diagnosis ages.
#'
#' @details 
#' - If both `FMH_11` (heart disease history) and `FMH_13` (stroke history) are `NA`, the function returns `NA(b)`.
#' - If either `FMH_11` or `FMH_13` indicates a diagnosis (`1` for "Yes"), the corresponding age (`FMH_12` for heart disease and `FMH_14` for stroke) is evaluated:
#'     - Ages between 0 and 59 indicate premature CVD.  
#'     - Ages between 60 and 100 indicate late-onset CVD.  
#'     - Ages outside this range or invalid inputs (997, 998, 999) result in `NA(b)`.  
#' - If both `FMH_11` and `FMH_13` are `2` ("No"), there is no family history of CVD (`2`).
#' - Any invalid inputs for `FMH_11` or `FMH_13` (values greater than 2) also result in `NA(b)`.
#'
#' @examples
#' # Example 1: Premature CVD due to heart disease diagnosis at age 50
#' determine_CVD_Family_History(FMH_11 = 1, FMH_12 = 50, FMH_13 = 2, FMH_14 = NA)
#' # Output: 1
#'
#' @export
determine_CVD_Family_History <- function(FMH_11, FMH_12, FMH_13, FMH_14) {
  famheart60 <- 0
  famstroke60 <- 0
  famCVD60 <- haven::tagged_na("b")
  
  # If all inputs are missing, return NA(b)
  if (is.na(FMH_11) && is.na(FMH_12) && is.na(FMH_13) && is.na(FMH_14)) {
    return(famCVD60)
  }
  
  # Check family history of heart disease
  if (!is.na(FMH_11) && FMH_11 == 1) {
    if (!is.na(FMH_12) && FMH_12 >= 0 && FMH_12 < 60) {
      famheart60 <- 1
    } else if (!is.na(FMH_12) && (FMH_12 < 0 || FMH_12 > 100 || FMH_12 %in% c(997, 998, 999))) {
      return(famCVD60)
    }
  } else if (!is.na(FMH_11) && FMH_11 > 2) {
    return(famCVD60)
  }
  
  # Check family history of stroke
  if (!is.na(FMH_13) && FMH_13 == 1) {
    if (!is.na(FMH_14) && FMH_14 >= 0 && FMH_14 < 60) {
      famstroke60 <- 1
    } else if (!is.na(FMH_14) && (FMH_14 < 0 || FMH_14 > 100 || FMH_14 %in% c(997, 998, 999))) {
      return(famCVD60)
    }
  } else if (!is.na(FMH_13) && FMH_13 > 2) {
    return(famCVD60)
  }
  
  # Determine final family history of premature CVD
  if (famheart60 == 1 || famstroke60 == 1) {
    famCVD60 <- 1
  } else if (famheart60 == 0 && famstroke60 == 0) {
    famCVD60 <- 2
  }
  
  return(famCVD60)
}