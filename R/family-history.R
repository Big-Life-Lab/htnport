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
#'         or stroke; 2 for "No" if the person had neither of the conditions; and NA if any of the input variables is a
#'         non-response, unless one of the conditions is present (i.e., one of the input variables is equal to 1).
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
  else if ((!is.na(CCC_61) && CCC_61 == 2) || (!is.na(CCC_63) && CCC_63 == 2) || (!is.na(CCC_81) && CCC_81 == 2)) {
    cardiov <- 2
  }
  
  return(cardiov)
}

#' @title Determine a respondent's cardiovascular disease (CVD) family history based on specific conditions related to heart disease and stroke in their immediate family members.
#'
#' @param FMH_11 An integer indicating whether an immediate family member was diagnosed with heart disease. 
#'              1 for "Yes", 2 for "No".
#' @param FMH_12 A numeric variable representing the youngest age at diagnosis of heart disease in an immediate family member.
#' @param FMH_13 An integer indicating whether an immediate family member was diagnosed with stroke. 
#'              1 for "Yes", 2 for "No".
#' @param FMH_14 A numeric variable representing the youngest age at diagnosis of stroke in an immediate family member.
#'
#' @return An integer indicating the cardiovascular disease (CVD) family history: 
#'   - 1 for "Yes" if there is a family history of premature CVD (before age 60) based on heart disease and stroke histories. 
#'   - 2 for "No" if there is no such family history.
#'   - NA(b) if any input contains non-responses or unknown diagnosis ages (997, 998, or 999).
#'
#' @details The function evaluates the input variables `FMH_11`, `FMH_12`, `FMH_13`, and `FMH_14` to determine the cardiovascular disease (CVD) family history.
#'   - If `FMH_11` is equal to 1 or `FMH_13` is equal to 1, it indicates that someone in the family had heart disease or stroke, respectively. 
#'   In this case, the function checks the age at diagnosis (`FMH_12` for heart disease and `FMH_14` for stroke). If the age is greater than or equal to 0 and less than 60, 
#'   it represents a diagnosis before age 60, and `famheart60` or `famstroke60` is set to 1, respectively.
#'   - If the age is greater than or equal to 60 and less than or equal to 100, it indicates a late diagnosis, and the corresponding `famheart60` and `famstroke60` are set to 0.
#'   - If any of the integer inputs contain non-responses (i.e., values greater than 2) or any age input takes non-response values (997, 998, or 999), `famCVD60` is set to NA to 
#'   indicate that the CVD family history is not available.
#'   - If any of the conditions for premature CVD (before age 60) are met for either heart disease or stroke, the function sets `famCVD60` to 1, indicating a family history of 
#'   premature CVD. Otherwise, `famCVD60` is set to 2, representing no family history of premature CVD.
#'
#' @examples
#' 
#' # Example: Determine CVD family history for a respondent with a family member diagnosed with heart disease at age 50.
#' determine_CVD_Family_History(FMH_11 = 1, FMH_12 = 50, FMH_13 = 2, FMH_14 = 6)
#' # Output: 1 (CVD family history is "Yes" due to a family member's premature heart disease).
#' 
#' @export
determine_CVD_Family_History <- function(FMH_11, FMH_12, FMH_13, FMH_14) {
  
  famheart60 <- 0
  famstroke60 <- 0
  famCVD60 <- haven::tagged_na("b")
  
  if (is.na(FMH_11) || is.na(FMH_12) || is.na(FMH_13) || is.na(FMH_14)) {
    return(famCVD60)
  }
  else if (FMH_11 == 1 || FMH_13 == 1) {  # someone in the family diagnosed with heart disease or stroke
    if (FMH_12 >= 0 && FMH_12 < 60) { # before age 60
      famheart60 <- 1
    }
    else if (FMH_14 >= 0 && FMH_14 < 60) { 
      famstroke60 <- 1
    } 
    else if (FMH_12 >= 60 && FMH_12 <= 100) {
      famstroke60 <- 0
    }
    else if (FMH_14 >= 60 && FMH_14 <= 100) {
      famheart60 <- 0
    }
    else {
      return(famCVD60)
    }
  }
  else if (FMH_11 == 2 && FMH_13 == 2) {
    famheart60 <- 0
  }
  else if (FMH_11 > 2 || FMH_13 > 2) {
    return(famCVD60)
  }
  
  if (famheart60 == 1 || famstroke60 == 1) { # Family history of premature CVD (before age 60) based on heart disease and stroke histories
    famCVD60 <- 1
  }
  else {
    famCVD60 <- 2
  }
  
  return(famCVD60)
  
}