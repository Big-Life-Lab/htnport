#' @title Cardiovascular disease (CVD) personal history
#'
#' @description This function determines a respondent's cardiovascular disease (CVD) personal history based on the presence or absence
#' of specific conditions related to heart disease, heart attack, and stroke.
#'
#' @param CCC_61 [integer] An integer representing the respondent's personal history of heart disease. 1 for "Yes" if the person has
#'               heart disease, 2 for "No" if the person does not have heart disease.
#' @param CCC_63 [integer] An integer representing the respondent's personal history of heart attack. 1 for "Yes" if the person had
#'               a heart attack, 2 for "No" if the person did not have a heart attack.
#' @param CCC_81 [integer] An integer representing the respondent's personal history of stroke. 1 for "Yes" if the person had a stroke,
#'               2 for "No" if the person did not have a stroke.
#'
#' @return [integer] The CVD personal history:
#'         - 1: "Yes" if the person had heart disease, heart attack, or stroke.
#'         - 2: "No" if the person had neither of the conditions.
#'         - `haven::tagged_na("a")`: Not applicable
#'         - `haven::tagged_na("b")`: Missing
#'
#' @details This function synthesizes self-reported data on major cardiovascular events (heart disease, heart attack, stroke) into a single binary indicator.
#'
#'          **Missing Data Codes:**
#'          - For all input variables:
#'            - `6`: Valid skip. Handled as `haven::tagged_na("a")`.
#'            - `7-9`: Don't know, refusal, or not stated. Handled as `haven::tagged_na("b")`.
#'
#' @examples
#' # Scalar usage: Single respondent
#' # Determine CVD personal history for a person with heart disease (CCC_61 = 1).
#' determine_CVD_personal_history(CCC_61 = 1, CCC_63 = 2, CCC_81 = 2)
#' # Output: 1
#'
#' # Example: Respondent has non-response values for all inputs.
#' result <- determine_CVD_personal_history(CCC_61 = 8, CCC_63 = 8, CCC_81 = 8)
#' result # Shows: NA
#' haven::is_tagged_na(result, "b") # Shows: TRUE (confirms it's tagged NA(b))
#' format(result, tag = TRUE) # Shows: "NA(b)" (displays the tag)
#'
#' # Multiple respondents
#' determine_CVD_personal_history(CCC_61 = c(1, 2, 2), CCC_63 = c(2, 1, 2), CCC_81 = c(2, 2, 1))
#' # Returns: c(1, 1, 1)
#'
#' @seealso [determine_CVD_family_history()]
#' @export
determine_CVD_personal_history <- function(CCC_61, CCC_63, CCC_81) {
  dplyr::case_when(
    # Positive evidence always wins
    CCC_61 == 1 | CCC_63 == 1 | CCC_81 == 1 ~ 1,

    # Explicit negatives (all 2)
    CCC_61 == 2 & CCC_63 == 2 & CCC_81 == 2 ~ 2,

    # Valid skips
    CCC_61 == 6 | CCC_63 == 6 | CCC_81 == 6 ~ haven::tagged_na("a"),

    # Don't know, refusal, not stated
    CCC_61 %in% 7:9 | CCC_63 %in% 7:9 | CCC_81 %in% 7:9 ~ haven::tagged_na("b"),

    # Default fallback for anything weird
    .default = haven::tagged_na("b")
  )
}

#' @title Cardiovascular Disease (CVD) family history
#'
#' @description This function evaluates a respondent's family history of cardiovascular disease (CVD), based on data about diagnoses of heart disease and stroke in immediate family members and the ages at which these diagnoses occurred. It identifies premature CVD if any diagnosis occurred before age 60.
#'
#' @param FMH_11 [integer] An integer: Indicates whether an immediate family member was diagnosed with heart disease.
#'               - 1 for "Yes"
#'               - 2 for "No".
#' @param FMH_12 [numeric] A numeric: Represents the youngest age at diagnosis of heart disease in an immediate family member.
#' @param FMH_13 [integer] An integer: Indicates whether an immediate family member was diagnosed with stroke.
#'               - 1 for "Yes"
#'               - 2 for "No".
#' @param FMH_14 [numeric] A numeric: Represents the youngest age at diagnosis of stroke in an immediate family member.
#'
#' @return [integer] The CVD family history:
#'   - 1: "Yes" — Family history of premature CVD exists (diagnosis before age 60).
#'   - 2: "No" — No family history of premature CVD.
#'   - `haven::tagged_na("a")`: Not applicable
#'   - `haven::tagged_na("b")`: Missing
#'
#' @details This function assesses family history of premature cardiovascular disease (CVD), a significant risk factor for personal CVD development.
#'
#'          **Missing Data Codes:**
#'          - `FMH_11`, `FMH_13`:
#'            - `6`: Valid skip. Handled as `haven::tagged_na("a")`.
#'            - `7-9`: Don't know, refusal, or not stated. Handled as `haven::tagged_na("b")`.
#'          - `FMH_12`, `FMH_14`:
#'            - `996`: Valid skip. Handled as `haven::tagged_na("a")`.
#'            - `997-999`: Don't know, refusal, or not stated. Handled as `haven::tagged_na("b")`.
#'
#' @examples
#' # Scalar usage: Single respondent
#' # Example 1: Premature CVD due to heart disease diagnosis at age 50
#' determine_CVD_family_history(FMH_11 = 1, FMH_12 = 50, FMH_13 = 2, FMH_14 = NA)
#' # Output: 1
#'
#' # Example 2: Respondent has non-response values for all inputs.
#' result <- determine_CVD_family_history(FMH_11 = 8, FMH_12 = 998, FMH_13 = 8, FMH_14 = 998)
#' result # Shows: NA
#' haven::is_tagged_na(result, "b") # Shows: TRUE (confirms it's tagged NA(b))
#' format(result, tag = TRUE) # Shows: "NA(b)" (displays the tag)
#'
#' # Multiple respondents
#' determine_CVD_family_history(
#'   FMH_11 = c(1, 2, 1), FMH_12 = c(50, NA, 70),
#'   FMH_13 = c(2, 1, 2), FMH_14 = c(NA, 55, NA)
#' )
#' # Returns: c(1, 1, 2)
#'
#' # Database usage: Applied to survey datasets
#' library(dplyr)
#' # dataset %>%
#' #   mutate(cvd_family_history = determine_CVD_family_history(FMH_11, FMH_12, FMH_13, FMH_14))
#'
#' @seealso [determine_CVD_personal_history()]
#' @export
determine_CVD_family_history <- function(FMH_11, FMH_12, FMH_13, FMH_14) {
  # Determine if there is a family history of heart disease before age 60
  famheart60 <- dplyr::case_when(
    FMH_11 == 1 & FMH_12 >= 0 & FMH_12 < 60 ~ 1,
    # Valid skip for age
    FMH_11 == 1 & FMH_12 == 996 ~ haven::tagged_na("a"),
    # Don't know, refusal, not stated
    FMH_11 == 1 & (FMH_12 < 0 | FMH_12 > 79 | FMH_12 %in% c(997, 998, 999)) ~ haven::tagged_na("b"),
    # Valid skip for yes/no
    FMH_11 == 6 ~ haven::tagged_na("a"),
    # Don't know, refusal, not stated
    FMH_11 %in% 7:9 | !FMH_11 %in% c(1, 2) ~ haven::tagged_na("b"),
    TRUE ~ 0
  )

  # Determine if there is a family history of stroke before age 60
  famstroke60 <- dplyr::case_when(
    FMH_13 == 1 & FMH_14 >= 0 & FMH_14 < 60 ~ 1,
    # Valid skip for age
    FMH_13 == 1 & FMH_14 == 996 ~ haven::tagged_na("a"),
    # Don't know, refusal, not stated
    FMH_13 == 1 & (FMH_14 < 0 | FMH_14 > 79 | FMH_14 %in% c(997, 998, 999)) ~ haven::tagged_na("b"),
    # Valid skip for yes/no
    FMH_13 == 6 ~ haven::tagged_na("a"),
    # Don't know, refusal, not stated
    FMH_13 %in% 7:9 | !FMH_13 %in% c(1, 2) ~ haven::tagged_na("b"),
    TRUE ~ 0
  )

  # Determine overall family history of premature CVD
  dplyr::case_when(
    # Propagate tagged NAs
    haven::is_tagged_na(famheart60, "a") & haven::is_tagged_na(famstroke60, "a") ~ haven::tagged_na("a"),
    haven::is_tagged_na(famheart60, "b") & haven::is_tagged_na(famstroke60, "b") ~ haven::tagged_na("b"),

    # If either condition is met, classify as 1 (Yes)
    famheart60 == 1 | famstroke60 == 1 ~ 1,

    # If both are 0, then no premature CVD
    famheart60 == 0 & famstroke60 == 0 ~ 2,

    # Otherwise, if there are NAs that prevent a clear determination, return NA(b)
    .default = haven::tagged_na("b")
  )
}
