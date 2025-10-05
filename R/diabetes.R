#' @title Diabetes derived variable
#'
#' @description This function evaluates diabetes status using a comprehensive approach that combines
#' laboratory measurements, self-reported diagnosis, and medication usage to create an inclusive
#' diabetes classification.
#'
#' @param diab_m [integer] An integer indicating whether the respondent has diabetes based on HbA1c level. 1 for "Yes", 2 for "No".
#' @param CCC_51 [integer] An integer indicating whether the respondent self-reported diabetes. 1 for "Yes", 2 for "No".
#' @param diab_drug2 [integer] An integer indicating whether the respondent is on diabetes medication. 1 for "Yes", 0 for "No".
#'
#' @return [integer] The inclusive diabetes status:
#'         - 1 ("Yes") if any of `diab_m`, `CCC_51`, or `diab_drug2` is 1.
#'         - 2 ("No") if all of `diab_m`, `CCC_51`, and `diab_drug2` are 2 or 0.
#'         - `haven::tagged_na("a")`: Not applicable
#'         - `haven::tagged_na("b")`: Missing
#'
#' @details This function classifies diabetes status based that considers:
#'
#'          **Data Sources:**
#'          - Laboratory: HbA1c levels indicating diabetes (diab_m)
#'          - Self-report: Participant-reported diabetes diagnosis (CCC_51)
#'          - Medication: Current diabetes medication usage (diab_drug2)
#'
#'          **Classification Logic:**
#'          - ANY positive indicator results in diabetes classification
#'          - ALL negative indicators required for "no diabetes" classification
#'          - Sophisticated missing data handling preserves available information
#'
#'          **Missing Data Codes:**
#'          - `diab_m`, `diab_drug2`:
#'            - Tagged NA "a": Valid skip.
#'            - Tagged NA "b": Don't know, refusal, or not stated.
#'          - `CCC_51`:
#'            - `6`: Valid skip. Handled as `haven::tagged_na("a")`.
#'            - `7-9`: Don't know, refusal, or not stated. Handled as `haven::tagged_na("b")`.
#'
#' @examples
#' # Scalar usage: Single respondent
#' # Example: Determine the inclusive diabetes status for a respondent with diabetes based on HbA1c.
#' determine_inclusive_diabetes(diab_m = 1, CCC_51 = 2, diab_drug2 = 0)
#' # Output: 1 (Inclusive diabetes status is "Yes").
#'
#' # Example: Determine the inclusive diabetes status for a respondent no diabetes all around.
#' determine_inclusive_diabetes(diab_m = 2, CCC_51 = 2, diab_drug2 = 0)
#' # Output: 2 (Inclusive diabetes status is "No").
#'
#' # Example: Determine inclusive diabetes status when only one parameter is NA.
#' determine_inclusive_diabetes(diab_m = 2, CCC_51 = NA, diab_drug2 = 1)
#' # Output: 1 (Based on `diab_drug2`, inclusive diabetes status is "Yes").
#'
#' # Example: Respondent has non-response values for all inputs.
#' result <- determine_inclusive_diabetes(haven::tagged_na("b"), 8, haven::tagged_na("b"))
#' result # Shows: NA
#' haven::is_tagged_na(result, "b") # Shows: TRUE (confirms it's tagged NA(b))
#' format(result, tag = TRUE) # Shows: "NA(b)" (displays the tag)
#'
#' # Multiple respondents
#' determine_inclusive_diabetes(diab_m = c(1, 2, 2), CCC_51 = c(2, 1, 2), diab_drug2 = c(0, 0, 1))
#' # Returns: c(1, 1, 1)
#'
#' # Database usage: Applied to survey datasets
#' library(dplyr)
#' # dataset %>%
#' #   mutate(diabetes_status = determine_inclusive_diabetes(diab_m, CCC_51, diab_drug2))
#'
#' @seealso Related health condition functions: [determine_hypertension()], [calculate_GFR()]
#' @export
determine_inclusive_diabetes <- function(diab_m, CCC_51, diab_drug2) {
  vals <- c(diab_m, CCC_51, diab_drug2)
  non_missing <- vals[!is.na(vals) & !haven::is_tagged_na(vals, "a") & !haven::is_tagged_na(vals, "b")]

  dplyr::case_when(
    # Positive evidence always first
    diab_m == 1 | CCC_51 == 1 | diab_drug2 == 1 ~ 1,

    # Explicit negatives if there is at least one observed value and ALL observed are negative
    length(non_missing) > 0 & all(non_missing %in% c(0, 2)) ~ 2,

    # NA(a) takes precedence over NA(b)
    haven::is_tagged_na(diab_m, "a") |
      haven::is_tagged_na(CCC_51, "a") | CCC_51 == 6 |
      haven::is_tagged_na(diab_drug2, "a") ~ haven::tagged_na("a"),

    # NA(b) next in precedence
    haven::is_tagged_na(diab_m, "b") |
      haven::is_tagged_na(CCC_51, "b") | CCC_51 %in% 7:9 |
      haven::is_tagged_na(diab_drug2, "b") |
      all(is.na(vals)) ~ haven::tagged_na("b"),

    # Default fallback
    .default = haven::tagged_na("b")
  )
}
