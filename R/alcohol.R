#' @title Low risk drinking score
#'
#' @description
#' This function calculates a low drink score (step 1 only) for a respondent using
#' Canada's Low-Risk Alcohol Drinking Guideline. The score is based solely on the
#' number of standard drinks consumed per week and the respondent's sex. (Step 2,
#' which would add additional points based on other drinking habits, is not included.).
#'
#' @param CLC_SEX [integer] An integer indicating the respondent's sex (1 for male, 2 for female).
#' @param ALC_11 [integer] An integer indicating whether the respondent drank alcohol in the past year (1 for "Yes", 2 for "No").
#' @param ALCDWKY [integer] An integer representing the number of standard drinks consumed by the respondent in a week.
#'
#' @return [integer] The low drink score, with:
#'   - 1 for "Low risk" (0 points),
#'   - 2 for "Marginal risk" (1–2 points),
#'   - 3 for "Medium risk" (3–4 points), and
#'   - 4 for "High risk" (5–9 points).
#' If inputs are invalid or out of bounds, the function returns a tagged NA.
#'
#' @details
#' The scoring is determined by first allocating points (referred to as `step1`) based on the weekly
#' alcohol consumption and the respondent's sex:
#'   - If the respondent drank in the past year (ALC_11 == 1):
#'     - For ALCDWKY <= 10, assign 0 points.
#'     - For ALCDWKY > 10 and <= 15: assign 0 points for males (CLC_SEX == 1) and 1 point for females (CLC_SEX == 2).
#'     - For ALCDWKY > 15 and <= 20: assign 1 point for males and 3 points for females.
#'     - For ALCDWKY > 20: assign 3 points.
#'   - For respondents who did not drink in the past year (ALC_11 == 2), 0 points are assigned.
#'
#' These `step1` points are then mapped to the final categorical score as follows:
#'   - 0 points -> score of 1 ("Low risk"),
#'   - 1–2 points -> score of 2 ("Marginal risk"),
#'   - 3–4 points -> score of 3 ("Medium risk"),
#'   - 5–9 points -> score of 4 ("High risk").
#'
#' @details
#' This function implements Canada's Low-Risk Alcohol Drinking Guidelines (Step 1 only) to assess
#' alcohol consumption risk. The scoring system considers both the quantity of alcohol consumed
#' and biological sex differences in alcohol metabolism.
#'
#' **Risk Categories:**
#' - Low risk (0 points): Safe consumption levels
#' - Marginal risk (1-2 points): Slightly elevated risk
#' - Medium risk (3-4 points): Moderate health concerns
#' - High risk (5-9 points): Significant health risks
#'
#' **Sex-Based Differences:**
#' Women generally have lower tolerance thresholds due to physiological differences in
#' alcohol metabolism, reflected in the sex-specific point allocations.
#'
#' **Non-response Handling:**
#' Invalid inputs or survey non-response values result in tagged NA ("b").
#'
#' @note
#' This function implements only Step 1 of the guidelines. Step 2 (additional drinking pattern
#' assessments) is not included due to data limitations in the survey.
#'
#' @examples
#' # Scalar usage: Single respondent
#' # Example: A male respondent who drank in the past year and consumes 3 standard drinks per week.
#' low_drink_score_fun(CLC_SEX = 1, ALC_11 = 1, ALCDWKY = 3)
#' # Expected output: 1 (Low risk)
#'
#' # Missing data examples showing tagged NA patterns
#' result <- low_drink_score_fun(CLC_SEX = 1, ALC_11 = 6, ALCDWKY = 5)
#' result # Shows: NA
#' haven::is_tagged_na(result, "a") # Shows: TRUE (confirms it's tagged NA(a))
#' format(result, tag = TRUE) # Shows: "NA(a)" (displays the tag)
#'
#' result <- low_drink_score_fun(CLC_SEX = 1, ALC_11 = 7, ALCDWKY = 5)
#' result # Shows: NA
#' haven::is_tagged_na(result, "b") # Shows: TRUE (confirms it's tagged NA(b))
#' format(result, tag = TRUE) # Shows: "NA(b)" (displays the tag)
#'
#' result <- low_drink_score_fun(CLC_SEX = 1, ALC_11 = 1, ALCDWKY = NA)
#' result # Shows: NA
#' haven::is_tagged_na(result, "b") # Shows: TRUE (confirms it's tagged NA(b))
#' format(result, tag = TRUE) # Shows: "NA(b)" (displays the tag)
#'
#' # Multiple respondents
#' low_drink_score_fun(CLC_SEX = c(1, 2, 1), ALC_11 = c(1, 1, 2), ALCDWKY = c(3, 12, NA))
#' # Returns: c(1, 2, 1)
#'
#' # Database usage: Applied to survey datasets
#' library(dplyr)
#' # dataset %>%
#' #   mutate(low_drink_score = low_drink_score_fun(CLC_SEX, ALC_11, ALCDWKY))
#'
#' @seealso [low_drink_score_fun1()] for extended categorization including former/never drinkers
#' @references Canada's Low-Risk Alcohol Drinking Guidelines, Health Canada
#' @export
low_drink_score_fun <- function(CLC_SEX, ALC_11, ALCDWKY) {
  step1 <- dplyr::case_when(
    # Sex and drinking alcohol within past year variables
    # Not applicable (takes precedence)
    CLC_SEX == 6 | ALC_11 == 6 ~ haven::tagged_na("a"),
    # Missing
    CLC_SEX %in% 7:9 | ALC_11 %in% 7:9 ~ haven::tagged_na("b"),
    # Invalid codes
    !CLC_SEX %in% c(1, 2) | !ALC_11 %in% c(1, 2) ~ haven::tagged_na("b"),

    # Drinks per week variable
    # Valid skip
    ALCDWKY == 996 ~ haven::tagged_na("a"),
    # Don't know, refusal, not stated
    ALCDWKY %in% 997:999 ~ haven::tagged_na("b"),

    # Logic for valid categorical values
    ALC_11 == 2 & is.na(ALCDWKY) ~ 0,
    ALCDWKY <= 10 ~ 0,
    ALCDWKY > 10 & ALCDWKY <= 15 & CLC_SEX == 1 ~ 0,
    ALCDWKY > 10 & ALCDWKY <= 15 & CLC_SEX == 2 ~ 1,
    ALCDWKY > 15 & ALCDWKY <= 20 & CLC_SEX == 1 ~ 1,
    ALCDWKY > 15 & ALCDWKY <= 20 & CLC_SEX == 2 ~ 3,
    ALCDWKY > 20 ~ 3,
    .default = haven::tagged_na("b")
  )

  dplyr::case_when(
    # Propagate any tagged NAs from step1
    haven::is_tagged_na(step1, "a") ~ haven::tagged_na("a"),
    haven::is_tagged_na(step1, "b") | is.na(step1) ~ haven::tagged_na("b"),

    # Score assignment for valid step1 values
    step1 == 0 ~ 1L,
    step1 %in% 1:2 ~ 2L,
    step1 %in% 3:4 ~ 3L,
    step1 %in% 5:9 ~ 4L,
    .default = haven::tagged_na("b")
  )
}

#' @title Low risk drinking score - former/never categories
#'
#' @description
#' Computes a categorical alcohol consumption score based on Canada's Low-Risk Alcohol Drinking Guidelines (Step 1),
#' while distinguishing between never, former, light, moderate, and heavy drinkers. The function uses information
#' about weekly consumption, past-year use, lifetime drinking, and history of heavy drinking.
#'
#' @param CLC_SEX [integer] Respondent's sex (1 = male, 2 = female).
#' @param ALC_11 [integer] Whether the respondent drank alcohol in the past year (1 = Yes, 2 = No).
#' @param ALCDWKY [integer] Number of standard drinks consumed in a typical week (0–84).
#' @param ALC_17 [integer] Whether the respondent ever drank alcohol in their lifetime (1 = Yes, 2 = No).
#' @param ALC_18 [integer] Whether the respondent regularly drank more than 12 drinks per week (1 = Yes, 2 = No).
#'
#' @return [integer] Score: 1 = Never drank, 2 = Low-risk (former or light) drinker, 3 = Moderate drinker (1--2 points), 4 = Heavy drinker (3--4 points).
#' If inputs are invalid or out of bounds, the function returns a tagged NA.
#'
#' @details
#' Step 1: Assign points based on weekly alcohol consumption.
#'   - If the respondent drank in the past year (ALC_11 == 1):
#'     - 0 to 10 drinks/week: 0 points
#'     - 11 to 15 drinks/week: 0 points for males, 1 point for females
#'     - 16 to 20 drinks/week: 1 point for males, 3 points for females
#'     - More than 20 drinks/week: 3 points for males, 5 points for females
#'   - If they did not drink in the past year (ALC_11 == 2): 0 points
#'
#' Step 2: Determine the final categorical score.
#'   - If the point score from Step 1 is 0, the final category is determined based on lifetime and past-year drinking habits:
#'     - A score of 1 (Never drinker) is assigned if the respondent either never drank alcohol in their lifetime or is a former drinker who did not regularly consume more than 12 drinks a week.
#'     - A score of 2 (Low-risk drinker) is assigned if the respondent drank in the past year (but still scored 0 points) or is a former drinker with a history of regularly consuming more than 12 drinks a week.
#'   - If the point score from Step 1 is 1 or 2, the respondent is classified as a Moderate drinker (Score = 3).
#'   - If the point score from Step 1 is 3 or more, the respondent is classified as a Heavy drinker (Score = 4).
#' If inputs are invalid or out of bounds, the function returns a tagged NA.
#'
#' @note
#' This function uses only Step 1 of the guidelines, as Step 2 information is unavailable in CHMS.
#'
#' @examples
#' # Scalar usage: Single respondent
#' # Example: Male, drinks 3 drinks/week, drank in past year, no history of heavy drinking
#' low_drink_score_fun1(CLC_SEX = 1, ALC_11 = 1, ALCDWKY = 3, ALC_17 = 1, ALC_18 = 2)
#' # Expected output: 2
#'
#' # Missing data examples showing tagged NA patterns
#' result <- low_drink_score_fun1(CLC_SEX = 1, ALC_11 = 6, ALCDWKY = 5, ALC_17 = 1, ALC_18 = 2)
#' result # Shows: NA
#' haven::is_tagged_na(result, "a") # Shows: TRUE (confirms it's tagged NA(a))
#' format(result, tag = TRUE) # Shows: "NA(a)" (displays the tag)
#'
#' result <- low_drink_score_fun1(CLC_SEX = 1, ALC_11 = 7, ALCDWKY = 5, ALC_17 = 1, ALC_18 = 2)
#' result # Shows: NA
#' haven::is_tagged_na(result, "b") # Shows: TRUE (confirms it's tagged NA(b))
#' format(result, tag = TRUE) # Shows: "NA(b)" (displays the tag)
#'
#' result <- low_drink_score_fun1(CLC_SEX = 1, ALC_11 = 1, ALCDWKY = NA, ALC_17 = 1, ALC_18 = 2)
#' result # Shows: NA
#' haven::is_tagged_na(result, "b") # Shows: TRUE (confirms it's tagged NA(b))
#' format(result, tag = TRUE) # Shows: "NA(b)" (displays the tag)
#'
#' # Multiple respondents
#' low_drink_score_fun1(
#'   CLC_SEX = c(1, 2, 1), ALC_11 = c(1, 1, 2),
#'   ALCDWKY = c(3, 12, NA), ALC_17 = c(1, 1, 1), ALC_18 = c(2, 2, 1)
#' )
#' # Returns: c(2, 3, 2)
#'
#' # Database usage: Applied to survey datasets
#' library(dplyr)
#' # dataset %>%
#' #   mutate(low_drink_score1 = low_drink_score_fun1(CLC_SEX, ALC_11, ALCDWKY, ALC_17, ALC_18))
#'
#' @seealso [low_drink_score_fun()] for basic risk scoring without drinking history
#' @references Canada's Low-Risk Alcohol Drinking Guidelines, Health Canada
#' @export
low_drink_score_fun1 <- function(CLC_SEX, ALC_11, ALCDWKY, ALC_17, ALC_18) {
  step1 <- dplyr::case_when(
    # Sex and drinking alcohol within past year variables
    # Not applicable (takes precedence)
    CLC_SEX == 6 | ALC_11 == 6 ~ haven::tagged_na("a"),
    # Missing
    CLC_SEX %in% 7:9 | ALC_11 %in% 7:9 ~ haven::tagged_na("b"),
    # Invalid codes
    !CLC_SEX %in% c(1, 2) | !ALC_11 %in% c(1, 2) ~ haven::tagged_na("b"),

    # Drinks per week variable
    # Valid skip
    ALCDWKY == 996 ~ haven::tagged_na("a"),
    # Don't know, refusal, not stated
    ALCDWKY %in% 997:999 ~ haven::tagged_na("b"),

    # ALCDWKY validation when drinking in past year
    ALC_11 == 1 & (is.na(ALCDWKY) | ALCDWKY < 0 | ALCDWKY > 84) ~ haven::tagged_na("b"),

    # Logic for valid categorical values
    ALC_11 == 2 & is.na(ALCDWKY) ~ 0,
    ALCDWKY <= 10 ~ 0,
    ALCDWKY > 10 & ALCDWKY <= 15 & CLC_SEX == 1 ~ 0,
    ALCDWKY > 10 & ALCDWKY <= 15 & CLC_SEX == 2 ~ 1,
    ALCDWKY > 15 & ALCDWKY <= 20 & CLC_SEX == 1 ~ 1,
    ALCDWKY > 15 & ALCDWKY <= 20 & CLC_SEX == 2 ~ 3,
    ALCDWKY > 20 & CLC_SEX == 1 ~ 3,
    ALCDWKY > 20 & CLC_SEX == 2 ~ 5,
    .default = haven::tagged_na("b")
  )

  dplyr::case_when(
    # Propagate any tagged NAs from step1
    haven::is_tagged_na(step1, "a") ~ haven::tagged_na("a"),
    haven::is_tagged_na(step1, "b") | is.na(step1) ~ haven::tagged_na("b"),

    # Check ALC_17 / ALC_18 if needed
    step1 == 0 & (ALC_17 %in% c(6) | ALC_18 %in% c(6)) ~ haven::tagged_na("a"),
    step1 == 0 & (ALC_17 %in% 7:9 | ALC_18 %in% 7:9) ~ haven::tagged_na("b"),
    step1 == 0 & (!ALC_17 %in% c(1, 2) | !ALC_18 %in% c(1, 2)) ~ haven::tagged_na("b"),

    # Score assignment for valid step1 values
    step1 == 0 & ALC_17 == 2 & ALC_11 == 2 ~ 1L,
    step1 == 0 & ALC_17 == 1 & ALC_11 == 2 & ALC_18 == 2 ~ 1L,
    step1 == 0 & ALC_17 == 1 & ALC_11 == 2 & ALC_18 == 1 ~ 2L,
    step1 == 0 & ALC_11 == 1 ~ 2L,
    step1 %in% c(1, 2) ~ 3L,
    step1 %in% 3:9 ~ 4L,
    .default = haven::tagged_na("b")
  )
}
