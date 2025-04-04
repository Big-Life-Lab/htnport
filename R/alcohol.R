#' @title Calculate the Low Drink Score for a CHMS Respondent Based on Alcohol Consumption
#'
#' @description 
#' This function calculates a low drink score (step 1 only) for a respondent using 
#' Canada's Low-Risk Alcohol Drinking Guideline. The score is based solely on the 
#' number of standard drinks consumed per week and the respondent's sex. (Step 2, 
#' which would add additional points based on other drinking habits, is not included.)
#'
#' @param CLC_SEX An integer indicating the respondent's sex (1 for male, 2 for female).
#' @param ALC_11 An integer indicating whether the respondent drank alcohol in the past year (1 for "Yes", 2 for "No").
#' @param ALCDWKY An integer representing the number of standard drinks consumed by the respondent in a week.
#'
#' @return An integer representing the low drink score, with:
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
#'     - For ALCDWKY ≤ 10, assign 0 points.
#'     - For ALCDWKY > 10 and ≤ 15: assign 0 points for males (CLC_SEX == 1) and 1 point for females (CLC_SEX == 2).
#'     - For ALCDWKY > 15 and ≤ 20: assign 1 point for males and 3 points for females.
#'     - For ALCDWKY > 20: assign 3 points.
#'   - For respondents who did not drink in the past year (ALC_11 == 2), 0 points are assigned.
#'
#' These `step1` points are then mapped to the final categorical score as follows:
#'   - 0 points → score of 1 ("Low risk"),
#'   - 1–2 points → score of 2 ("Marginal risk"),
#'   - 3–4 points → score of 3 ("Medium risk"),
#'   - 5–9 points → score of 4 ("High risk").
#'
#' @note 
#' This function does not include the additional points from step 2 of the guideline.
#'
#' @examples
#' # Example: A male respondent who drank in the past year and consumes 3 standard drinks per week.
#' low_drink_score_fun(CLC_SEX = 1, ALC_11 = 1, ALCDWKY = 3)
#' # Expected output: 1 (Low risk)
#'
#' @export
low_drink_score_fun <- function(CLC_SEX, ALC_11, ALCDWKY) {
  
  ## Step 1: How many standard drinks did you have in a week?
  if (CLC_SEX %in% c(1, 2) && !is.na(ALC_11) && ALC_11 == 1) {
    if (!is.na(ALCDWKY) && ALCDWKY <= 10) {
      step1 <- 0
    } else if (CLC_SEX == 1 && !is.na(ALCDWKY) && ALCDWKY > 10 && ALCDWKY <= 15) {
      step1 <- 0
    } else if (CLC_SEX == 2 && !is.na(ALCDWKY) && ALCDWKY > 10 && ALCDWKY <= 15) {
      step1 <- 1
    } else if (CLC_SEX == 1 && !is.na(ALCDWKY) && ALCDWKY > 15 && ALCDWKY <= 20) {
      step1 <- 1
    } else if (CLC_SEX == 2 && !is.na(ALCDWKY) && ALCDWKY > 15 && ALCDWKY <= 20) {
      step1 <- 3
    } else if (!is.na(ALCDWKY) && ALCDWKY > 20) {
      step1 <- 3
    }
  } else if (CLC_SEX %in% c(1, 2) && !is.na(ALC_11) && ALC_11 == 2) {
    step1 <- 0
  } else {
    step1 <- NA
  }
  
  ## Categorical score
  low_drink_score <- 0
  if (!is.na(step1)) {
    if (step1 == 0) {
      low_drink_score <- 1
    } else if (step1 %in% 1:2) {
      low_drink_score <- 2
    } else if (step1 %in% 3:4) {
      low_drink_score <- 3
    } else if (step1 %in% 5:9) {
      low_drink_score <- 4
    }
  } else {
    low_drink_score <- haven::tagged_na("b")
  }
  
  return(low_drink_score)
}

#' @title Calculate the Low Drink Score for a Respondent Based on Alcohol Consumption (Including Former and Never Categories)
#'
#' @description 
#' This function calculates a low drink score using Canada's Low-Risk Alcohol Drinking Guideline 
#' (step 1 only) while also differentiating between never drinkers and former drinkers. The score 
#' is based on the respondent's weekly alcohol consumption, whether they drank in the past year, 
#' and their lifetime drinking history.
#'
#' @param CLC_SEX An integer indicating the respondent's sex (1 for male, 2 for female).
#' @param ALC_11 An integer indicating whether the respondent drank alcohol in the past year (1 for "Yes", 2 for "No").
#' @param ALCDWKY An integer representing the number of standard drinks consumed in a week.
#' @param ALC_17 An integer indicating whether the respondent ever drank alcohol in their lifetime 
#'              (1 for "Yes", 2 for "No"). Note: if this value is missing (NA) for respondents who 
#'              drank in the past year, the function defaults to treating them as former drinkers.
#'
#' @return An integer representing the low drink score, defined as follows:
#'   - 1: Never drank (0 points)
#'   - 2: Former drinker (0 points)
#'   - 3: Light drinker (1–2 points)
#'   - 4: Moderate-to-heavy drinker (3–4 points)
#'   - NA: If any input is invalid or contains non-response values.
#'
#' @details 
#' The function calculates the score in two steps:
#' 
#' **Step 1: Points Allocation Based on Weekly Consumption**
#' - For respondents who drank in the past year (ALC_11 == 1):
#'   - If ALCDWKY is 10 or fewer, assign 0 points.
#'   - If ALCDWKY is greater than 10 but ≤ 15:
#'     - Males (CLC_SEX == 1) receive 0 points.
#'     - Females (CLC_SEX == 2) receive 1 point.
#'   - If ALCDWKY is greater than 15 but ≤ 20:
#'     - Males receive 1 point.
#'     - Females receive 3 points.
#'   - If ALCDWKY is greater than 20:
#'     - Males receive 3 points.
#'     - Females receive 5 points.
#'
#' - For respondents who did not drink in the past year (ALC_11 == 2), 0 points are assigned.
#'
#' **Step 2: Deriving the Final Categorical Score**
#' - For respondents with 0 points in Step 1:
#'   - If they never drank in their lifetime (ALC_17 == 2) and did not drink in the past year, the score is 1.
#'   - If they have a history of drinking (ALC_17 == 1) or if ALC_17 is missing for respondents who drank in the past year, 
#'     the score is 2.
#' - For respondents with 1 or 2 points in Step 1, the final score is 3 (marginal risk).
#' - For respondents with 3 or more points in Step 1 (specifically, between 3 and 9), the final score is 4 (moderate-to-heavy risk).
#'
#' @note 
#' This function is based only on Step 1 of the guideline since additional drinking habit questions (Step 2) 
#' are not available in the CHMS cycles.
#'
#' @examples
#' # Example: A male respondent who consumed 3 standard drinks per week, drank in the past year, 
#' low_drink_score_fun1(CLC_SEX = 1, ALC_11 = 1, ALCDWKY = 3, ALC_17 = NA)
#' # Expected output: 2
#'
#' @export
low_drink_score_fun1 <- function(CLC_SEX, ALC_11, ALCDWKY, ALC_17) {
  
  ## Step 1: How many standard drinks did you have in a week?
  if (CLC_SEX %in% c(1, 2) && !is.na(ALC_11) && ALC_11 == 1) {
    if (!is.na(ALCDWKY) && ALCDWKY <= 10) {
      step1 <- 0
    } else if (!is.na(ALCDWKY) && ALCDWKY > 10 && ALCDWKY <= 15) {
      if (CLC_SEX == 1) {
        step1 <- 0
      } else {
        step1 <- 1
      }
    } else if (!is.na(ALCDWKY) && ALCDWKY > 15 && ALCDWKY <= 20) {
      if (CLC_SEX == 1) {
        step1 <- 1
      } else {
        step1 <- 3
      }
    } else if (!is.na(ALCDWKY) && ALCDWKY > 20) {
      if (CLC_SEX == 1) {
        step1 <- 3
      } else {
        step1 <- 5
      }
    }
  } else if (CLC_SEX %in% c(1, 2) && !is.na(ALC_11) && ALC_11 == 2) {
    step1 <- 0
  } else {
    step1 <- NA
  }
  
  ## Step 2: Calculate the Final Categorical Score
  if (!is.na(step1)) {
    if (step1 == 0) {
      if (!is.na(ALC_17) && ALC_17 == 2 && !is.na(ALC_11) && ALC_11 == 2) {
        low_drink_score1 <- 1
      } else if (!is.na(ALC_17) && ALC_17 == 1 && !is.na(ALC_11) && ALC_11 == 2) {
        low_drink_score1 <- 2
      } else if (is.na(ALC_17) && !is.na(ALC_11) && ALC_11 == 1) {
        low_drink_score1 <- 2
      } else {
        low_drink_score1 <- haven::tagged_na("b")
      }
    } else if (step1 %in% c(1, 2)) {
      low_drink_score1 <- 3
    } else if (step1 %in% 3:9) {
      low_drink_score1 <- 4
    } else {
      low_drink_score1 <- haven::tagged_na("b")
    }
  } else {
    low_drink_score1 <- haven::tagged_na("b")
  }
  
  return(low_drink_score1)
}