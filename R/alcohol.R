#' @title Low risk drinking score
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
    } else {
      step1 <- NA
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

#' @title Low risk drinking score - former/never categories
#'
#' @description
#' Computes a categorical alcohol consumption score based on Canada's Low-Risk Alcohol Drinking Guidelines (Step 1),
#' while distinguishing between never, former, light, moderate, and heavy drinkers. The function uses information
#' about weekly consumption, past-year use, lifetime drinking, and history of heavy drinking.
#'
#' @param CLC_SEX Integer. Respondent's sex (1 = male, 2 = female).
#' @param ALC_11 Integer. Whether the respondent drank alcohol in the past year (1 = Yes, 2 = No).
#' @param ALCDWKY Integer. Number of standard drinks consumed in a typical week (0–84).
#' @param ALC_17 Integer. Whether the respondent ever drank alcohol in their lifetime (1 = Yes, 2 = No).
#' @param ALC_18 Integer. Whether the respondent regularly drank more than 12 drinks per week (1 = Yes, 2 = No).
#'
#' @return An integer score:
#' \itemize{
#'   \item 1 = Never drank
#'   \item 2 = Low-risk (former or light) drinker
#'   \item 3 = Moderate drinker (1--2 points)
#'   \item 4 = Heavy drinker (3--4 points)
#' }
#' If inputs are invalid or out of bounds, the function returns a tagged NA.
#'
#' @details
#' \strong{Step 1: Assign points based on weekly alcohol consumption.}
#' \itemize{
#'   \item If the respondent drank in the past year (ALC_11 == 1):
#'     \itemize{
#'       \item 0 to 10 drinks/week: 0 points
#'       \item 11 to 15 drinks/week: 0 points for males, 1 point for females
#'       \item 16 to 20 drinks/week: 1 point for males, 3 points for females
#'       \item More than 20 drinks/week: 3 points for males, 5 points for females
#'     }
#'   \item If they did not drink in the past year (ALC_11 == 2): 0 points
#' }
#'
#' \strong{Step 2: Determine the final categorical score.}
#' \itemize{
#'   \item If the point score from Step 1 is 0, the final category is determined based on lifetime and past-year drinking habits:
#'     \itemize{
#'       \item A score of 1 (Never drinker) is assigned if the respondent either never drank alcohol in their lifetime or is a former drinker who did not regularly consume more than 12 drinks a week.
#'       \item A score of 2 (Low-risk drinker) is assigned if the respondent drank in the past year (but still scored 0 points) or is a former drinker with a history of regularly consuming more than 12 drinks a week.
#'     }
#'   \item If the point score from Step 1 is 1 or 2, the respondent is classified as a \strong{Moderate drinker} (Score = 3).
#'   \item If the point score from Step 1 is 3 or more, the respondent is classified as a \strong{Heavy drinker} (Score = 4).
#' }
#'
#' @note
#' This function uses only Step 1 of the guidelines, as Step 2 information is unavailable in CHMS.
#'
#' @examples
#' # Male, drinks 3 drinks/week, drank in past year, no history of heavy drinking
#' low_drink_score_fun1(CLC_SEX = 1, ALC_11 = 1, ALCDWKY = 3, ALC_17 = NA, ALC_18 = 2)
#' # Expected output: 2
#'
#' @export
low_drink_score_fun1 <- function(CLC_SEX, ALC_11, ALCDWKY, ALC_17, ALC_18) {
  ## Step 1: How many standard drinks did you have in a week?
  if (CLC_SEX %in% c(1, 2) && (!is.na(ALC_11) && ALC_11 == 1) && (!is.na(ALCDWKY) && ALCDWKY >= 0 && ALCDWKY <= 84)) {
    if (ALCDWKY <= 10) {
      step1 <- 0
    } else if (ALCDWKY > 10 && ALCDWKY <= 15) {
      if (CLC_SEX == 1) {
        step1 <- 0
      } else {
        step1 <- 1
      }
    } else if (ALCDWKY > 15 && ALCDWKY <= 20) {
      if (CLC_SEX == 1) {
        step1 <- 1
      } else {
        step1 <- 3
      }
    } else if (ALCDWKY > 20) {
      if (CLC_SEX == 1) {
        step1 <- 3
      } else {
        step1 <- 5
      }
    }
  } else if (CLC_SEX %in% c(1, 2) && (!is.na(ALC_11) && ALC_11 == 2) && is.na(ALCDWKY)) {
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
        if (!is.na(ALC_18) && ALC_18 == 2) {
          low_drink_score1 <- 1
        } else if (!is.na(ALC_18) && ALC_18 == 1) {
          low_drink_score1 <- 2
        } else {
          low_drink_score1 <- haven::tagged_na("b")
        }
      } else if (!is.na(ALC_11) && ALC_11 == 1) {
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
