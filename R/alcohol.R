#' @title Calculate the low drink score for a CHMS respondent based on alcohol consumption.
#'
#' @param CLC_SEX An integer indicating the respondent's sex. 1 for male, 2 for female.
#' @param ALCDWKY An integer representing the number of standard drinks consumed by the respondent in a week.
#'
#' @return An integer representing the low drink score:
#'   - 1 for "Low risk" (0 points).
#'   - 2 for "Marginal risk" (1-2 points).
#'   - 3 for "Medium risk" (3-4 points).
#'   - 4 for "High risk" (5-9 points).
#'   - NA for invalid input or non-response values.
#'
#' @details The low risk drinking score is based on the scoring system in 
#' Canada's Low-Risk Alcohol Drinking Guideline. The score is divided into two 
#' steps. Step 1 allocates points based on sex and the number of drinks 
#' that you usually have each week. In step 2, one point will be awarded for 
#' each item that is true related to drinking habits. The total score is 
#' obtained from adding the points in step 1 and step 2.
#' 
#' @note Step 2 is not included in this function because the questions in 
#' step 2 are not asked in any of the CHMS cycles. The score is only based on 
#' step 1.
#'
#' @examples
#' 
#' # Example: Determine low drink score for a male respondent who consumed 3 standard drinks per week.
#' low_drink_score_fun(CLC_SEX = 1, ALCDWKY = 3)
#' # Output: 1 (Low risk due to consuming 3 standard drinks per week).
#' 
#' @export
low_drink_score_fun <- function(CLC_SEX, ALCDWKY) {
    ## Step 1: How many standard drinks did you have in a week?
    step1 <- NA
    if (CLC_SEX %in% c(1, 2) && ALCDWKY %in% 0:995) {
      if (ALCDWKY <= 10) {
        step1 <- 0
      } else if (CLC_SEX == 1 && ALCDWKY > 10 && ALCDWKY <= 15) {
        step1 <- 0
      } else if (CLC_SEX == 2 && ALCDWKY > 10 && ALCDWKY <= 15) {
        step1 <- 1
      } else if (CLC_SEX == 1 && ALCDWKY > 15 && ALCDWKY <= 20) {
        step1 <- 1
      } else if (CLC_SEX == 2 && ALCDWKY > 15 && ALCDWKY <= 20) {
        step1 <- 3
      } else if (ALCDWKY > 20) {
        step1 <- 3
      } else {
        step1 <- NA
      }
    }
    else if (is.na(CLC_SEX) || is.na(ALCDWKY)) {
      low_drink_score <- haven::tagged_na("b")
    }
    
    ## Categorical score
    low_drink_score <- NA
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
    }
    
    return(low_drink_score)
  }

#' @title Calculate the low drink score for a respondent based on alcohol consumption (with former and never categories included).
#'
#' @param CLC_SEX An integer indicating the respondent's sex. 1 for male, 2 for female.
#' @param ALCDWKY An integer representing the number of standard drinks consumed by the respondent in a week.
#' @param ALC_17 An integer indicating whether the respondent ever drank in their lifetime. 
#'              1 for "Yes", 2 for "No".
#' @param ALC_11 An integer indicating whether the respondent drank alcohol in the past year. 
#'              1 for "Yes", 2 for "No".
#'
#' @return An integer representing the low drink score:
#'   - 1 - Low risk - never drank (0 points)
#'   - 2 - Low risk - former drinker (0 points)
#'   - 3 - Marginal risk (1-2 points)
#'   - 4 - Medium risk (3-4 points)
#'   - 5 - High risk (5-9 points)
#'   - NA if any input contains non-response values or is invalid.

#' @details The low risk drinking score is based on the scoring system in 
#' Canada's Low-Risk Alcohol Drinking Guideline. The score is divided into two 
#' steps. Step 1 allocates points based on sex and the number of drinks 
#' that you usually have each week. In step 2, one point will be awarded for 
#' each item that is true related to drinking habits. The total score is 
#' obtained from adding the points in step 1 and step 2.
#' 
#' This score has two 0 point categories: low risk (never drank) and low risk 
#' (former drinker). The two drinking groups are derived from 'ever had a drink 
#' in lifetime'. 'Ever had a drink in lifetime' is only available in CCHS 
#' 2001-2008 and 2015-2018.
#' 
#' 
#' @note Step 2 is not included in this function because the questions in 
#' step 2 are not asked in any of the CHMS cycles. The score is only based on 
#' step 1.
#'
#' @examples
#' 
#' # Example: Determine low drink score for a male respondent who consumed 3 standard drinks per week and drank within past year.
#' low_drink_score_fun1(CLC_SEX = 1, ALCDWKY = 3, ALC_17 = 1, ALC_11 = 1)
#' # Output: 2 (Low risk due to consuming 3 standard drinks per week and having alcohol before).
#' 
#' @export
low_drink_score_fun1 <- function(CLC_SEX, ALCDWKY, ALC_17, ALC_11) {
    ## Step 1: How many standard drinks did you have in a week?
    step1 <- NA
    if (CLC_SEX %in% c(1, 2) && ALCDWKY %in% 0:995 && ALC_17 %in% c(1, 2) && ALC_11 %in% c(1, 2)) {
      if (ALCDWKY <= 10) {
        step1 <- 0
      } else if (CLC_SEX == 1 && ALCDWKY > 10 && ALCDWKY <= 15) {
        step1 <- 0
      } else if (CLC_SEX == 2 && ALCDWKY > 10 && ALCDWKY <= 15) {
        step1 <- 1
      } else if (CLC_SEX == 1 && ALCDWKY > 15 && ALCDWKY <= 20) {
        step1 <- 1
      } else if (CLC_SEX == 2 && ALCDWKY > 15 && ALCDWKY <= 20) {
        step1 <- 3
      } else if (ALCDWKY > 20) {
        step1 <- 3
      } else {
        step1 <- NA
      }
    }
    else if (is.na(CLC_SEX) || is.na(ALCDWKY) || is.na(ALC_17) || is.na(ALC_11)) {
      low_drink_score1 <- haven::tagged_na("b")
    }
    
    ## Categorical score
    low_drink_score1 <- NA
    if (!is.na(step1) && ALC_17 %in% c(1, 2) && ALC_11 %in% c(1, 2)) {
      if (step1 == 0 && ALC_17 == 2) {
        low_drink_score1 <- 1
      } else if (step1 == 0 && ALC_17 == 1) {
        low_drink_score1 <- 2
      } else if (step1 %in% c(1, 2)) {
        low_drink_score1 <- 3
      } else if (step1 %in% c(3, 4)) {
        low_drink_score1 <- 4
      } else if (step1 %in% 5:9) {
        low_drink_score1 <- 5
      } else {
        low_drink_score1 <- haven::tagged_na("b")
      }
    }
    
    return(low_drink_score1)
  }
