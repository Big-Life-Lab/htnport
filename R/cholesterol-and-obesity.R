#' @title Calculate Non-HDL Cholesterol Level
#'
#' @description This function calculates a respondent's non-HDL cholesterol level by subtracting their HDL cholesterol level 
#' from their total cholesterol level. It first checks whether the input values `LAB_CHOL` (total cholesterol) 
#' and `LAB_HDL` (HDL cholesterol) are both less than certain thresholds (99.6 mmol/L and 9.96 mmol/L, respectively). 
#' If both conditions are met, it calculates the non-HDL cholesterol level; otherwise, it sets the non-HDL value to 
#' NA to indicate that the calculation is not applicable.
#'
#' @param LAB_CHOL A numeric representing a respondent's total cholesterol level in mmol/L.
#' @param LAB_HDL A numeric representing a respondent's HDL cholesterol level in mmol/L.
#'
#' @return A numeric representing the calculated non-HDL cholesterol level (in mmol.L) if both `LAB_CHOL` and 
#' `LAB_HDL` are below the specified thresholds; otherwise, it returns NA(b) to indicate that the calculation is not applicable.
#'
#' @details The function calculates the non-HDL cholesterol level by subtracting the HDL cholesterol level from the total cholesterol level.
#' It first checks if both `LAB_CHOL` and `LAB_HDL` are less than the specified thresholds (99.6 mmol/L and 9.96 mmol/L, respectively).
#' If both conditions are met and neither input is missing, the non-HDL cholesterol level is calculated. If either of the conditions
#' is not met or if either input is missing (NA), the function returns NA(b) to indicate that the calculation is not applicable.
#'
#' @examples
#'
#' # Example: Calculate non-HDL cholesterol level for a respondent with total cholesterol of 50 mmol/L and 
#' HDL cholesterol of 5 mmol/L.
#' calculate_nonHDL(LAB_CHOL = 50, LAB_HDL = 5)
#' # Output: 45 (non-HDL cholesterol = total cholesterol - HDL cholesterol = 50 - 5 = 45)
#' 
#' @export
calculate_nonHDL <- function(LAB_CHOL, LAB_HDL) {
  
  nonHDL <- 0
  
  if (LAB_CHOL < 99.6 && LAB_HDL < 9.96 && !is.na(LAB_CHOL) && !is.na(LAB_HDL)) {
    nonHDL <- LAB_CHOL - LAB_HDL
  }
  else {
    nonHDL <- haven::tagged_na("b")
  }
  
  return(nonHDL)
  
}

#' Non-HDL Cholesterol Categorization
#'
#' This function categorizes individuals' non-HDL cholesterol levels based on a threshold value.
#'
#' @param nonHDL Numeric value representing an individual's non-HDL cholesterol level.
#'
#' @return A categorical value indicating the non-HDL cholesterol category:
#'   - 1: High non-HDL cholesterol (nonHDL >= 4.3)
#'   - 2: Normal non-HDL cholesterol (nonHDL < 4.3)
#'   - NA(b): Missing or invalid input
#'
#' @examples
#' # Example 1: Categorize a nonHDL value of 5.0 as high non-HDL cholesterol
#' categorize_nonHDL(5.0)
#' # Output: 1
#' 
#' # Example 2: Categorize a nonHDL value of 3.8 as normal non-HDL cholesterol
#' categorize_nonHDL(3.8)
#' # Output: 2
#'
#' @export
categorize_nonHDL <- function(nonHDL) {
  
  nonhdltodd <- 0
  
  if (is.na(nonHDL)) {
    nonhdltodd <- haven::tagged_na("b")
  }
  else {
    if (nonHDL >= 4.3) {
      nonhdltodd <- 1
    }
    else {
      nonhdltodd <- 2
    }
  }
  return(nonhdltodd)
  
}

#' @title Calculate Waist-to-Height Ratio (WHR)
#'
#' @description
#' This function calculates the Waist-to-Height Ratio (WHR) by dividing the waist circumference by the height of the respondent.
#'
#' @param HWM_11CM A numeric value representing the height of the respondent in centimeters.
#' @param HWM_14CX A numeric value representing the waist circumference of the respondent in centimeters.
#'
#' @return A numeric value representing the WHR:
#'   - If both `HWM_11CM` and `HWM_14CX` are provided, the function returns the WHR (waist circumference divided by height).
#'   - If either `HWM_11CM` or `HWM_14CX` is missing, the function returns a tagged NA (`NA(b)`) indicating an invalid input or non-response.
#'
#' @examples
#' 
#' # Example 1: Calculate WHR for a respondent with height = 170 cm and waist circumference = 85 cm.
#' calculate_WHR(HWM_11CM = 170, HWM_14CX = 85)
#' # Output: 0.5 (85/170)
#'
#' # Example 2: Calculate WHR for a respondent with missing height.
#' calculate_WHR(HWM_11CM = NA, HWM_14CX = 85)
#' # Output: NA(b)
#' 
#' @export
calculate_WHR <- function(HWM_11CM, HWM_14CX) {
  
  WHR <- 0
  
  if (is.na(HWM_11CM) || is.na(HWM_14CX)) {
    WHR <- haven::tagged_na("b")
  }
  else {
    WHR <- HWM_14CX / HWM_11CM
  }
  
  return(WHR)
}