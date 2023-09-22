#' @title Calculate daily fruit and vegetable consumption in a year for respondent in CHMS cycles 1-2.
#' 
#' This function calculates the daily fruit and vegetable consumption in a year for respondent in the Canadian Health Measures 
#' Survey (CHMS) cycles 1-2. It takes seven parameters, each representing the number of times per year a specific fruit or vegetable item
#' was consumed. The function then sums up the consumption frequencies of all these items and divides the total by 365 to
#' obtain the average daily consumption of fruits and vegetables in a year.
#'
#' @param WSDD14Y A numeric representing the number of times per year fruit juice was consumed.
#' @param GFVD17Y A numeric representing the number of times per year fruit (excluding juice) was consumed.
#' @param GFVD18Y A numeric representing the number of times per year tomato or tomato sauce was consumed.
#' @param GFVD19Y A numeric representing the number of times per year lettuce or green leafy salad was consumed.
#' @param GFVD20Y A numeric representing the number of times per year spinach, mustard greens, and cabbage were consumed.
#' @param GFVD22Y A numeric representing the number of times per year potatoes were consumed.
#' @param GFVD23Y A numeric representing the number of times per year other vegetables were consumed.
#'
#' @return A numeric representing the average times per day fruits and vegetables were consumed in a year.
#' 
#' @details The function calculates the total consumption of fruits and vegetables in a year by summing up the consumption
#'          frequencies of all the input items. It then divides the total by 365 to obtain the average daily consumption of
#'          fruits and vegetables in a year. NA(b) is only returned if all the parameters are missing or if the average ends
#'          up being NA.
#' 
#' @examples
#' 
#' # Example: Calculate the average daily fruit and vegetable consumption for one Cycle 1 or 2 respondent.
#' # Let's assume the following annual consumption frequencies for each item:
#' # WSDD14Y (fruit juice) = 50 times
#' # GFVD17Y (fruit, excluding juice) = 150 times
#' # GFVD18Y (tomato or tomato sauce) = 200 times
#' # GFVD19Y (lettuce or green leafy salad) = 100 times
#' # GFVD20Y (spinach, mustard greens, and cabbage) = 80 times
#' # GFVD22Y (potatoes) = 120 times
#' # GFVD23Y (other vegetables) = 90 times
#' # Using the function:
#' find_totalFV_cycles1and2(WSDD14Y = 50, GFVD17Y = 150, GFVD18Y = 200, GFVD19Y = 100, GFVD20Y = 80, GFVD22Y = 120, GFVD23Y = 90)
#' # Output: 2.164384
#' # The average daily consumption of fruits and vegetables in a year is approximately 2.16 times per day based on CHMS cycles 1-2 data.
#' 
#' @export
find_totalFV_cycles1and2 <- function(WSDD14Y, GFVD17Y, GFVD18Y, GFVD19Y, GFVD20Y, GFVD22Y, GFVD23Y) {
  
  if (all(is.na(c(WSDD14Y, GFVD17Y, GFVD18Y, GFVD19Y, GFVD20Y, GFVD22Y, GFVD23Y)))) {
    totalFV <- haven::tagged_na("b")
  }
  else {
    totalFV <- sum(c(WSDD14Y, GFVD17Y, GFVD18Y, GFVD19Y, GFVD20Y, GFVD22Y, GFVD23Y), na.rm = TRUE) / 365
    if (is.na(totalFV)) {
      totalFV <- haven::tagged_na("b")
    }
  }
  
  return(totalFV)
  
}

#' @title Calculate daily fruit and vegetable consumption in a year for respondents in CHMS cycles 3-6.
#' 
#' This function calculates the daily fruit and vegetable consumption in a year for respondents in the Canadian Health Measures 
#' Survey (CHMS) cycles 3-6. It takes eleven parameters, each representing the number of times per year a specific fruit or 
#' vegetable item was consumed. The function then sums up the consumption frequencies of all these items and divides the total
#' by 365 to obtain the average daily consumption of fruits and vegetables in a year.
#'
#' @param WSDD34Y A numeric representing the number of times per year orange or grapefruit juice was consumed.
#' @param WSDD35Y A numeric representing the number of times per year other fruit juices were consumed.
#' @param GFVD17AY A numeric representing the number of times per year citrus fruits were consumed.
#' @param GFVD17BY A numeric representing the number of times per year strawberries were consumed (in summer).
#' @param GFVD17CY A numeric representing the number of times per year strawberries were consumed (outside summer).
#' @param GFVD17DY A numeric representing the number of times per year other fruits were consumed.
#' @param GFVD18Y A numeric representing the number of times per year tomato or tomato sauce was consumed.
#' @param GFVD19Y A numeric representing the number of times per year lettuce or green leafy salad was consumed.
#' @param GFVD20Y A numeric representing the number of times per year spinach, mustard greens, and cabbage were consumed.
#' @param GFVD22Y A numeric representing the number of times per year potatoes were consumed.
#' @param GFVD23Y A numeric representing the number of times per year other vegetables were consumed.
#'
#' @return A numeric representing the average times per day fruits and vegetables were consumed in a year.
#' 
#' @details The function calculates the total consumption of fruits and vegetables in a year by summing up the consumption
#'          frequencies of all the input items. It then divides the total by 365 to obtain the average daily consumption of
#'          fruits and vegetables in a year. NA(b) is only returned if all the parameters are missing or if the average ends
#'          up being NA.
#' 
#' @examples
#' 
#' # Example: Calculate the average daily fruit and vegetable consumption for a respondent in CHMS cycles 3-6.
#' # Let's assume the following annual consumption frequencies for each item:
#' # WSDD34Y (orange or grapefruit juice) = 50 times
#' # WSDD35Y (other fruit juices) = 100 times
#' # GFVD17AY (citrus fruits) = 150 times
#' # GFVD17BY (strawberries in summer) = 80 times
#' # GFVD17CY (strawberries outside summer) = 40 times
#' # GFVD17DY (other fruits) = 200 times
#' # GFVD18Y (tomato or tomato sauce) = 100 times
#' # GFVD19Y (lettuce or green leafy salad) = 80 times
#' # GFVD20Y (spinach, mustard greens, and cabbage) = 60 times
#' # GFVD22Y (potatoes) = 120 times
#' # GFVD23Y (other vegetables) = 90 times
#' # Using the function:
#' find_totalFV_cycles3to6(WSDD34Y = 50, WSDD35Y = 100, GFVD17AY = 150, GFVD17BY = 80, GFVD17CY = 40, GFVD17DY = 200,
#'                         GFVD18Y = 100, GFVD19Y = 80, GFVD20Y = 60, GFVD22Y = 120, GFVD23Y = 90)
#' # Output: 2.931507
#' # The average daily consumption of fruits and vegetables in a year for this respondent is approximately 2.91 times per day based on CHMS cycles 3-6 data.
#' 
#' @export
find_totalFV_cycles3to6 <- function(WSDD34Y, WSDD35Y, GFVD17AY, GFVD17BY, GFVD17CY, GFVD17DY, GFVD18Y, GFVD19Y, GFVD20Y, GFVD22Y, GFVD23Y) {
  
  if (all(is.na(c(WSDD34Y, WSDD35Y, GFVD17AY, GFVD17BY, GFVD17CY, GFVD17DY, GFVD18Y, GFVD19Y, GFVD20Y, GFVD22Y, GFVD23Y)))) {
    totalFV <- haven::tagged_na("b")
  }
  else {
    totalFV <- sum(c(WSDD34Y, WSDD35Y, GFVD17AY, GFVD17BY, GFVD17CY, GFVD17DY, GFVD18Y, GFVD19Y, GFVD20Y, GFVD22Y, GFVD23Y), na.rm = TRUE) / 365
    if (is.na(totalFV)) {
      totalFV <- haven::tagged_na("b")
    }
  }
  return(totalFV)
}
      
#' Poor Diet Categorization 
#'
#' This function categorizes individuals' diet quality based on their total fruit and vegetable consumption.
#'
#' @param totalFV Numeric value representing the average times per day fruits and vegetables were consumed in a year.
#'
#' @return A categorical value indicating the diet quality:
#'   - 1: Poor diet (totalFV < 5)
#'   - 2: Good diet (totalFV >= 5)
#'   - NA(b): Missing or invalid input
#'
#' @examples
#' # Example 1: Categorize a totalFV value of 3 as poor diet
#' determine_poordiet(3)
#' # Output: 1
#' 
#' # Example 2: Categorize a totalFV value of 7 as good diet
#' determine_poordiet(7)
#' # Output: 2
#'
#' @export
determine_poordiet <- function(totalFV) {
  
  poordiet <- haven::tagged_na("b")
  
  if (is.na(totalFV)) {
    return(poordiet)
  }
  else {
    if (totalFV < 5) {
      poordiet <- 1
    }
    else {
      poordiet <- 2
    }
  }
  return(poordiet)
  
}

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
  
  nonhdltodd <- haven::tagged_na("b")
  
  if (is.na(nonHDL)) {
    return(nonhdltodd)
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