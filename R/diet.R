#' @title Calculate daily fruit and vegetable consumption in a year for respondent in CHMS cycles 1-2.
#' 
#' @description This function calculates the daily fruit and vegetable consumption in a year for respondent in the Canadian Health Measures 
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
  
  totalFV <- 0
  
  if (all(is.na(c(WSDD14Y, GFVD17Y, GFVD18Y, GFVD19Y, GFVD20Y, GFVD22Y, GFVD23Y)))) {
    totalFV <- haven::tagged_na("b")
  }
  else {
    totalFV <- sum(c(WSDD14Y, GFVD17Y, GFVD18Y, GFVD19Y, GFVD20Y, GFVD22Y, GFVD23Y), na.rm = TRUE) / 365
  }
  
  return(totalFV)
  
}

#' @title Calculate daily fruit and vegetable consumption in a year for respondents in CHMS cycles 3-6.
#' 
#' @description This function calculates the daily fruit and vegetable consumption in a year for respondents in the Canadian Health Measures 
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
  
  totalFV <- 0
  
  if (all(is.na(c(WSDD34Y, WSDD35Y, GFVD17AY, GFVD17BY, GFVD17CY, GFVD17DY, GFVD18Y, GFVD19Y, GFVD20Y, GFVD22Y, GFVD23Y)))) {
    totalFV <- haven::tagged_na("b")
  }
  else {
    totalFV <- sum(c(WSDD34Y, WSDD35Y, GFVD17AY, GFVD17BY, GFVD17CY, GFVD17DY, GFVD18Y, GFVD19Y, GFVD20Y, GFVD22Y, GFVD23Y), na.rm = TRUE) / 365
  }
  return(totalFV)
}
      
#' @title Diet Categorization 
#'
#' @description This function categorizes individuals' diet quality based on their total fruit and vegetable consumption.
#'
#' @param totalFV Numeric value representing the average times per day fruits and vegetables were consumed in a year.
#'
#' @return A categorical value indicating the diet quality:
#'   - 1: Good diet (totalFV >= 5)
#'   - 2: Poor diet (totalFV < 5)
#'   - NA(b): Missing or invalid input
#'
#' @examples
#' # Example 1: Categorize a totalFV value of 3 as poor diet
#' determine_gooddiet(3)
#' # Output: 2
#' 
#' # Example 2: Categorize a totalFV value of 7 as good diet
#' determine_gooddiet(7)
#' # Output: 1
#'
#' @export
determine_gooddiet <- function(totalFV) {
  
  gooddiet <- 0
  
  if (is.na(totalFV)) {
    gooddiet <- haven::tagged_na("b")
  }
  else {
    if (totalFV >= 5) {
      gooddiet <- 1
    }
    else {
      gooddiet <- 2
    }
  }
  return(gooddiet)
  
}