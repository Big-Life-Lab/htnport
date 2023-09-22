#' @title Calculate the average minutes of exercise per day for week-long accelerometer data. 
#' 
#' This function calculates the average minutes of exercise per day across a week of accelerometer data. It takes seven
#' parameters, each representing the minutes of exercise on a specific day (Day 1 to Day 7) of accelerometer measurement.
#' The function computes the average of these values to obtain the average minutes of exercise per day.
#'
#' @param AMMDMVA1 A numeric representing minutes of exercise on Day 1 of accelerometer measurement. 
#' @param AMMDMVA2 A numeric representing minutes of exercise on Day 2 of accelerometer measurement. 
#' @param AMMDMVA3 A numeric representing minutes of exercise on Day 3 of accelerometer measurement. 
#' @param AMMDMVA4 A numeric representing minutes of exercise on Day 4 of accelerometer measurement. 
#' @param AMMDMVA5 A numeric representing minutes of exercise on Day 5 of accelerometer measurement. 
#' @param AMMDMVA6 A numeric representing minutes of exercise on Day 6 of accelerometer measurement. 
#' @param AMMDMVA7 A numeric representing minutes of exercise on Day 7 of accelerometer measurement. 
#'
#' @return A numeric representing the average minutes of exercise per day across a week of accelerometer use.
#' 
#' @details The function calculates the average minutes of exercise per day by taking the mean of the seven input parameters.
#' 
#' @examples
#' 
#' # Example: Calculate the average minutes of exercise per day for a week of accelerometer data.
#' find_week_accelerometer_average(30, 40, 25, 35, 20, 45, 50)
#' # Output: 35 (The average minutes of exercise per day across the week is 35 minutes.)
#' 
#' @export
find_week_accelerometer_average <- function(AMMDMVA1, AMMDMVA2, AMMDMVA3, AMMDMVA4, AMMDMVA5, AMMDMVA6, AMMDMVA7) {
  
  measurements <- c(AMMDMVA1, AMMDMVA2, AMMDMVA3, AMMDMVA4, AMMDMVA5, AMMDMVA6, AMMDMVA7)
  MVPA_min <- mean(measurements, na.rm = FALSE)
  if (is.na(MVPA_min)) {
    MVPA_min <- haven::tagged_na("b")
  }
  return(MVPA_min)
  
}

#' @title Convert minutes per day to minutes per week. 
#' 
#' This function takes the average minutes of exercise per day across a week of accelerometer use as an input (`MVPA_min`) and
#' calculates the equivalent minutes of exercise per one week of accelerometer use. The result is returned as a numeric value.
#'
#' @param MVPA_min A numeric representing the average minutes of exercise per day across a week of accelerometer use. 
#'
#' @return A numeric representing the average minutes of exercise per one week of accelerometer use.
#' 
#' @details The function simply multiplies the average minutes of exercise per day (`MVPA_min`) by 7 to obtain the equivalent
#'          minutes of exercise per one week of accelerometer use.
#' 
#' @examples
#' 
#' # Example: Convert average minutes of exercise per day to minutes per week.
#' minperday_to_minperweek(35)
#' # Output: 245 (The equivalent minutes of exercise per one week is 245 minutes.)
#' 
#' @export
minperday_to_minperweek <- function(MVPA_min) {
  
  minperweek <- MVPA_min * 7
  if (is.na(minperweek)) {
    minperweek <- haven::tagged_na("b")
  }
  return(minperweek)

}

#' Categorize Weekly Physical Activity
#'
#' This function categorizes individuals' weekly physical activity levels based on a threshold value.
#'
#' @param minperweek Numeric value representing an individual's minutes of moderate-to-vigorous
#'   physical activity (MVPA) per week.
#'
#' @return A categorical value indicating the physical activity category:
#'   - 1: Meets or exceeds the recommended 150 minutes of MVPA per week (minperweek >= 150)
#'   - 2: Below the recommended 150 minutes of MVPA per week (minperweek < 150)
#'   - NA(b): Missing or invalid input
#'
#' @examples
#' # Example 1: Categorize 180 minutes of MVPA per week as meeting the recommendation
#' categorize_minperweek(180)
#' # Output: 1
#' 
#' # Example 2: Categorize 120 minutes of MVPA per week as below the recommendation
#' categorize_minperweek(120)
#' # Output: 2
#'
#' @export
categorize_minperweek <- function(minperweek) {
  
  mvpa150wk <- haven::tagged_na("b")
  
  if (is.na(minperweek)) {
    return(mvpa150wk)
  }
  else {
    if (minperweek >= 150) {
      mvpa150wk <- 1
    }
    else {
      mvpa150wk <- 2
    }
  }
  return(mvpa150wk)
  
}