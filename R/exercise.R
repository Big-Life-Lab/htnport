#' @title Average minutes of exercise per day for week-long accelerometer data
#'
#' @description This function calculates the average minutes of exercise per day across a week of accelerometer data. It takes seven
#' parameters, each representing the minutes of exercise on a specific day (Day 1 to Day 7) of accelerometer measurement.
#' The function computes the average of these values to obtain the average minutes of exercise per day.
#'
#' @param AMMDMVA1 [numeric] A numeric representing minutes of exercise on Day 1 of accelerometer measurement.
#' @param AMMDMVA2 [numeric] A numeric representing minutes of exercise on Day 2 of accelerometer measurement.
#' @param AMMDMVA3 [numeric] A numeric representing minutes of exercise on Day 3 of accelerometer measurement.
#' @param AMMDMVA4 [numeric] A numeric representing minutes of exercise on Day 4 of accelerometer measurement.
#' @param AMMDMVA5 [numeric] A numeric representing minutes of exercise on Day 5 of accelerometer measurement.
#' @param AMMDMVA6 [numeric] A numeric representing minutes of exercise on Day 6 of accelerometer measurement.
#' @param AMMDMVA7 [numeric] A numeric representing minutes of exercise on Day 7 of accelerometer measurement.
#'
#' @return [numeric] The average minutes of exercise per day across a week of accelerometer use. If inputs are invalid or out of bounds, the function returns a tagged NA.
#'
#' @details This function processes physical activity data from accelerometer measurements
#'          to create a weekly activity summary.
#'
#'          **Data Quality Requirements:**
#'          - Requires complete 7-day data (missing days result in tagged NA)
#'          - This conservative approach ensures reliable activity estimates
#'          - Zero values are preserved (represent valid no-activity days)
#'
#'          **Missing Data Codes:**
#'          - For all input variables:
#'            - `9996`: Valid skip. Handled as `haven::tagged_na("a")`.
#'            - `9997-9999`: Don't know, refusal, or not stated. Handled as `haven::tagged_na("b")`.
#'
#' @examples
#' # Scalar usage: Single respondent
#' # Example: Calculate the average minutes of exercise per day for a week of accelerometer data.
#' find_week_accelerometer_average(30, 40, 25, 35, 20, 45, 50)
#' # Output: 35
#'
#' # Example: Respondent has non-response values for all inputs.
#' result <- find_week_accelerometer_average(9998, 9998, 9998, 9998, 9998, 9998, 9998)
#' result # Shows: NA
#' haven::is_tagged_na(result, "b") # Shows: TRUE (confirms it's tagged NA(b))
#' format(result, tag = TRUE) # Shows: "NA(b)" (displays the tag)
#'
#' # Multiple respondents
#' find_week_accelerometer_average(
#'   c(30, 20), c(40, 30), c(25, 35), c(35, 45),
#'   c(20, 25), c(45, 55), c(50, 60)
#' )
#' # Returns: c(35, 39.28571)
#'
#' @seealso [minperday_to_minperweek()] for activity unit conversion, [categorize_minperweek()] for activity level classification
#' @export
find_week_accelerometer_average <- function(AMMDMVA1, AMMDMVA2, AMMDMVA3, AMMDMVA4, AMMDMVA5, AMMDMVA6, AMMDMVA7) {
  # Combine all measurements into a data frame
  measurements <- data.frame(AMMDMVA1, AMMDMVA2, AMMDMVA3, AMMDMVA4, AMMDMVA5, AMMDMVA6, AMMDMVA7)

  # Replace missing data codes with NA
  measurements[measurements == 9996] <- haven::tagged_na("a") # Valid skip
  measurements[measurements >= 9997] <- haven::tagged_na("b") # Don't know, refusal, not stated

  # Calculate the average minutes of moderate-to-vigorous physical activity
  MVPA_min <- rowMeans(measurements, na.rm = FALSE)

  # Handle cases with all missing data or negative values
  dplyr::case_when(
    rowSums(is.na(measurements)) == ncol(measurements) ~ haven::tagged_na("b"),
    rowSums(measurements < 0, na.rm = TRUE) > 0 ~ haven::tagged_na("b"),
    TRUE ~ MVPA_min
  )
}

#' @title Minutes per week from minutes per day
#'
#' @description This function takes the average minutes of exercise per day across a week of accelerometer use as an input (`MVPA_min`) and
#' calculates the equivalent minutes of exercise per one week of accelerometer use. The result is returned as a numeric value.
#'
#' @param MVPA_min [numeric] A numeric representing the average minutes of exercise per day across a week of accelerometer use.
#'
#' @return [numeric] The average minutes of exercise per one week of accelerometer use. If inputs are invalid or out of bounds, the function returns a tagged NA.
#'
#' @details The function simply multiplies the average minutes of exercise per day (`MVPA_min`) by 7 to obtain the equivalent
#'          minutes of exercise per one week of accelerometer use.
#'
#'          **Missing Data Codes:**
#'          - Propagates tagged NAs from the input `MVPA_min`.
#'
#' @examples
#' # Scalar usage: Single respondent
#' # Example: Convert average minutes of exercise per day to minutes per week.
#' minperday_to_minperweek(35)
#' # Output: 245
#'
#' # Multiple respondents
#' minperday_to_minperweek(c(35, 40, 20))
#' # Returns: c(245, 280, 140)
#'
#' # Database usage: Applied to survey datasets
#' library(dplyr)
#' # dataset %>%
#' #   mutate(min_per_week = minperday_to_minperweek(avg_exercise))
#'
#' @seealso [find_week_accelerometer_average()], [categorize_minperweek()]
#' @export
minperday_to_minperweek <- function(MVPA_min) {
  # Calculate minutes per week
  minperweek <- MVPA_min * 7

  # Handle tagged NAs and negative values
  dplyr::case_when(
    # Valid skip
    haven::is_tagged_na(MVPA_min, "a") ~ haven::tagged_na("a"),
    # Don't know, refusal, not stated
    haven::is_tagged_na(MVPA_min, "b") | MVPA_min < 0 ~ haven::tagged_na("b"),
    TRUE ~ minperweek
  )
}

#' @title Categorical weekly physical activity indicator
#'
#' @description This function categorizes individuals' weekly physical activity levels based on a threshold value.
#'
#' @param minperweek [numeric] A numeric representing an individual's minutes of moderate-to-vigorous
#'   physical activity (MVPA) per week.
#'
#' @return [integer] A categorical indicating the physical activity category:
#'   - 1: Meets or exceeds the recommended 150 minutes of MVPA per week (minperweek >= 150)
#'   - 2: Below the recommended 150 minutes of MVPA per week (minperweek < 150)
#'   - `haven::tagged_na("a")`: Not applicable
#'   - `haven::tagged_na("b")`: Missing
#'
#' @details This function applies the national physical activity guideline of 150 minutes of moderate-to-vigorous physical activity (MVPA) per week.
#'
#'          **Missing Data Codes:**
#'          - Propagates tagged NAs from the input `minperweek`.
#'
#' @examples
#' # Scalar usage: Single respondent
#' # Example 1: Categorize 180 minutes of MVPA per week as meeting the recommendation
#' categorize_minperweek(180)
#' # Output: 1
#'
#' # Example 2: Categorize 120 minutes of MVPA per week as below the recommendation
#' categorize_minperweek(120)
#' # Output: 2
#'
#' # Multiple respondents
#' categorize_minperweek(c(180, 120, 150))
#' # Returns: c(1, 2, 1)
#'
#' # Database usage: Applied to survey datasets
#' library(dplyr)
#' # dataset %>%
#' #   mutate(pa_category = categorize_minperweek(min_per_week))
#'
#' @seealso [minperday_to_minperweek()]
#' @export
categorize_minperweek <- function(minperweek) {
  dplyr::case_when(
    # Valid skip
    haven::is_tagged_na(minperweek, "a") ~ haven::tagged_na("a"),
    # Don't know, refusal, not stated
    haven::is_tagged_na(minperweek, "b") | minperweek < 0 ~ haven::tagged_na("b"),

    # Categorize physical activity level
    minperweek >= 150 ~ 1,
    minperweek < 150 ~ 2,

    # Handle any other cases
    .default = haven::tagged_na("b")
  )
}
