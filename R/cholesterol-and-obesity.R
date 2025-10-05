#' @title Non-HDL cholesterol level
#'
#' @description This function calculates a respondent's non-HDL cholesterol level by subtracting their HDL cholesterol level
#' from their total cholesterol level. It first checks whether the input values `LAB_CHOL` (total cholesterol)
#' and `LAB_HDL` (HDL cholesterol) are within valid ranges.
#'
#' @param LAB_CHOL [numeric] A numeric representing a respondent's total cholesterol level in mmol/L.
#' @param LAB_HDL [numeric] A numeric representing a respondent's HDL cholesterol level in mmol/L.
#'
#' @return [numeric] The calculated non-HDL cholesterol level (in mmol/L). If inputs are invalid or out of bounds, the function returns a tagged NA.
#'
#' @details The function calculates the non-HDL cholesterol level by subtracting the HDL cholesterol level from the total cholesterol level.
#'
#'          **Missing Data Codes:**
#'          - `LAB_CHOL`:
#'            - `99.96`: Valid skip. Handled as `haven::tagged_na("a")`.
#'            - `99.97-99.99`: Don't know, refusal, or not stated. Handled as `haven::tagged_na("b")`.
#'          - `LAB_HDL`:
#'            - `9.96`: Valid skip. Handled as `haven::tagged_na("a")`.
#'            - `9.97-9.99`: Don't know, refusal, or not stated. Handled as `haven::tagged_na("b")`.
#'
#' @examples
#' # Scalar usage: Single respondent
#' # Example: Respondent has total cholesterol of 5.0 mmol/L and HDL cholesterol of 1.5 mmol/L.
#' calculate_nonHDL(LAB_CHOL = 5.0, LAB_HDL = 1.5)
#' # Output: 3.5
#'
#' # Example: Respondent has non-response values for cholesterol.
#' result <- calculate_nonHDL(LAB_CHOL = 99.98, LAB_HDL = 1.5)
#' result # Shows: NA
#' haven::is_tagged_na(result, "b") # Shows: TRUE (confirms it's tagged NA(b))
#' format(result, tag = TRUE) # Shows: "NA(b)" (displays the tag)
#'
#' # Multiple respondents
#' calculate_nonHDL(LAB_CHOL = c(5.0, 6.0, 7.0), LAB_HDL = c(1.5, 1.0, 2.0))
#' # Returns: c(3.5, 5.0, 5.0)
#'
#' # Database usage: Applied to survey datasets
#' library(dplyr)
#' # dataset %>%
#' #   mutate(non_hdl = calculate_nonHDL(LAB_CHOL, LAB_HDL))
#'
#' @seealso [categorize_nonHDL()]
#' @export
calculate_nonHDL <- function(LAB_CHOL, LAB_HDL) {
  dplyr::case_when(
    # Valid skip
    LAB_CHOL == 99.96 | LAB_HDL == 9.96 ~ haven::tagged_na("a"),
    # Don't know, refusal, not stated
    (LAB_CHOL >= 99.97 & LAB_CHOL <= 99.99) | (LAB_HDL >= 9.97 & LAB_HDL <= 9.99) ~ haven::tagged_na("b"),

    # Handle out of range values
    LAB_CHOL < 1.88 | LAB_CHOL > 13.58 | LAB_HDL < 0.49 | LAB_HDL > 3.74 ~ haven::tagged_na("b"),

    # Calculate non-HDL cholesterol
    TRUE ~ LAB_CHOL - LAB_HDL
  )
}

#' @title Categorical non-HDL cholesterol level
#'
#' @description This function categorizes individuals' non-HDL cholesterol levels based on a threshold value.
#'
#' @param nonHDL [numeric] A numeric representing an individual's non-HDL cholesterol level.
#'
#' @return [integer] A categorical indicating the non-HDL cholesterol category:
#'   - 1: High non-HDL cholesterol (nonHDL >= 4.3)
#'   - 2: Normal non-HDL cholesterol (nonHDL < 4.3)
#'   - `haven::tagged_na("a")`: Not applicable
#'   - `haven::tagged_na("b")`: Missing
#'
#' @details This function categorizes non-HDL cholesterol levels into 'High' or 'Normal' based on a 4.3 mmol/L threshold.
#'
#'          **Missing Data Codes:**
#'          - Propagates tagged NAs from the input `nonHDL`.
#'
#' @examples
#' # Scalar usage: Single respondent
#' # Example 1: Categorize a nonHDL value of 5.0 as high non-HDL cholesterol
#' categorize_nonHDL(5.0)
#' # Output: 1
#'
#' # Example 2: Categorize a nonHDL value of 3.8 as normal non-HDL cholesterol
#' categorize_nonHDL(3.8)
#' # Output: 2
#'
#' # Multiple respondents
#' categorize_nonHDL(c(5.0, 3.8, 4.3))
#' # Returns: c(1, 2, 1)
#'
#' # Database usage: Applied to survey datasets
#' library(dplyr)
#' # dataset %>%
#' #   mutate(non_hdl_category = categorize_nonHDL(non_hdl))
#'
#' @seealso [calculate_nonHDL()]
#' @export
categorize_nonHDL <- function(nonHDL) {
  dplyr::case_when(
    # Propagate tagged NAs
    haven::is_tagged_na(nonHDL, "a") ~ haven::tagged_na("a"),
    haven::is_tagged_na(nonHDL, "b") ~ haven::tagged_na("b"),

    # Categorize non-HDL cholesterol
    nonHDL >= 4.3 ~ 1,
    nonHDL < 4.3 ~ 2,

    # Handle any other cases
    .default = haven::tagged_na("b")
  )
}

#' @title Waist-to-height ratio (WHtR)
#'
#' @description This function calculates the Waist-to-Height Ratio (WHtR) by dividing the waist circumference by the height of the respondent.
#'
#' @param HWM_11CM [numeric] A numeric representing the height of the respondent in centimeters.
#' @param HWM_14CX [numeric] A numeric representing the waist circumference of the respondent in centimeters.
#'
#' @return [numeric] The WHtR. If inputs are invalid or out of bounds, the function returns a tagged NA.
#'
#' @details This function calculates the Waist-to-Height Ratio (WHtR), an indicator of central obesity.
#'
#'          **Missing Data Codes:**
#'          - `HWM_11CM`:
#'            - `999.96`: Valid skip. Handled as `haven::tagged_na("a")`.
#'            - `999.97-999.99`: Don't know, refusal, or not stated. Handled as `haven::tagged_na("b")`.
#'          - `HWM_14CX`:
#'            - `999.6`: Valid skip. Handled as `haven::tagged_na("a")`.
#'            - `999.7-999.9`: Don't know, refusal, or not stated. Handled as `haven::tagged_na("b")`.
#'
#' @examples
#' # Scalar usage: Single respondent
#' # Example 1: Calculate WHtR for a respondent with height = 170 cm and waist circumference = 85 cm.
#' calculate_WHR(HWM_11CM = 170, HWM_14CX = 85)
#' # Output: 0.5 (85/170)
#'
#' # Example 2: Calculate WHtR for a respondent with missing height.
#' result <- calculate_WHR(HWM_11CM = 999.98, HWM_14CX = 85)
#' result # Shows: NA
#' haven::is_tagged_na(result, "b") # Shows: TRUE (confirms it's tagged NA(b))
#' format(result, tag = TRUE) # Shows: "NA(b)" (displays the tag)
#'
#' # Multiple respondents
#' calculate_WHR(HWM_11CM = c(170, 180, 160), HWM_14CX = c(85, 90, 80))
#' # Returns: c(0.5, 0.5, 0.5)
#'
#' # Database usage: Applied to survey datasets
#' library(dplyr)
#' # dataset %>%
#' #   mutate(whtr = calculate_WHR(HWM_11CM, HWM_14CX))
#'
#' @export
calculate_WHR <- function(HWM_11CM, HWM_14CX) {
  dplyr::case_when(
    # Valid skip
    HWM_11CM == 999.96 | HWM_14CX == 999.6 ~ haven::tagged_na("a"),
    # Don't know, refusal, not stated
    (HWM_11CM >= 999.97 & HWM_11CM <= 999.99) | (HWM_14CX >= 999.7 & HWM_14CX <= 999.9) ~ haven::tagged_na("b"),

    # Handle out of range values
    HWM_11CM < 0 | HWM_14CX < 0 ~ haven::tagged_na("b"),

    # Calculate WHtR
    TRUE ~ HWM_14CX / HWM_11CM
  )
}
