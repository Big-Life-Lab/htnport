#' @title Adjusted total household income
#'
#' @description This function calculates the adjusted total household income based on the respondent's income amount
#'              and actual household size, taking into account the weighted household size.
#'
#' @param THI_01 [numeric] A numeric representing the respondent's household income amount in dollars.
#' @param DHHDHSZ [integer] An integer representing the respondent's actual household size in persons.
#'
#' @return [numeric] The calculated adjusted total household income as a numeric. If inputs are invalid or out of bounds, the function returns a tagged NA.
#'
#' @details This function applies equivalence scales to adjust household income for household size,
#'          allowing for meaningful income comparisons across different household compositions.
#'
#'          **Equivalence Scale Logic:**
#'          - First adult: Weight = 1.0 (full weight)
#'          - Second adult: Weight = 0.4 (economies of scale)
#'          - Additional members: Weight = 0.3 each (further economies)
#'
#'          **Missing Data Codes:**
#'          - `THI_01`:
#'            - `99999996`: Valid skip. Handled as `haven::tagged_na("a")`.
#'            - `99999997-99999999`: Don't know, refusal, or not stated. Handled as `haven::tagged_na("b")`.
#'          - `DHHDHSZ`:
#'            - `96`: Valid skip. Handled as `haven::tagged_na("a")`.
#'            - `97-99`: Don't know, refusal, or not stated. Handled as `haven::tagged_na("b")`.
#'
#' @examples
#' # Scalar usage: Single respondent
#' # Example 1: Respondent with $50,000 income and a household size of 3.
#' calculate_hhld_income(THI_01 = 50000, DHHDHSZ = 3)
#' # Output: 29411.76
#'
#' # Example 2: Respondent has non-response values for all inputs.
#' result <- calculate_hhld_income(THI_01 = 99999998, DHHDHSZ = 98)
#' result # Shows: NA
#' haven::is_tagged_na(result, "b") # Shows: TRUE (confirms it's tagged NA(b))
#' format(result, tag = TRUE) # Shows: "NA(b)" (displays the tag)
#'
#' # Multiple respondents
#' calculate_hhld_income(THI_01 = c(50000, 75000, 90000), DHHDHSZ = c(3, 2, 1))
#' # Returns: c(29411.76, 53571.43, 90000)
#'
#' # Database usage: Applied to survey datasets
#' library(dplyr)
#' # dataset %>%
#' #   mutate(adj_hh_income = calculate_hhld_income(THI_01, DHHDHSZ))
#'
#' @seealso [categorize_income()] for income classification, [in_lowest_income_quintile()] for poverty indicators
#' @export
calculate_hhld_income <- function(THI_01, DHHDHSZ) {
  # Calculate the household size weight
  hh_size_wt <- dplyr::case_when(
    # Valid skip
    DHHDHSZ == 96 ~ haven::tagged_na("a"),
    # Don't know, refusal, not stated
    DHHDHSZ <= 0 | DHHDHSZ %in% 97:99 ~ haven::tagged_na("b"),
    DHHDHSZ == 1 ~ 1,
    DHHDHSZ == 2 ~ 1 + 0.4,
    TRUE ~ 1 + 0.4 + (DHHDHSZ - 2) * 0.3
  )

  # Adjust the household income
  adj_hh_inc <- THI_01 / hh_size_wt

  # Handle missing data codes and out of range values
  dplyr::case_when(
    # Valid skip
    (THI_01 == 99999996) | (DHHDHSZ == 96) ~ haven::tagged_na("a"),
    # Don't know, refusal, not stated
    (THI_01 >= 99999997 & THI_01 <= 99999999) | (DHHDHSZ >= 97 & DHHDHSZ <= 99) ~ haven::tagged_na("b"),
    adj_hh_inc < 0 ~ haven::tagged_na("b"),
    TRUE ~ adj_hh_inc
  )
}

#' @title Categorical adjusted household income
#'
#' @description This function categorizes individuals' adjusted household income based on specified income ranges.
#'
#' @param adj_hh_inc [numeric] A numeric representing the adjusted household income.
#'
#' @return [integer] The income category:
#'   - 1: Below or equal to $21,500
#'   - 2: Above $21,500 and up to $35,000
#'   - 3: Above $35,000 and up to $50,000
#'   - 4: Above $50,000 and up to $70,000
#'   - 5: Above $70,000
#'   - `haven::tagged_na("a")`: Not applicable
#'   - `haven::tagged_na("b")`: Missing
#'
#' @details This function segments adjusted household income into quintiles, providing a standardized measure of socioeconomic status.
#'
#'          **Missing Data Codes:**
#'          - Propagates tagged NAs from the input `adj_hh_inc`.
#'
#' @examples
#' # Scalar usage: Single respondent
#' # Example 1: Categorize a household income of $25,000
#' categorize_income(25000)
#' # Output: 2
#'
#' # Example 2: Categorize a household income of $45,000
#' categorize_income(45000)
#' # Output: 3
#'
#' # Multiple respondents
#' categorize_income(c(25000, 45000, 80000))
#' # Returns: c(2, 3, 5)
#'
#' # Database usage: Applied to survey datasets
#' library(dplyr)
#' # dataset %>%
#' #   mutate(income_category = categorize_income(adj_hh_inc))
#'
#' @seealso [calculate_hhld_income()], [in_lowest_income_quintile()]
#' @export
categorize_income <- function(adj_hh_inc) {
  dplyr::case_when(
    # Propagate tagged NAs
    haven::is_tagged_na(adj_hh_inc, "a") ~ haven::tagged_na("a"),
    haven::is_tagged_na(adj_hh_inc, "b") | adj_hh_inc < 0 ~ haven::tagged_na("b"),

    # Categorize income
    adj_hh_inc <= 21500 ~ 1,
    adj_hh_inc > 21500 & adj_hh_inc <= 35000 ~ 2,
    adj_hh_inc > 35000 & adj_hh_inc <= 50000 ~ 3,
    adj_hh_inc > 50000 & adj_hh_inc <= 70000 ~ 4,
    adj_hh_inc > 70000 ~ 5,

    # Handle any other cases
    .default = haven::tagged_na("b")
  )
}

#' @title Lowest income quintile indicator
#'
#' @description This function checks if an individual's income category corresponds to the lowest income quintile.
#'
#' @param incq [integer] A categorical vector indicating the income category as defined by the categorize_income function.
#'
#' @return [integer] Whether the individual is in the lowest income quintile:
#'   - 1: In the lowest income quntile
#'   - 2: Not in the lowest income quntile
#'   - `haven::tagged_na("a")`: Not applicable
#'   - `haven::tagged_na("b")`: Missing
#'
#' @details This function identifies individuals in the lowest income quintile, a common indicator for socioeconomic disadvantage.
#'
#'          **Missing Data Codes:**
#'          - Propagates tagged NAs from the input `incq`.
#'
#' @examples
#' # Scalar usage: Single respondent
#' # Example 1: Check if an income category of 3 is in the lowest quintile
#' in_lowest_income_quintile(3)
#' # Output: 2
#'
#' # Example 2: Check if an income category of 1 is in the lowest quintile
#' in_lowest_income_quintile(1)
#' # Output: 1
#'
#' # Multiple respondents
#' in_lowest_income_quintile(c(3, 1, 5))
#' # Returns: c(2, 1, 2)
#'
#' # Database usage: Applied to survey datasets
#' library(dplyr)
#' # dataset %>%
#' #   mutate(in_lowest_quintile = in_lowest_income_quintile(income_category))
#'
#' @seealso [categorize_income()]
#' @export
in_lowest_income_quintile <- function(incq) {
  dplyr::case_when(
    # Propagate tagged NAs
    haven::is_tagged_na(incq, "a") ~ haven::tagged_na("a"),
    haven::is_tagged_na(incq, "b") | incq < 1 | incq > 5 ~ haven::tagged_na("b"),
    is.na(incq) ~ haven::tagged_na("b"),

    # Check if in lowest income quintile
    incq == 1 ~ 1,
    TRUE ~ 2
  )
}
