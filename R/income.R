#' @title Calculate adjusted total household income based on weighted household size.
#'
#' @description This function calculates the adjusted total household income based on the respondent's income amount
#'              and actual household size, taking into account the weighted household size.
#'
#' @param THI_01 A numeric representing the respondent's household income amount in dollars.
#' @param DHHDHSZ An integer representing the respondent's actual household size in persons.
#'
#' @return The calculated adjusted total household income as a numeric value. If any of the input parameters (THI_01,
#'         DHHDHSZ) are non-response values (THI_01 >= 996, DHHDHSZ >= 996), the adjusted household income will be
#'         NA(b) (Not Available).
#'
#' @details The function first calculates the weighted household size (hh_size_wt) based on the respondent's actual
#'          household size (DHHDHSZ). It uses a loop to iterate from 1 to DHHDHSZ and assigns weights to each household
#'          member based on their count. If the household size (i) is 1, the weight is 1; if i is 2, the weight is 0.4; if
#'          i is greater than or equal to 3, the weight is 0.3. The weighted household size is then used to adjust the
#'          respondent's total household income (THI_01) by dividing it by hh_size_wt. The adjusted household income
#'          (adj_hh_inc) is returned as the final output.
#'
#' @examples
#'
#' # Example 1: Calculate adjusted household income for a respondent with $50,000 income and a household size of 3.
#' calculate_Hhld_Income(THI_01 = 50000, DHHDHSZ = 3)
#' # Output: 29411.76
#'
#' # Example 2: Calculate adjusted household income for a respondent with $75000 income and a household size of 2.
#' calculate_Hhld_Income(THI_01 = 75000, DHHDHSZ = 2)
#' # Output: 53571.43
#'
#' # Example 3: Calculate adjusted household income for a respondent with $90000 income and a household size of 1.
#' calculate_Hhld_Income(THI_01 = 90000, DHHDHSZ = 1)
#' # Output: 90000
#' 
#' @export
calculate_Hhld_Income <- function(THI_01, DHHDHSZ) {
  
  # Step 1 - derive household adjustment based on household size 
  hh_size_wt <- 0
  
  if (is.na(DHHDHSZ)) {
    return(haven::tagged_na("b"))
  }
  
  for (i in 1:DHHDHSZ) {
    if (i == 1) {
      hh_size_wt <- hh_size_wt + 1
    } else if (i == 2) {
      hh_size_wt <- hh_size_wt + 0.4
    } else if (i >= 3) {
      hh_size_wt <- hh_size_wt + 0.3
    }
  }
  
  # Step 2 - Adjust total household income based on household size 
  adj_hh_inc <- THI_01 / hh_size_wt
  if (is.na(adj_hh_inc)) {
    adj_hh_inc <- haven::tagged_na("b")
  }
  return(adj_hh_inc)
  
}

#' Categorize Household Income
#'
#' This function categorizes individuals' household income based on specified income ranges.
#'
#' @param adj_hh_inc Numeric value representing the adjusted household income.
#'
#' @return A categorical value indicating the income category:
#'   - 1: Below or equal to $21,500
#'   - 2: Above $21,500 and up to $35,000
#'   - 3: Above $35,000 and up to $50,000
#'   - 4: Above $50,000 and up to $70,000
#'   - 5: Above $70,000
#'   - NA(b): Missing or invalid input
#'
#' @examples
#' # Example 1: Categorize a household income of $25,000
#' categorize_income(25000)
#' # Output: 2
#'
#' # Example 2: Categorize a household income of $45,000
#' categorize_income(45000)
#' # Output: 3
#'
#' @export
categorize_income <- function(adj_hh_inc) {
  
  incq <- haven::tagged_na("b")
  
  if (is.na(adj_hh_inc)) {
    return(incq)
  }
  else {
    if (adj_hh_inc <= 21500) {
      incq <- 1
    }
    else if (adj_hh_inc > 21500 && adj_hh_inc <= 35000) {
      incq <- 2
    }
    else if (adj_hh_inc > 35500 && adj_hh_inc <= 50000) {
      incq <- 3
    }
    else if (adj_hh_inc > 50000 && adj_hh_inc <= 70000) {
      incq <- 4
    }
    else if (adj_hh_inc > 70000) {
      incq <- 5
    }
  }
  return(incq)
  
}

#' Check If in Lowest Income Quartile
#'
#' This function checks if an individual's income category corresponds to the lowest income quartile.
#'
#' @param incq Categorical value indicating the income category as defined by the categorize_income function.
#'
#' @return A categorical value indicating whether the individual is in the lowest income quartile:
#'   - 1: In the lowest income quartile
#'   - 2: Not in the lowest income quartile
#'   - NA(b): Missing or invalid input
#'
#' @examples
#' # Example 1: Check if an income category of 3 (between $35,000 and $50,000) is in the lowest quartile
#' in_lowest_income_qunitle(3)
#' # Output: 2
#'
#' # Example 2: Check if an income category of 1 (below or equal to $21,500) is in the lowest quartile
#' in_lowest_income_qunitle(1)
#' # Output: 1
#'
#' @export
in_lowest_income_qunitle <- function(incq) {
  
  incq1 <- haven::tagged_na("b")
  
  if (is.na(incq)) {
    return(incq)
  }
  else {
    if (incq == 1) {
      incq1 <- 1
    }
    else {
      incq1 <- 2
    }
  }
  return(incq1)
  
}