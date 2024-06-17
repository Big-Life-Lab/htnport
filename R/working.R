#' Binary Working Status Determination  
#'
#' This function determines individuals' binary working status based on their working hours and their working status last week.
#'
#' @param LMH_016 Numeric value representing usual working hours per week.
#' @param LAFDWSL Numeric value representing working status in the past week.
#'
#' @return A categorical value indicating the binary working status:
#'   - 1: Has a job
#'   - 2: Does not have a job
#'   - NA(b): Missing or invalid input
#'
#' @examples
#' # Example 1: Determine binary working status for someone who works 20 hours/week and worked last week.
#' determine_working_status(20, 1)
#' # Output: 1
#' 
#' # Example 2: Determine binary working status from 40 hours worked per week and got a temporary layoff.
#' determine_working_status(40, 2)
#' # Output: 2
#'
#' @export
determine_working_status <- function(LMH_016, LAFDWSL) {
  
  working <- haven::tagged_na("b")
  
  if (is.na(LMH_016)) {
    if (haven::is_tagged_na(LMH_016, tag = "a")) {
      working <- 2
    }
    else {
      return(working)
    }
  }
  else if (LMH_016 %in% 0:128) {
    if (LAFDWSL == 1 || LAFDWSL == 3) {
      working <- 1
    }
    else {
      working <- 2
    }
  }
  else {
    return(working)
  }
  
  return(working)
  
}