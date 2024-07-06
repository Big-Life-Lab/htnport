#' @title Determine Survey Cycle
#'
#' @description
#' This function determines the survey cycle based on the provided data_name.
#'
#' @param data_name A character string representing the name of the survey cycle.
#'   Possible values are "cycle1", "cycle2", "cycle3", "cycle4", "cycle5", and "cycle6".
#'
#' @return A character string representing the survey cycle if data_name matches one of the predefined cycles,
#'   or an error message if it does not.
#'
#' @examples
#'
#' # Example 1: Determine the survey cycle for "cycle1".
#' SurveyCycle.fun("cycle1")
#' # Output: "cycle1"
#'
#' # Example 2: Determine the survey cycle for an unknown cycle.
#' SurveyCycle.fun("unknown_cycle")
#' # Output: Error: Unknown data_name argument when creating cycle variable unknown_cycle
#'
#' @export
SurveyCycle.fun <- function(data_name) {
  # Ensure data_name is a character string
  if (!is.character(data_name)) {
    stop("data_name must be a character string")
  }
  
  cycle <- switch(
    data_name,
    cycle1 = "cycle1",
    cycle2 = "cycle2",
    cycle3 = "cycle3",
    cycle4 = "cycle4",
    cycle5 = "cycle5",
    cycle6 = "cycle6",
    {
      stop(paste("Unknown data_name argument when creating cycle variable", data_name))
    }
  )
  
  return(cycle)
}