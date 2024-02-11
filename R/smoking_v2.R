# Suggested code for pack_years function in smoking_v2.R
# This code is not intended to be run as-is, but rather to be used as a reference
# suggestions
# 1) More explicity defeine the constants for minimum pack years values
# 2) More explicity define the data types for the arguments, and check those data types using checkmake
# 3) Use the logger library to log debug messages. The focus of logger is to display clear messages to the user when there is fatal errors (default), with additional logging info when the user requests it.
# 4) Use switch rather than if-else statements


library(logger)
library(haven)
library(checkmate)

pack_years_fun2 <- function(SMKDSTY = NA, CLC_AGE = NA, SMKDSTP = NA, SMK_52 = NA, SMK_31 = NA,
                            SMK_41 = NA, SMK_53 = NA, SMK_23 = NA, SMK_21 = NA, SMK_11 = NA) {
  # Define constants for minimum pack years
  MIN_PACK_YEARS_DAILY <- 0.0137
  MIN_PACK_YEARS_OCCASIONAL <- 0.007

  # Initialize pack_years with NA
  pack_years <- rep(haven::tagged_na("b"), length(SMKDSTY))

  # Special handling for tagged NAs
  if (haven::is_tagged_na(SMKDSTY)) {
    return(SMKDSTY) # Return the tagged NA without alteration
  }

  # Input validation for SMKDSTY and CLC_AGE
  if (!is.numeric(SMKDSTY) || SMKDSTY < 1 || SMKDSTY > 6) {
    log_warn("SMKDSTY is invalid. Must be numeric 1-6. Returning tagged NA.")
    return(haven::tagged_na("b"))
  }
  if (!is.numeric(CLC_AGE) || is.na(CLC_AGE) || CLC_AGE < 0) {
    log_warn("CLC_AGE is invalid. Must be a non-negative number. Returning tagged NA.")
    return(haven::tagged_na("b"))
  }

  # Calculate pack years based on smoking status
  pack_years <- switch(as.character(SMKDSTY),
    "1" = { 
      # Daily smoker
      if (!test_numeric(SMK_52, lower = 0) || !test_numeric(SMK_31, lower = 0)) {
        log_warn("SMK_52 or SMK_31 is invalid for SMKDSTY 1. Returning tagged NA.")
        return(haven::tagged_na("b")) 
      }
      pmax(((CLC_AGE - SMK_52) * (SMK_31 / 20)), MIN_PACK_YEARS_DAILY)
    },
    "2" = {
      # Occasional Smoker (former daily)
      if (!test_numeric(SMK_52, lower = 0) || !test_numeric(SMKDSTP, lower = 0) || !test_numeric(SMK_53, lower = 0) || !test_numeric(SMK_41, lower = 0) || !test_numeric(SMK_23, lower = 0)) {
        log_warn("Variables are invalid for SMKDSTY 2. Returning tagged NA.")
        return(haven::tagged_na("b"))
      }
      pmax(((CLC_AGE - SMK_52 - SMKDSTP) * (SMK_53 / 20)), MIN_PACK_YEARS_DAILY) +
        (pmax((SMK_41 * (SMK_23 * 12) / 30), 1) * SMKDSTP)
    },
    "3" = {
      # Former daily smoker (non-smoker now)
      if (!test_numeric(SMK_41, lower = 0) || !test_numeric(SMK_23, lower = 0) || !test_numeric(SMK_21, lower = 0)) {
        log_warn("Variables are invalid for SMKDSTY 3. Returning tagged NA.")
        return(haven::tagged_na("b"))
      }
      pmax((SMK_41 * (SMK_23 * 12) / 30), 1) / 20 * (CLC_AGE - SMK_21)
    },
    "4" = {
      # Former occasional smoker (non-smoker now) who
      # smoked at least 100 cigarettes lifetime
      if (!test_numeric(SMK_52, lower = 0) || !test_numeric(SMKDSTP, lower = 0) || !test_numeric(SMK_53, lower = 0)) {
        log_warn("Variables are invalid for SMKDSTY 4. Returning tagged NA.")
        return(haven::tagged_na("b"))
      }
      pmax(((CLC_AGE - SMK_52 - SMKDSTP) * (SMK_53 / 20)), MIN_PACK_YEARS_DAILY)
    },
    "5" = {
      # Former occasional smoker (non-smoker now) who 
      # have not smoked at least 100 cigarettes lifetime
      if (!test_numeric(SMK_11, lower = 1, upper = 2)) {
        log_warn("SMK_11 is invalid for SMKDSTY 5. It must be 1 or 2. Returning tagged NA.")
        return(haven::tagged_na("b"))
      }
      ifelse(SMK_11 == 1, MIN_PACK_YEARS_DAILY, ifelse(SMK_11 == 2, MIN_PACK_YEARS_OCCASIONAL, haven::tagged_na("b")))
    },
    # Non-smoker
    "6" = {
      # No variables to validate for non-smokers
      0
    },
    # Default case for any unhandled values of SMKDSTY, returning a tagged NA "b"
    {
      log_warn("Unhandled SMKDSTY value. Returning tagged NA.")
      haven::tagged_na("b")
    }
  )

  return(pack_years)
}

# Example usage:
pack_years_fun(SMKDSTY = 1, CLC_AGE = 40, SMK_52 = 20, SMK_31 = 30)
pack_years_fun(SMKDSTY = 5, CLC_AGE = 50, SMKDSTP = 10, SMK_52 = 18, SMK_41 = 15, SMK_23 = 3, SMK_21 = 25, SMK_11 = 1)
# pack_years_fun(SMKDSTY = haven::tagged_na("a"))
# pack_years_fun(SMKDSTY = 6)
# pack_years_fun(SMKDSTY = 10)

pack_years_fun2(SMKDSTY = 1, CLC_AGE = 40, SMK_52 = 20, SMK_31 = 30)
pack_years_fun2(SMKDSTY = 5, CLC_AGE = 50, SMKDSTP = 10, SMK_52 = 18, SMK_41 = 15, SMK_23 = 3, SMK_21 = 25, SMK_11 = 1)
pack_years_fun2(SMKDSTY = haven::tagged_na("a"))
pack_years_fun2(SMKDSTY = 6)
pack_years_fun2(SMKDSTY = 10)
