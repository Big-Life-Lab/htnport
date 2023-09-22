#' @title Adjuste systolic blood pressure
#' 
#' This function adjusts systolic blood pressure based on the respondent's systolic average blood pressure across 
#' six measurements. The adjustment is made using specific correction factors. The adjusted systolic blood pressure
#' is returned as a numeric value.
#'
#' @param BPMDPBPS A numeric representing the respondent's systolic average blood pressure (in mmHg) across six measurements.
#'
#' @return The adjusted systolic blood pressure as a numeric value.
#' 
#' @details The function calculates the adjusted systolic blood pressure (SBP_adj) based on the value of BPMDPBPS. If
#'          BPMDPBPS is greater than or equal to 0 and less than 996, the adjustment is made using the formula:
#'          SBP_adj = 11.4 + (0.93 * BPMDPBPS). Otherwise, if BPMDPBPS is a non-response value (BPMDPBPS >= 996), the
#'          adjusted systolic blood pressure is set to NA(b), indicating that the measurement is not available. The adjusted
#'          systolic blood pressure is returned as the final output.
#'
#' @examples
#' 
#' # Example: Calculate adjusted systolic blood pressure for a respondent with an average systolic blood pressure of 120 mmHg.
#' adjust_SBP(BPMDPBPS = 120)
#' # Output: 123
#' 
#' @export
adjust_SBP <- function(BPMDPBPS) {
  
  SBP_adj <- 0
  
  if (BPMDPBPS >= 0 && BPMDPBPS < 996 && !is.na(BPMDPBPS)) { # Proceeds without non-responses
    SBP_adj <- 11.4 + (0.93 * BPMDPBPS)
  }
  else {
    SBP_adj <- haven::tagged_na("b") # SBP_adj is set to NA(b) for non-response values
  }
  
  return(SBP_adj)
}

#' @title Adjust diastolic blood pressure
#' 
#' This function adjusts diastolic blood pressure based on the respondent's diastolic average blood pressure across 
#' six measurements. The adjustment is made using specific correction factors. The adjusted diastolic blood pressure
#' is returned as a numeric value.
#'
#' @param BPMDPBPD A numeric representing the respondent's diastolic average blood pressure (in mmHg) across six measurements.
#'
#' @return The adjusted diastolic blood pressure as a numeric value.
#' 
#' @details The function calculates the adjusted diastolic blood pressure (DBP_adj) based on the value of BPMDPBPD. If
#'          BPMDPBPD is greater than or equal to 0 and less than 996, the adjustment is made using the formula:
#'          DBP_adj = 15.6 + (0.83 * BPMDPBPD). Otherwise, if BPMDPBPD is a non-response value (BPMDPBPD >= 996), the
#'          adjusted diastolic blood pressure is set to NA(b), indicating that the measurement is not available. The adjusted
#'          diastolic blood pressure is returned as the final output.
#'
#' @examples
#' 
#' # Example: Calculate adjusted diastolic blood pressure for a respondent with an average diastolic blood pressure of 80 mmHg.
#' adjust_DBP(BPMDPBPD = 80)
#' # Output: 82
#' 
#' @export
adjust_DBP <- function(BPMDPBPD) {
  
  DBP_adj <- 0
  
  if (BPMDPBPD >= 0 && BPMDPBPD < 996 && !is.na(BPMDPBPD)) { # Proceeds without non-responses
    DBP_adj <- 15.6 + (0.83 * BPMDPBPD)
  }
  else {
    DBP_adj <- haven::tagged_na("b") # DBP_adj is set to NA(b) for non-response values
  }
  
  return(DBP_adj)
}

#' @title Determine Hypertension Status
#'
#' @description
#' This function determines the hypertension status of a respondent based on their systolic and diastolic blood pressure measurements and medication usage.
#'
#' @param BPMDPBPS An integer representing the systolic blood pressure measurement of the respondent.
#' @param BPMDPBPD An integer representing the diastolic blood pressure measurement of the respondent.
#' @param CCC_32 An integer indicating whether the respondent is on medication for hypertension.
#'   - 1: Yes
#'   - 2: No
#'
#' @return An integer representing the hypertension status:
#'   - 1: High blood pressure (BP ≥ 140/90 mmHg or on hypertension medication)
#'   - 2: Normal blood pressure (BP < 140/90 mmHg and not on hypertension medication)
#'   - NA(b): Invalid input or non-response
#'
#' @examples
#' 
#' # Example 1: Determine hypertension status for a respondent with systolic BP = 150, diastolic BP = 95, and on medication.
#' determine_hypertension(BPMDPBPS = 150, BPMDPBPD = 95, CCC_32 = 1)
#' # Output: 1 (High blood pressure due to systolic BP, diastolic BP, and medication usage).
#'
#' # Example 2: Determine hypertension status for a respondent with systolic BP = 120, diastolic BP = 80, and not on medication.
#' determine_hypertension(BPMDPBPS = 120, BPMDPBPD = 80, CCC_32 = 2)
#' # Output: 2 (Normal blood pressure as BP is below 140/90 mmHg and not on medication).
#' 
#' @export
determine_hypertension <- function(BPMDPBPS, BPMDPBPD, CCC_32) {
  highsys140 <- NA
  highdias90 <- NA
  highBP14090 <- haven::tagged_na("b")
  
  if (is.na(BPMDPBPS) || is.na(BPMDPBPD) || is.na(CCC_32)) {
    return(highBP14090)
  }
  
  # Check conditions and assign values to highsys140 and highdias90
  if (140 <= BPMDPBPS && BPMDPBPS < 996) {
    highsys140 <- 1
  } else if (0 <= BPMDPBPS && BPMDPBPS < 140) {
    highsys140 <- 2
  }
  else {
    return(highBP14090)
  }
  
  if (90 <= BPMDPBPD && BPMDPBPD < 996) {
    highdias90 <- 1
  } else if (0 <= BPMDPBPD && BPMDPBPD < 90) {
    highdias90 <- 2
  }
  else {
    return(highBP14090)
  }
  
  # Calculate highBP14090
  if (highsys140 == 1 || highdias90 == 1 || CCC_32 == 1) {
    highBP14090 <- 1
  } else if (highsys140 == 2 && highdias90 == 2 && CCC_32 == 2) {
    highBP14090 <- 2
  }
  
  return(highBP14090)
}

#' @title Determine Adjusted Hypertension Status
#'
#' @description
#' This function determines the adjusted hypertension status of a respondent based on their adjusted systolic and diastolic blood pressure measurements and medication usage.
#'
#' @param SBP_adj An integer representing the adjusted systolic blood pressure measurement of the respondent.
#' @param DBP_adj An integer representing the adjusted diastolic blood pressure measurement of the respondent.
#' @param CCC_32 An integer indicating whether the respondent is on medication for hypertension.
#'   - 1: Yes
#'   - 2: No
#'
#' @return An integer representing the adjusted hypertension status:
#'   - 1: High blood pressure (adjusted BP ≥ 140/90 mmHg or on hypertension medication)
#'   - 2: Normal blood pressure (adjusted BP < 140/90 mmHg and not on hypertension medication)
#'   - NA(b): Invalid input or non-response
#'
#' @examples
#' 
#' # Example 1: Determine adjusted hypertension status for a respondent with adjusted systolic BP = 150, adjusted diastolic BP = 95, and on medication.
#' determine_adjusted_hypertension(SBP_adj = 150, DBP_adj = 95, CCC_32 = 1)
#' # Output: 1 (High blood pressure due to adjusted systolic BP, adjusted diastolic BP, and medication usage).
#'
#' # Example 2: Determine adjusted hypertension status for a respondent with adjusted systolic BP = 120, adjusted diastolic BP = 80, and not on medication.
#' determine_adjusted_hypertension(SBP_adj = 120, DBP_adj = 80, CCC_32 = 2)
#' # Output: 2 (Normal blood pressure as adjusted BP is below 140/90 mmHg and not on medication).
#' 
#' @export
determine_adjusted_hypertension <- function(SBP_adj, DBP_adj, CCC_32) {
  highsys140_adj <- NA
  highdias90_adj <- NA
  highBP14090_adj <- haven::tagged_na("b")
  
  if (is.na(SBP_adj) || is.na(DBP_adj) || is.na(CCC_32)) {
    return(highBP14090)
  }
  
  # Check conditions and assign values to highsys140_adj and highdias90_adj
  if (140 <= SBP_adj && SBP_adj < 996) {
    highsys140_adj <- 1
  } else if (0 <= SBP_adj && SBP_adj < 140) {
    highsys140_adj <- 2
  }
  else {
    return(highBP14090_adj)
  }
  
  if (90 <= DBP_adj && DBP_adj < 996) {
    highdias90_adj <- 1
  } else if (0 <= DBP_adj && DBP_adj < 90) {
    highdias90_adj <- 2
  }
  else {
    return(highBP14090_adj)
  }
  
  # Initialize and calculate highBP14090_adj
  
  if (highsys140_adj == 1 || highdias90_adj == 1 || CCC_32 == 1) {
    highBP14090_adj <- 1
  } else if (highsys140_adj == 2 && highdias90_adj == 2 && CCC_32 == 2) {
    highBP14090_adj <- 2
  }
  
  return(highBP14090_adj)
}

#' @title Determine Controlled Hypertension Status
#'
#' @description
#' This function determines the controlled hypertension status of a respondent based on their systolic and diastolic blood pressure measurements and medication usage.
#'
#' @param BPMDPBPS An integer representing the systolic blood pressure measurement of the respondent.
#' @param BPMDPBPD An integer representing the diastolic blood pressure measurement of the respondent.
#' @param CCC_32 An integer indicating whether the respondent is on medication for hypertension.
#'   - 1: Yes
#'   - 2: No
#'
#' @return An integer representing the controlled hypertension status:
#'   - 1: Hypertension not controlled (BP ≥ 140/90 mmHg or on hypertension medication).
#'   - 2: Hypertension controlled (BP < 140/90 mmHg and on hypertension medication).
#'   - NA(b): Invalid input or non-response.
#'
#' @examples
#' 
#' # Example 1: Determine controlled hypertension status for a respondent with systolic BP = 150, diastolic BP = 95, and on medication.
#' determine_controlled_hypertension(BPMDPBPS = 150, BPMDPBPD = 95, CCC_32 = 1)
#' # Output: 1 (Hypertension not controlled due to systolic BP, diastolic BP, and medication usage).
#'
#' # Example 2: Determine controlled hypertension status for a respondent with systolic BP = 120, diastolic BP = 80, and on medication.
#' determine_controlled_hypertension(BPMDPBPS = 120, BPMDPBPD = 80, CCC_32 = 1)
#' # Output: 2 (Hypertension controlled as BP is below 140/90 mmHg and on medication).
#' 
#' @export
determine_controlled_hypertension <- function(BPMDPBPS, BPMDPBPD, CCC_32) {
  highsys140 <- NA
  highdias90 <- NA
  Control14090 <- haven::tagged_na("b")
  
  if (is.na(BPMDPBPS) || is.na(BPMDPBPD) || is.na(CCC_32)) {
    return(highBP14090)
  }
  
  # Check conditions and assign values to highsys140 and highdias90
  if (140 <= BPMDPBPS && BPMDPBPS < 996) {
    highsys140 <- 1
  } else if (0 <= BPMDPBPS && BPMDPBPS < 140) {
    highsys140 <- 2
  }
  else {
    return(Control14090)
  }
  
  if (90 <= BPMDPBPD && BPMDPBPD < 996) {
    highdias90 <- 1
  } else if (0 <= BPMDPBPD && BPMDPBPD < 90) {
    highdias90 <- 2
  }
  else {
    return(Control14090)
  }
  
  # Check the conditions using nested ifelse statements
  if (CCC_32 == 1) {
    Control14090 <- ifelse(highsys140 == 1 || highdias90 == 1, 2,
                           ifelse(highsys140 == 2 && highdias90 == 2, 1, NA))
  }
  else {
    Control14090 <- 2
  }
  
  return(Control14090)
}

#' @title Determine Controlled Adjusted Hypertension Status
#'
#' @description
#' This function determines the controlled adjusted hypertension status of a respondent based on their adjusted systolic and diastolic blood pressure measurements and medication usage.
#'
#' @param SBP_adj An integer representing the adjusted systolic blood pressure measurement of the respondent.
#' @param DBP_adj An integer representing the adjusted diastolic blood pressure measurement of the respondent.
#' @param CCC_32 An integer indicating whether the respondent is on medication for hypertension.
#'   - 1: Yes
#'   - 2: No
#'
#' @return An integer representing the controlled adjusted hypertension status:
#'   - 1: Hypertension not controlled (adjusted BP ≥ 140/90 mmHg or on hypertension medication).
#'   - 2: Hypertension controlled (adjusted BP < 140/90 mmHg and on hypertension medication).
#'   - NA(b): Invalid input or non-response.
#'
#' @examples
#' 
#' # Example 1: Determine controlled adjusted hypertension status for a respondent with adjusted systolic BP = 150, adjusted diastolic BP = 95, and on medication.
#' determine_controlled_adjusted_hypertension(SBP_adj = 150, DBP_adj = 95, CCC_32 = 1)
#' # Output: 1 (Hypertension not controlled due to adjusted systolic BP, adjusted diastolic BP, and medication usage).
#'
#' # Example 2: Determine controlled adjusted hypertension status for a respondent with adjusted systolic BP = 120, adjusted diastolic BP = 80, and on medication.
#' determine_controlled_adjusted_hypertension(SBP_adj = 120, DBP_adj = 80, CCC_32 = 1)
#' # Output: 2 (Hypertension controlled as adjusted BP is below 140/90 mmHg and on medication).
#' 
#' @export
determine_controlled_adjusted_hypertension <- function(SBP_adj, DBP_adj, CCC_32) {
  highsys140_adj <- NA
  highdias90_adj <- NA
  Control14090_adj <- haven::tagged_na("b")
  
  if (is.na(SBP_adj) || is.na(DBP_adj) || is.na(CCC_32)) {
    return(Control14090_adj)
  }
  
  # Check conditions and assign values to highsys140_adj and highdias90_adj
  if (140 <= SBP_adj && SBP_adj < 996) {
    highsys140_adj <- 1
  } else if (0 <= SBP_adj && SBP_adj < 140) {
    highsys140_adj <- 2
  }
  else {
    return(Control14090_adj)
  }
  
  if (90 <= DBP_adj && DBP_adj < 996) {
    highdias90_adj <- 1
  } else if (0 <= DBP_adj && DBP_adj < 90) {
    highdias90_adj <- 2
  }
  else {
    return(Control14090_adj)
  }
  
  # Check the conditions using nested ifelse statements
  if (CCC_32 == 1) {
    Control14090_adj <- ifelse(highsys140_adj == 1 || highdias90_adj == 1, 2,
                               ifelse(highsys140_adj == 2 && highdias90_adj == 2, 1, NA))
  }
  else {
    Control14090 <- 2
  }
  
  return(Control14090_adj)
}