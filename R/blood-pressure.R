#' @title Adjust systolic blood pressure
#'
#' @description This function adjusts systolic blood pressure based on the respondent's systolic average blood pressure across
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
#' # Example: Adjust for a respondent with average systolic blood pressure of 120 mmHg.
#' adjust_SBP(BPMDPBPS = 120)
#' # Output: 123
#'
#' @export
adjust_SBP <- function(BPMDPBPS) {
  SBP_adj <- 0
  
  if (BPMDPBPS >= 0 && BPMDPBPS < 996 && !is.na(BPMDPBPS)) { # Proceeds without non-responses
    SBP_adj <- 11.4 + (0.93 * BPMDPBPS)
  } else {
    SBP_adj <- haven::tagged_na("b") # SBP_adj is set to NA(b) for non-response values
  }
  
  return(SBP_adj)
}

#' @title Adjust diastolic blood pressure
#'
#' @description This function adjusts diastolic blood pressure based on the respondent's diastolic average blood pressure across
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
#' # Example: Adjust for a respondent with average diastolic blood pressure of 80 mmHg.
#' adjust_DBP(BPMDPBPD = 80)
#' # Output: 82
#'
#' @export
adjust_DBP <- function(BPMDPBPD) {
  DBP_adj <- 0
  
  if (BPMDPBPD >= 0 && BPMDPBPD < 996 && !is.na(BPMDPBPD)) { # Proceeds without non-responses
    DBP_adj <- 15.6 + (0.83 * BPMDPBPD)
  } else {
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
#' @param ANYMED2 An integer indicating whether the respondent is on medication for hypertension.
#'   - 1: Yes
#'   - 0: No
#' @param CCC_32 An optional integer indicating whether the respondent is actually on medication for hypertension.
#'   - 1: Yes
#'   - 2: No (default)
#' @param CARDIOV An optional integer indicating the presence of cardiovascular disease, affecting medication status.
#'   - 1: Yes
#'   - 2: No (default)
#' @param DIABX An optional integer indicating the presence of diabetes, affecting blood pressure thresholds.
#'   - 1: Yes
#'   - 2: No (default)
#' @param CKD An optional integer indicating the presence of chronic kidney disease, affecting blood pressure thresholds.
#'   - 1: Yes
#'   - 2: No (default)
#'
#' @return An integer representing the hypertension status:
#'   - 1: High blood pressure (BP ≥ 140/90 mmHg (or ≥ 130/80 mmHg if diabetes or CKD) or on hypertension medication)
#'   - 2: Normal blood pressure (BP < 140/90 mmHg (or < 130/80 mmHg if diabetes or CKD) and not on hypertension medication)
#'   - NA(b): Invalid input or non-response
#'
#' @examples
#'
#' # Example 1: Respondent has systolic BP = 150, diastolic BP = 95, and on medication.
#' determine_hypertension(BPMDPBPS = 150, BPMDPBPD = 95, ANYMED2 = 1)
#' # Output: 1 (High blood pressure due to systolic BP, diastolic BP, and medication usage).
#'
#' # Example 2: Respondent has systolic BP = 120, diastolic BP = 80, and not on medication.
#' determine_hypertension(BPMDPBPS = 120, BPMDPBPD = 80, ANYMED2 = 2)
#' # Output: 2 (Normal blood pressure as BP is below 140/90 mmHg and not on medication).
#'
#' @export
determine_hypertension <- function(BPMDPBPS, BPMDPBPD, ANYMED2, CCC_32 = 2, CARDIOV = 2, DIABX = 2, CKD = 2) {
  highsys140 <- NA
  highdias90 <- NA
  highBP14090 <- haven::tagged_na("b")
  
  # Set ANYMED2 to 0 if CCC_32 = 2 and either CARDIOV, CKD, or DIABX = 1
  if ((!is.na(CCC_32) && CCC_32 == 2) && ((!is.na(CARDIOV) && CARDIOV == 1) || (!is.na(CKD) && CKD == 1) || (!is.na(DIABX) && DIABX == 1))) {
    ANYMED2 <- 0
  } else if (!is.na(ANYMED2) && ANYMED2 == "NA(b)") {
    ANYMED2 <- NA
  }
  
  if ((BPMDPBPS < 0) || (BPMDPBPS >= 996) || is.na(BPMDPBPS) || (BPMDPBPD < 0) || (BPMDPBPD >= 996) || is.na(BPMDPBPD)) {
    if (!is.na(ANYMED2) && ANYMED2 == 1) {
      highBP14090 <- 1
      return(highBP14090)
    } else {
      return(highBP14090)
    }
  }
  
  # Check conditions and assign values to highsys140 and highdias90
  if ((!is.na(DIABX) && DIABX == 1) || (!is.na(CKD) && CKD == 1)) {
    if (130 <= BPMDPBPS && BPMDPBPS < 996) {
      highsys140 <- 1
    } else if (0 <= BPMDPBPS && BPMDPBPS < 130) {
      highsys140 <- 2
    } else {
      return(highBP14090)
    }
    
    if (80 <= BPMDPBPD && BPMDPBPD < 996) {
      highdias90 <- 1
    } else if (0 <= BPMDPBPD && BPMDPBPD < 80) {
      highdias90 <- 2
    } else {
      return(highBP14090)
    }
  } else {
    if (140 <= BPMDPBPS && BPMDPBPS < 996) {
      highsys140 <- 1
    } else if (0 <= BPMDPBPS && BPMDPBPS < 140) {
      highsys140 <- 2
    } else {
      return(highBP14090)
    }
    
    if (90 <= BPMDPBPD && BPMDPBPD < 996) {
      highdias90 <- 1
    } else if (0 <= BPMDPBPD && BPMDPBPD < 90) {
      highdias90 <- 2
    } else {
      return(highBP14090)
    }
  }
  
  # Calculate highBP14090
  if (highsys140 == 1 || highdias90 == 1) {
    highBP14090 <- 1
  } else if (highsys140 == 2 && highdias90 == 2) {
    if (ANYMED2 == 0 || is.na(ANYMED2)) {
      highBP14090 <- 2
    } else {
      highBP14090 <- 1
    }
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
#' @param ANYMED2 An integer indicating whether the respondent is on medication for hypertension.
#'   - 1: Yes
#'   - 0: No
#' @param CCC_32 An optional integer indicating whether the respondent is actually on medication for hypertension.
#'   - 1: Yes
#'   - 2: No (default)
#' @param CARDIOV An optional integer indicating the presence of cardiovascular disease, affecting medication status.
#'   - 1: Yes
#'   - 2: No (default)
#' @param DIABX An optional integer indicating the presence of diabetes, affecting blood pressure thresholds.
#'   - 1: Yes
#'   - 2: No (default)
#' @param CKD An optional integer indicating the presence of chronic kidney disease, affecting blood pressure thresholds.
#'   - 1: Yes
#'   - 2: No (default)
#'
#' @return An integer representing the hypertension status:
#'   - 1: High blood pressure (adjusted BP ≥ 140/90 mmHg (or ≥ 130/80 mmHg if diabetes or CKD) or on hypertension medication)
#'   - 2: Normal blood pressure (adjusted BP < 140/90 mmHg (or < 130/80 mmHg if diabetes or CKD) and not on hypertension medication)
#'   - NA(b): Invalid input or non-response
#'
#' @examples
#'
#' # Example 1: Respondent has adjusted SBP = 150, adjusted DBP = 95, and on medication.
#' determine_adjusted_hypertension(SBP_adj = 150, DBP_adj = 95, ANYMED2 = 1)
#' # Output: 1 (High blood pressure due to adjusted SBP, adjusted DBP, and medication usage).
#'
#' # Example 2: Respondent has adjusted SBP = 120, adjusted DBP = 80, and not on medication.
#' determine_adjusted_hypertension(SBP_adj = 120, DBP_adj = 80, ANYMED2 = 2)
#' # Output: 2 (Normal blood pressure as adjusted BP is below 140/90 mmHg and not on medication).
#'
#' @export
determine_adjusted_hypertension <- function(SBP_adj, DBP_adj, ANYMED2, CCC_32 = 2, CARDIOV = 2, DIABX = 2, CKD = 2) {
  highsys140_adj <- NA
  highdias90_adj <- NA
  highBP14090_adj <- haven::tagged_na("b")
  
  # Set ANYMED2 to 0 if CCC_32 = 2 and either CARDIOV, CKD, or DIABX = 1
  if ((!is.na(CCC_32) && CCC_32 == 2) && ((!is.na(CARDIOV) && CARDIOV == 1) || (!is.na(CKD) && CKD == 1) || (!is.na(DIABX) && DIABX == 1))) {
    ANYMED2 <- 0
  } else if (!is.na(ANYMED2) && ANYMED2 == "NA(b)") {
    ANYMED2 <- NA
  }
  
  if ((SBP_adj < 0) || (SBP_adj >= 996) || is.na(SBP_adj) || (DBP_adj < 0) || (DBP_adj >= 996) || is.na(DBP_adj)) {
    if (!is.na(ANYMED2) && ANYMED2 == 1) {
      highBP14090_adj <- 1
      return(highBP14090_adj)
    } else {
      return(highBP14090_adj)
    }
  }
  
  # Check conditions and assign values to highsys140_adj and highdias90_adj
  if ((!is.na(DIABX) && DIABX == 1) || (!is.na(CKD) && CKD == 1)) {
    if (130 <= SBP_adj && SBP_adj < 996) {
      highsys140_adj <- 1
    } else if (0 <= SBP_adj && SBP_adj < 130) {
      highsys140_adj <- 2
    } else {
      return(highBP14090_adj)
    }
    
    if (80 <= DBP_adj && DBP_adj < 996) {
      highdias90_adj <- 1
    } else if (0 <= DBP_adj && DBP_adj < 80) {
      highdias90_adj <- 2
    } else {
      return(highBP14090_adj)
    }
  } else {
    if (140 <= SBP_adj && SBP_adj < 996) {
      highsys140_adj <- 1
    } else if (0 <= SBP_adj && SBP_adj < 140) {
      highsys140_adj <- 2
    } else {
      return(highBP14090_adj)
    }
    
    if (90 <= DBP_adj && DBP_adj < 996) {
      highdias90_adj <- 1
    } else if (0 <= DBP_adj && DBP_adj < 90) {
      highdias90_adj <- 2
    } else {
      return(highBP14090_adj)
    }
  }
  
  # Initialize and calculate highBP14090_adj
  if (highsys140_adj == 1 || highdias90_adj == 1) {
    highBP14090_adj <- 1
  } else if (highsys140_adj == 2 && highdias90_adj == 2) {
    if (ANYMED2 == 0 || is.na(ANYMED2)) {
      highBP14090_adj <- 2
    } else {
      highBP14090_adj <- 1
    }
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
#' @param ANYMED2 An integer indicating whether the respondent is on medication for hypertension.
#'   - 1: Yes
#'   - 0: No
#' @param CCC_32 An optional integer indicating whether the respondent is actually on medication for hypertension.
#'   - 1: Yes
#'   - 2: No (default)
#' @param CARDIOV An optional integer indicating the presence of cardiovascular disease, affecting medication status.
#'   - 1: Yes
#'   - 2: No (default)
#' @param DIABX An optional integer indicating the presence of diabetes, affecting blood pressure thresholds.
#'   - 1: Yes
#'   - 2: No (default)
#' @param CKD An optional integer indicating the presence of chronic kidney disease, affecting blood pressure thresholds.
#'   - 1: Yes
#'   - 2: No (default)
#'
#' @return An integer representing the hypertension status:
#'   - 1: Hypertension controlled (BP < 140/90 mmHg (or < 130/80 mmHg if diabetes or CKD) when on hypertension medication)
#'   - 2: Hypertension not controlled (BP ≥ 140/90 mmHg (or ≥ 130/80 mmHg if diabetes or CKD) when on hypertension medication)
#'   - NA(b): Invalid input or non-response
#'
#' @examples
#'
#' # Example 1: Respondent has systolic BP = 150, diastolic BP = 95, and on medication.
#' determine_controlled_hypertension(BPMDPBPS = 150, BPMDPBPD = 95, ANYMED2 = 1)
#' # Output: 2 (Hypertension not controlled due to high SBP and SBP despite medication usage).
#'
#' # Example 2: Respondent has systolic BP = 120, diastolic BP = 80, and on medication.
#' determine_controlled_hypertension(BPMDPBPS = 120, BPMDPBPD = 80, ANYMED2 = 1)
#' # Output: 1 (Hypertension controlled as BP is below 140/90 mmHg and on medication).
#'
#' @export
determine_controlled_hypertension <- function(BPMDPBPS, BPMDPBPD, ANYMED2, CCC_32 = 2, CARDIOV = 2, DIABX = 2, CKD = 2) {
  highsys140 <- NA
  highdias90 <- NA
  Control14090 <- haven::tagged_na("b")
  
  # Set ANYMED2 to 0 if CCC_32 = 2 and either CARDIOV, CKD, or DIABX = 1
  if ((!is.na(CCC_32) && CCC_32 == 2) && ((!is.na(CARDIOV) && CARDIOV == 1) || (!is.na(CKD) && CKD == 1) || (!is.na(DIABX) && DIABX == 1))) {
    ANYMED2 <- 0
  } else if (!is.na(ANYMED2) && ANYMED2 == "NA(b)") {
    ANYMED2 <- NA
  }
  
  if ((BPMDPBPS < 0) || (BPMDPBPS >= 996) || is.na(BPMDPBPS) || (BPMDPBPD < 0) || (BPMDPBPD >= 996) || is.na(BPMDPBPD)) {
    return(Control14090)
  }
  
  # Check conditions and assign values to highsys140 and highdias90
  if ((!is.na(DIABX) && DIABX == 1) || (!is.na(CKD) && CKD == 1)) {
    if (130 <= BPMDPBPS && BPMDPBPS < 996) {
      highsys140 <- 1
    } else if (0 <= BPMDPBPS && BPMDPBPS < 130) {
      highsys140 <- 2
    } else {
      return(Control14090)
    }
    
    if (80 <= BPMDPBPD && BPMDPBPD < 996) {
      highdias90 <- 1
    } else if (0 <= BPMDPBPD && BPMDPBPD < 80) {
      highdias90 <- 2
    } else {
      return(Control14090)
    }
  } else {
    if (140 <= BPMDPBPS && BPMDPBPS < 996) {
      highsys140 <- 1
    } else if (0 <= BPMDPBPS && BPMDPBPS < 140) {
      highsys140 <- 2
    } else {
      return(Control14090)
    }
    
    if (90 <= BPMDPBPD && BPMDPBPD < 996) {
      highdias90 <- 1
    } else if (0 <= BPMDPBPD && BPMDPBPD < 90) {
      highdias90 <- 2
    } else {
      return(Control14090)
    }
  }
  
  # Check the conditions using nested ifelse statements
  if (!is.na(ANYMED2) && ANYMED2 == 1) {
    Control14090 <- ifelse(highsys140 == 1 || highdias90 == 1, 2,
                           ifelse(highsys140 == 2 && highdias90 == 2, 1, NA)
    )
  } else {
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
#' @param ANYMED2 An integer indicating whether the respondent is on medication for hypertension.
#'   - 1: Yes
#'   - 0: No
#' @param CCC_32 An optional integer indicating whether the respondent is actually on medication for hypertension.
#'   - 1: Yes
#'   - 2: No (default)
#' @param CARDIOV An optional integer indicating the presence of cardiovascular disease, affecting medication status.
#'   - 1: Yes
#'   - 2: No (default)
#' @param DIABX An optional integer indicating the presence of diabetes, affecting blood pressure thresholds.
#'   - 1: Yes
#'   - 2: No (default)
#' @param CKD An optional integer indicating the presence of chronic kidney disease, affecting blood pressure thresholds.
#'   - 1: Yes
#'   - 2: No (default)
#'
#' @return An integer representing the hypertension status:
#'   - 1: Hypertension controlled (BP < 140/90 mmHg (or < 130/80 mmHg if diabetes or CKD) when on hypertension medication)
#'   - 2: Hypertension not controlled (BP ≥ 140/90 mmHg (or ≥ 130/80 mmHg if diabetes or CKD) when on hypertension medication)
#'   - NA(b): Invalid input or non-response
#'
#' @examples
#'
#' # Example 1: Respondent has adjusted SBP = 150, adjusted DBP = 95, and on medication.
#' determine_controlled_adjusted_hypertension(SBP_adj = 150, DBP_adj = 95, ANYMED2 = 1)
#' # Output: 2 (Hypertension not controlled due to high adjusted SBP and DBP despite medication usage).
#'
#' # Example 2: Respondent has adjusted SBP = 120, adjusted DBP = 80, and on medication.
#' determine_controlled_adjusted_hypertension(SBP_adj = 120, DBP_adj = 80, ANYMED2 = 1)
#' # Output: 1 (Hypertension controlled as adjusted BP is below 140/90 mmHg and on medication).
#'
#' @export
determine_controlled_adjusted_hypertension <- function(SBP_adj, DBP_adj, ANYMED2, CCC_32 = 2, CARDIOV = 2, DIABX = 2, CKD = 2) {
  highsys140_adj <- NA
  highdias90_adj <- NA
  Control14090_adj <- haven::tagged_na("b")
  
  # Set ANYMED2 to 0 if CCC_32 = 2 and either CARDIOV, CKD, or DIABX = 1
  if ((!is.na(CCC_32) && CCC_32 == 2) && ((!is.na(CARDIOV) && CARDIOV == 1) || (!is.na(CKD) && CKD == 1) || (!is.na(DIABX) && DIABX == 1))) {
    ANYMED2 <- 0
  } else if (!is.na(ANYMED2) && ANYMED2 == "NA(b)") {
    ANYMED2 <- NA
  }
  
  if ((SBP_adj < 0) || (SBP_adj >= 996) || is.na(SBP_adj) || (DBP_adj < 0) || (DBP_adj >= 996) || is.na(DBP_adj)) {
    return(Control14090_adj)
  }
  
  # Check conditions and assign values to highsys140_adj and highdias90_adj
  if ((!is.na(DIABX) && DIABX == 1) || (!is.na(CKD) && CKD == 1)) {
    if (130 <= SBP_adj && SBP_adj < 996) {
      highsys140_adj <- 1
    } else if (0 <= SBP_adj && SBP_adj < 130) {
      highsys140_adj <- 2
    } else {
      return(Control14090_adj)
    }
    
    if (80 <= DBP_adj && DBP_adj < 996) {
      highdias90_adj <- 1
    } else if (0 <= DBP_adj && DBP_adj < 80) {
      highdias90_adj <- 2
    } else {
      return(Control14090_adj)
    }
  } else {
    if (140 <= SBP_adj && SBP_adj < 996) {
      highsys140_adj <- 1
    } else if (0 <= SBP_adj && SBP_adj < 140) {
      highsys140_adj <- 2
    } else {
      return(Control14090_adj)
    }
    
    if (90 <= DBP_adj && DBP_adj < 996) {
      highdias90_adj <- 1
    } else if (0 <= DBP_adj && DBP_adj < 90) {
      highdias90_adj <- 2
    } else {
      return(Control14090_adj)
    }
  }
  
  # Check the conditions using nested ifelse statements
  if (!is.na(ANYMED2) && ANYMED2 == 1) {
    Control14090_adj <- ifelse(highsys140_adj == 1 || highdias90_adj == 1, 2,
                               ifelse(highsys140_adj == 2 && highdias90_adj == 2, 1, NA)
    )
  } else {
    Control14090_adj <- 2
  }
  
  return(Control14090_adj)
}