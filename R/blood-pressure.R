#' @title Adjusted systolic blood pressure
#'
#' @description This function adjusts systolic blood pressure based on the respondent's systolic average blood pressure across
#' six measurements. The adjustment is made using specific correction factors. The adjusted systolic blood pressure
#' is returned as a numeric value.
#'
#' @param BPMDPBPS [numeric] A numeric representing the respondent's systolic average blood pressure (in mmHg) across six measurements.
#'
#' @return [numeric] The adjusted systolic blood pressure as a numeric.
#'
#' @details Blood pressure measurements in survey settings may require adjustment to account for
#'          measurement conditions and equipment differences. This function applies a standardized adjustment
#'          using the formula: SBP_adj = 11.4 + (0.93 * BPMDPBPS).
#'
#'          **Missing Data Codes:**
#'          - `996`: Valid skip (e.g., measurement not taken). Handled as `haven::tagged_na("a")`.
#'          - `997-999`: Don't know, refusal, or not stated. Handled as `haven::tagged_na("b")`.
#'
#' @examples
#' # Scalar usage: Single respondent
#' # Example: Adjust for a respondent with average systolic blood pressure of 120 mmHg.
#' adjust_SBP(BPMDPBPS = 120)
#' # Output: 123
#'
#' # Example: Adjust for a respondent with a non-response systolic blood pressure of 996.
#' result <- adjust_SBP(BPMDPBPS = 996)
#' result # Shows: NA
#' haven::is_tagged_na(result, "a") # Shows: TRUE (confirms it's tagged NA(a))
#' format(result, tag = TRUE) # Shows: "NA(a)" (displays the tag)
#'
#' # Multiple respondents
#' adjust_SBP(BPMDPBPS = c(120, 130, 140))
#' # Returns: c(123, 132.3, 141.6)
#'
#' # Database usage: Applied to survey datasets
#' library(dplyr)
#' # dataset %>%
#' #   mutate(sbp_adj = adjust_SBP(BPMDPBPS))
#'
#' @seealso [adjust_DBP()] for diastolic blood pressure adjustment, [determine_hypertension()] for hypertension classification
#' @export
adjust_SBP <- function(BPMDPBPS) {
  dplyr::case_when(
    # Valid skip
    BPMDPBPS == 996 ~ haven::tagged_na("a"),
    # Don't know, refusal, not stated
    BPMDPBPS < 0 | BPMDPBPS %in% 997:999 ~ haven::tagged_na("b"),

    # Apply adjustment formula
    TRUE ~ 11.4 + (0.93 * BPMDPBPS)
  )
}

#' @title Adjusted diastolic blood pressure
#'
#' @description This function adjusts diastolic blood pressure based on the respondent's diastolic average blood pressure across
#' six measurements. The adjustment is made using specific correction factors. The adjusted diastolic blood pressure
#' is returned as a numeric value.
#'
#' @param BPMDPBPD [numeric] A numeric representing the respondent's diastolic average blood pressure (in mmHg) across six measurements.
#'
#' @return [numeric] The adjusted diastolic blood pressure as a numeric.
#'
#' @details Blood pressure measurements in survey settings may require adjustment to account for
#'          measurement conditions and equipment differences. This function applies a standardized adjustment
#'          using the formula: DBP_adj = 15.6 + (0.83 * BPMDPBPD).
#'
#'          **Missing Data Codes:**
#'          - `996`: Valid skip (e.g., measurement not taken). Handled as `haven::tagged_na("a")`.
#'          - `997-999`: Don't know, refusal, or not stated. Handled as `haven::tagged_na("b")`.
#'
#' @examples
#' # Scalar usage: Single respondent
#' # Example: Adjust for a respondent with average diastolic blood pressure of 80 mmHg.
#' adjust_DBP(BPMDPBPD = 80)
#' # Output: 82
#'
#' # Example: Adjust for a respondent with a non-response diastolic blood pressure of 996.
#' result <- adjust_DBP(BPMDPBPD = 996)
#' result # Shows: NA
#' haven::is_tagged_na(result, "a") # Shows: TRUE (confirms it's tagged NA(a))
#' format(result, tag = TRUE) # Shows: "NA(a)" (displays the tag)
#'
#' # Multiple respondents
#' adjust_DBP(BPMDPBPD = c(80, 90, 100))
#' # Returns: c(82, 90.3, 98.6)
#'
#' # Database usage: Applied to survey datasets
#' library(dplyr)
#' # dataset %>%
#' #   mutate(dbp_adj = adjust_DBP(BPMDPBPD))
#'
#' @seealso [adjust_SBP()] for systolic blood pressure adjustment, [determine_hypertension()] for hypertension classification
#' @export
adjust_DBP <- function(BPMDPBPD) {
  dplyr::case_when(
    # Valid skip
    BPMDPBPD == 996 ~ haven::tagged_na("a"),
    # Don't know, refusal, not stated
    BPMDPBPD < 0 | BPMDPBPD %in% 997:999 ~ haven::tagged_na("b"),

    # Apply adjustment formula
    TRUE ~ 15.6 + (0.83 * BPMDPBPD)
  )
}

#' @title Hypertension derived variable
#'
#' @description
#' This function determines the hypertension status of a respondent based on their systolic and diastolic blood pressure measurements and medication usage.
#'
#' @param BPMDPBPS [integer] An integer representing the systolic blood pressure measurement of the respondent.
#' @param BPMDPBPD [integer] An integer representing the diastolic blood pressure measurement of the respondent.
#' @param ANYMED2 [integer] An integer indicating whether the respondent is on medication for hypertension.
#'   - 1: Yes
#'   - 0: No
#' @param CCC_32 [integer] An optional integer indicating whether the respondent is actually on medication for hypertension.
#'   - 1: Yes
#'   - 2: No (default)
#' @param CARDIOV [integer] An optional integer indicating the presence of cardiovascular disease, affecting medication status.
#'   - 1: Yes
#'   - 2: No (default)
#' @param DIABX [integer] An optional integer indicating the presence of diabetes, affecting blood pressure thresholds.
#'   - 1: Yes
#'   - 2: No (default)
#' @param CKD [integer] An optional integer indicating the presence of chronic kidney disease, affecting blood pressure thresholds.
#'   - 1: Yes
#'   - 2: No (default)
#'
#' @return [integer] The hypertension status:
#'   - 1: High blood pressure (BP >= 140/90 mmHg (or >= 130/80 mmHg if diabetes or CKD) or on hypertension medication)
#'   - 2: Normal blood pressure (BP < 140/90 mmHg (or < 130/80 mmHg if diabetes or CKD) and not on hypertension medication)
#'   - `haven::tagged_na("a")`: Not applicable
#'   - `haven::tagged_na("b")`: Missing
#'
#' @details This function implements clinical guidelines for hypertension classification:
#'
#'          **Blood Pressure Thresholds:**
#'          - General population: >= 140/90 mmHg indicates hypertension
#'          - Diabetes or CKD patients: >= 130/80 mmHg indicates hypertension (lower threshold)
#'
#'          **Medication Logic:**
#'          - Anyone taking hypertension medication is classified as hypertensive
#'          - Medication status may be adjusted based on comorbidities (diabetes, CKD, cardiovascular disease)
#'
#'          **Missing Data Codes:**
#'          - `BPMDPBPS`, `BPMDPBPD`:
#'            - `996`: Valid skip. Handled as `haven::tagged_na("a")`.
#'            - `997-999`: Don't know, refusal, or not stated. Handled as `haven::tagged_na("b")`.
#'          - `ANYMED2`:
#'            - Tagged NA "a": Valid skip.
#'            - Tagged NA "b": Don't know, refusal, or not stated.
#'          - `CCC_32`, `CARDIOV`, `DIABX`, `CKD`:
#'            - `6`: Valid skip. Handled as `haven::tagged_na("a")`.
#'            - `7-9`: Don't know, refusal, or not stated. Handled as `haven::tagged_na("b")`.
#'
#' @examples
#' # Scalar usage: Single respondent
#' # Example 1: Respondent has systolic BP = 150, diastolic BP = 95, and on medication.
#' determine_hypertension(BPMDPBPS = 150, BPMDPBPD = 95, ANYMED2 = 1)
#' # Output: 1 (High blood pressure due to systolic BP, diastolic BP, and medication usage).
#'
#' # Example 2: Respondent has systolic BP = 120, diastolic BP = 80, and not on medication.
#' determine_hypertension(BPMDPBPS = 120, BPMDPBPD = 80, ANYMED2 = 0)
#' # Output: 2 (Normal blood pressure as BP is below 140/90 mmHg and not on medication).
#'
#' # Example 3: Respondent has non-response BP values of 996 for both systolic and diastolic.
#' result <- determine_hypertension(BPMDPBPS = 996, BPMDPBPD = 996, ANYMED2 = 0)
#' result # Shows: NA
#' haven::is_tagged_na(result, "a") # Shows: TRUE (confirms it's tagged NA(a))
#' format(result, tag = TRUE) # Shows: "NA(a)" (displays the tag)
#'
#' # Multiple respondents
#' determine_hypertension(
#'   BPMDPBPS = c(150, 120, 135), BPMDPBPD = c(95, 80, 85),
#'   ANYMED2 = c(1, 0, 1), DIABX = c(2, 2, 1)
#' )
#' # Returns: c(1, 2, 1)
#'
#' @seealso [adjust_SBP()], [adjust_DBP()] for blood pressure adjustment, [determine_adjusted_hypertension()] for adjusted BP classification
#' @export
determine_hypertension <- function(BPMDPBPS, BPMDPBPD, ANYMED2, CCC_32 = 2, CARDIOV = 2, DIABX = 2, CKD = 2) {
  # Adjust medication status
  ANYMED2 <- dplyr::case_when(
    CCC_32 == 2 & (CARDIOV == 1 | CKD == 1 | DIABX == 1) ~ 0,
    haven::is_tagged_na(ANYMED2, "a") ~ haven::tagged_na("a"),
    haven::is_tagged_na(ANYMED2, "b") ~ haven::tagged_na("b"),
    TRUE ~ as.numeric(ANYMED2)
  )
  
  # High systolic
  highsys140 <- dplyr::case_when(
    BPMDPBPS == 996 ~ haven::tagged_na("a"),
    BPMDPBPS %in% 997:999 ~ haven::tagged_na("b"),
    is.na(BPMDPBPS) ~ haven::tagged_na("b"),
    
    (DIABX == 1 | CKD == 1) & !is.na(BPMDPBPS) & BPMDPBPS >= 130 ~ 1,
    (DIABX == 1 | CKD == 1) & !is.na(BPMDPBPS) & BPMDPBPS < 130 ~ 2,
    !(DIABX == 1 | CKD == 1) & !is.na(BPMDPBPS) & BPMDPBPS >= 140 ~ 1,
    !(DIABX == 1 | CKD == 1) & !is.na(BPMDPBPS) & BPMDPBPS < 140 ~ 2,
    .default = haven::tagged_na("b")
  )
  
  # High diastolic
  highdias90 <- dplyr::case_when(
    BPMDPBPD == 996 ~ haven::tagged_na("a"),
    BPMDPBPD %in% 997:999 ~ haven::tagged_na("b"),
    is.na(BPMDPBPD) ~ haven::tagged_na("b"),
    
    (DIABX == 1 | CKD == 1) & !is.na(BPMDPBPD) & BPMDPBPD >= 80 ~ 1,
    (DIABX == 1 | CKD == 1) & !is.na(BPMDPBPD) & BPMDPBPD < 80 ~ 2,
    !(DIABX == 1 | CKD == 1) & !is.na(BPMDPBPD) & BPMDPBPD >= 90 ~ 1,
    !(DIABX == 1 | CKD == 1) & !is.na(BPMDPBPD) & BPMDPBPD < 90 ~ 2,
    .default = haven::tagged_na("b")
  )
  
  # Overall hypertension
  dplyr::case_when(
    !is.na(ANYMED2) & ANYMED2 == 1 ~ 1,
    highsys140 == 1 | highdias90 == 1 ~ 1,
    highsys140 == 2 & highdias90 == 2 & (ANYMED2 == 0 | is.na(ANYMED2)) ~ 2,
    haven::is_tagged_na(highsys140, "a") | haven::is_tagged_na(highdias90, "a") ~ haven::tagged_na("a"),
    haven::is_tagged_na(highsys140, "b") | haven::is_tagged_na(highdias90, "b") |
      haven::is_tagged_na(ANYMED2, "b") ~ haven::tagged_na("b"),
    .default = haven::tagged_na("b")
  )
}

#' @title Hypertension derived variable with adjusted blood pressures
#'
#' @description
#' This function determines the hypertension status of a respondent based on their adjusted systolic and diastolic blood pressure measurements and medication usage.
#'
#' @param SBP_adj [integer] An integer representing the adjusted systolic blood pressure measurement of the respondent.
#' @param DBP_adj [integer] An integer representing the adjusted diastolic blood pressure measurement of the respondent.
#' @param ANYMED2 [integer] An integer indicating whether the respondent is on medication for hypertension.
#'   - 1: Yes
#'   - 0: No
#' @param CCC_32 [integer] An optional integer indicating whether the respondent is actually on medication for hypertension.
#'   - 1: Yes
#'   - 2: No (default)
#' @param CARDIOV [integer] An optional integer indicating the presence of cardiovascular disease, affecting medication status.
#'   - 1: Yes
#'   - 2: No (default)
#' @param DIABX [integer] An optional integer indicating the presence of diabetes, affecting blood pressure thresholds.
#'   - 1: Yes
#'   - 2: No (default)
#' @param CKD [integer] An optional integer indicating the presence of chronic kidney disease, affecting blood pressure thresholds.
#'   - 1: Yes
#'   - 2: No (default)
#'
#' @return [integer] The hypertension status:
#'   - 1: High blood pressure (adjusted BP ≥ 140/90 mmHg (or ≥ 130/80 mmHg if diabetes or CKD) or on hypertension medication)
#'   - 2: Normal blood pressure (adjusted BP < 140/90 mmHg (or < 130/80 mmHg if diabetes or CKD) and not on hypertension medication)
#'   - `haven::tagged_na("a")`: Not applicable
#'   - `haven::tagged_na("b")`: Missing
#'
#' @details This function implements clinical guidelines for hypertension classification using adjusted blood pressure values:
#'
#'          **Blood Pressure Thresholds:**
#'          - General population: >= 140/90 mmHg indicates hypertension
#'          - Diabetes or CKD patients: >= 130/80 mmHg indicates hypertension (lower threshold)
#'
#'          **Medication Logic:**
#'          - Anyone taking hypertension medication is classified as hypertensive
#'          - Medication status may be adjusted based on comorbidities (diabetes, CKD, cardiovascular disease)
#'
#'          **Missing Data Codes:**
#'          - `SBP_adj`, `DBP_adj`:
#'            - `996`: Valid skip. Handled as `haven::tagged_na("a")`.
#'            - `997-999`: Don't know, refusal, or not stated. Handled as `haven::tagged_na("b")`.
#'          - `ANYMED2`:
#'            - Tagged NA "a": Valid skip.
#'            - Tagged NA "b": Don't know, refusal, or not stated.
#'          - `CCC_32`, `CARDIOV`, `DIABX`, `CKD`:
#'            - `6`: Valid skip. Handled as `haven::tagged_na("a")`.
#'            - `7-9`: Don't know, refusal, or not stated. Handled as `haven::tagged_na("b")`.
#'
#' @examples
#' # Scalar usage: Single respondent
#' # Example 1: Respondent has adjusted SBP = 150, adjusted DBP = 95, and on medication.
#' determine_adjusted_hypertension(SBP_adj = 150, DBP_adj = 95, ANYMED2 = 1)
#' # Output: 1 (High blood pressure due to adjusted SBP, adjusted DBP, and medication usage).
#'
#' # Example 2: Respondent has adjusted SBP = 120, adjusted DBP = 80, and not on medication.
#' determine_adjusted_hypertension(SBP_adj = 120, DBP_adj = 80, ANYMED2 = 2)
#' # Output: 2 (Normal blood pressure as adjusted BP is below 140/90 mmHg and not on medication).
#'
#' # Example 3: Respondent has non-response BP values of 996 for both systolic and diastolic.
#' result <- determine_adjusted_hypertension(SBP_adj = 996, DBP_adj = 996, ANYMED2 = 0)
#' result # Shows: NA
#' haven::is_tagged_na(result, "a") # Shows: TRUE (confirms it's tagged NA(a))
#' format(result, tag = TRUE) # Shows: "NA(a)" (displays the tag)
#'
#' # Multiple respondents
#' determine_adjusted_hypertension(
#'   SBP_adj = c(150, 120, 135), DBP_adj = c(95, 80, 85),
#'   ANYMED2 = c(1, 0, 1), DIABX = c(2, 2, 1)
#' )
#' # Returns: c(1, 2, 1)
#'
#' @seealso [determine_hypertension()] for unadjusted BP classification
#' @export
determine_adjusted_hypertension <- function(
    SBP_adj, DBP_adj, ANYMED2, CCC_32 = 2, CARDIOV = 2, DIABX = 2, CKD = 2
) {
  # Step 1 — Adjust ANYMED2 based on conditions
  ANYMED2 <- dplyr::case_when(
    CCC_32 == 2 & (CARDIOV == 1 | CKD == 1 | DIABX == 1) ~ 0,
    haven::is_tagged_na(ANYMED2, "a") ~ haven::tagged_na("a"),
    haven::is_tagged_na(ANYMED2, "b") ~ haven::tagged_na("b"),
    TRUE ~ as.numeric(ANYMED2)
  )
  
  # Step 2 — High systolic status
  highsys140 <- dplyr::case_when(
    # Valid skip
    SBP_adj == 996 ~ haven::tagged_na("a"),
    # DK/refused/not stated
    SBP_adj %in% 997:999 ~ haven::tagged_na("b"),
    # Missing value handling
    is.na(SBP_adj) ~ haven::tagged_na("b"),
    
    # Disease-based cutoffs
    (DIABX == 1 | CKD == 1) & !is.na(SBP_adj) & SBP_adj >= 130 ~ 1,
    (DIABX == 1 | CKD == 1) & !is.na(SBP_adj) & SBP_adj < 130 ~ 2,
    !(DIABX == 1 | CKD == 1) & !is.na(SBP_adj) & SBP_adj >= 140 ~ 1,
    !(DIABX == 1 | CKD == 1) & !is.na(SBP_adj) & SBP_adj < 140 ~ 2,
    .default = haven::tagged_na("b")
  )
  
  # Step 3 — High diastolic status
  highdias90 <- dplyr::case_when(
    DBP_adj == 996 ~ haven::tagged_na("a"),
    DBP_adj %in% 997:999 ~ haven::tagged_na("b"),
    is.na(DBP_adj) ~ haven::tagged_na("b"),
    
    (DIABX == 1 | CKD == 1) & !is.na(DBP_adj) & DBP_adj >= 80 ~ 1,
    (DIABX == 1 | CKD == 1) & !is.na(DBP_adj) & DBP_adj < 80 ~ 2,
    !(DIABX == 1 | CKD == 1) & !is.na(DBP_adj) & DBP_adj >= 90 ~ 1,
    !(DIABX == 1 | CKD == 1) & !is.na(DBP_adj) & DBP_adj < 90 ~ 2,
    .default = haven::tagged_na("b")
  )
  
  # Step 4 — Overall hypertension status (same logic as original)
  dplyr::case_when(
    # On medication → hypertensive
    !is.na(ANYMED2) & ANYMED2 == 1 ~ 1,
    
    # High systolic or diastolic → hypertensive
    highsys140 == 1 | highdias90 == 1 ~ 1,
    
    # Normal both, no meds → normotensive
    highsys140 == 2 & highdias90 == 2 & (ANYMED2 == 0 | is.na(ANYMED2)) ~ 2,
    
    # Valid skip anywhere → tagged NA(a)
    haven::is_tagged_na(highsys140, "a") | haven::is_tagged_na(highdias90, "a") ~ haven::tagged_na("a"),
    
    # Don’t know/refused anywhere → tagged NA(b)
    haven::is_tagged_na(highsys140, "b") | haven::is_tagged_na(highdias90, "b") |
      haven::is_tagged_na(ANYMED2, "b") ~ haven::tagged_na("b"),
    
    # Fallback default
    .default = haven::tagged_na("b")
  )
}


#' @title Controlled hypertension derived variable
#'
#' @description
#' This function determines the controlled hypertension status of a respondent based on their systolic and diastolic blood pressure measurements and medication usage.
#'
#' @param BPMDPBPS [integer] An integer representing the systolic blood pressure measurement of the respondent.
#' @param BPMDPBPD [integer] An integer representing the diastolic blood pressure measurement of the respondent.
#' @param ANYMED2 [integer] An integer indicating whether the respondent is on medication for hypertension.
#'   - 1: Yes
#'   - 0: No
#' @param CCC_32 [integer] An optional integer indicating whether the respondent is actually on medication for hypertension.
#'   - 1: Yes
#'   - 2: No (default)
#' @param CARDIOV [integer] An optional integer indicating the presence of cardiovascular disease, affecting medication status.
#'   - 1: Yes
#'   - 2: No (default)
#' @param DIABX [integer] An optional integer indicating the presence of diabetes, affecting blood pressure thresholds.
#'   - 1: Yes
#'   - 2: No (default)
#' @param CKD [integer] An optional integer indicating the presence of chronic kidney disease, affecting blood pressure thresholds.
#'   - 1: Yes
#'   - 2: No (default)
#'
#' @return [integer] The hypertension status:
#'   - 1: Hypertension controlled (BP < 140/90 mmHg (or < 130/80 mmHg if diabetes or CKD) when on hypertension medication)
#'   - 2: Hypertension not controlled (BP >= 140/90 mmHg (or >= 130/80 mmHg if diabetes or CKD) when on hypertension medication)
#'   - `haven::tagged_na("a")`: Not applicable
#'   - `haven::tagged_na("b")`: Missing
#'
#' @details This function assesses whether a respondent's hypertension is controlled:
#'
#'          **Control Thresholds:**
#'          - General population: < 140/90 mmHg
#'          - Diabetes or CKD patients: < 130/80 mmHg
#'
#'          **Logic:**
#'          - Only applies to respondents taking hypertension medication.
#'          - If BP is below the threshold, hypertension is "controlled" (1).
#'          - If BP is at or above the threshold, it is "not controlled" (2).
#'
#'          **Missing Data Codes:**
#'          - `BPMDPBPS`, `BPMDPBPD`:
#'            - `996`: Valid skip. Handled as `haven::tagged_na("a")`.
#'            - `997-999`: Don't know, refusal, or not stated. Handled as `haven::tagged_na("b")`.
#'          - `ANYMED2`:
#'            - Tagged NA "a": Valid skip.
#'            - Tagged NA "b": Don't know, refusal, or not stated.
#'          - `CCC_32`, `CARDIOV`, `DIABX`, `CKD`:
#'            - `6`: Valid skip. Handled as `haven::tagged_na("a")`.
#'            - `7-9`: Don't know, refusal, or not stated. Handled as `haven::tagged_na("b")`.
#'
#' @examples
#' # Scalar usage: Single respondent
#' # Example 1: Respondent has systolic BP = 150, diastolic BP = 95, and on medication.
#' determine_controlled_hypertension(BPMDPBPS = 150, BPMDPBPD = 95, ANYMED2 = 1)
#' # Output: 2 (Hypertension not controlled due to high SBP and SBP despite medication usage).
#'
#' # Example 2: Respondent has systolic BP = 120, diastolic BP = 80, and on medication.
#' determine_controlled_hypertension(BPMDPBPS = 120, BPMDPBPD = 80, ANYMED2 = 1)
#' # Output: 1 (Hypertension controlled as BP is below 140/90 mmHg and on medication).
#'
#' # Example 3: Respondent has non-response BP values of 996 for both systolic and diastolic.
#' result <- determine_controlled_hypertension(BPMDPBPS = 996, BPMDPBPD = 996, ANYMED2 = 0)
#' result # Shows: NA
#' haven::is_tagged_na(result, "a") # Shows: TRUE (confirms it's tagged NA(a))
#' format(result, tag = TRUE) # Shows: "NA(a)" (displays the tag)
#'
#' # Multiple respondents
#' determine_controlled_hypertension(
#'   BPMDPBPS = c(150, 120, 135), BPMDPBPD = c(95, 80, 85),
#'   ANYMED2 = c(1, 1, 1), DIABX = c(2, 2, 1)
#' )
#' # Returns: c(2, 1, 2)
#'
#' @seealso [determine_controlled_adjusted_hypertension()] for controlled status with adjusted BP
#' @export
determine_controlled_hypertension <- function(BPMDPBPS, BPMDPBPD, ANYMED2, CCC_32 = 2, CARDIOV = 2, DIABX = 2, CKD = 2) {
  # Adjust medication status
  ANYMED2 <- dplyr::case_when(
    CCC_32 == 2 & (CARDIOV == 1 | CKD == 1 | DIABX == 1) ~ 0,
    haven::is_tagged_na(ANYMED2, "a") ~ haven::tagged_na("a"),
    haven::is_tagged_na(ANYMED2, "b") ~ haven::tagged_na("b"),
    TRUE ~ as.numeric(ANYMED2)
  )
  
  # High systolic
  highsys140 <- dplyr::case_when(
    BPMDPBPS == 996 ~ haven::tagged_na("a"),
    BPMDPBPS %in% 997:999 ~ haven::tagged_na("b"),
    is.na(BPMDPBPS) ~ haven::tagged_na("b"),
    
    (DIABX == 1 | CKD == 1) & !is.na(BPMDPBPS) & BPMDPBPS >= 130 ~ 1,
    (DIABX == 1 | CKD == 1) & !is.na(BPMDPBPS) & BPMDPBPS < 130 ~ 2,
    !(DIABX == 1 | CKD == 1) & !is.na(BPMDPBPS) & BPMDPBPS >= 140 ~ 1,
    !(DIABX == 1 | CKD == 1) & !is.na(BPMDPBPS) & BPMDPBPS < 140 ~ 2,
    .default = haven::tagged_na("b")
  )
  
  # High diastolic
  highdias90 <- dplyr::case_when(
    BPMDPBPD == 996 ~ haven::tagged_na("a"),
    BPMDPBPD %in% 997:999 ~ haven::tagged_na("b"),
    is.na(BPMDPBPD) ~ haven::tagged_na("b"),
    
    (DIABX == 1 | CKD == 1) & !is.na(BPMDPBPD) & BPMDPBPD >= 80 ~ 1,
    (DIABX == 1 | CKD == 1) & !is.na(BPMDPBPD) & BPMDPBPD < 80 ~ 2,
    !(DIABX == 1 | CKD == 1) & !is.na(BPMDPBPD) & BPMDPBPD >= 90 ~ 1,
    !(DIABX == 1 | CKD == 1) & !is.na(BPMDPBPD) & BPMDPBPD < 90 ~ 2,
    .default = haven::tagged_na("b")
  )
  
  # Controlled hypertension
  dplyr::case_when(
    ANYMED2 == 1 & (highsys140 == 1 | highdias90 == 1) ~ 2,  # Not controlled
    ANYMED2 == 1 & (highsys140 == 2 & highdias90 == 2) ~ 1,  # Controlled
    ANYMED2 == 0 ~ 2,
    
    haven::is_tagged_na(highsys140, "a") |
      haven::is_tagged_na(highdias90, "a") |
      haven::is_tagged_na(ANYMED2, "a") ~ haven::tagged_na("a"),
    
    haven::is_tagged_na(highsys140, "b") |
      haven::is_tagged_na(highdias90, "b") |
      haven::is_tagged_na(ANYMED2, "b") ~ haven::tagged_na("b"),
    
    .default = haven::tagged_na("b")
  )
}

#' @title Controlled hypertension derived variable with adjusted blood pressures
#'
#' @description
#' This function determines the controlled hypertension status of a respondent based on their adjusted systolic and diastolic blood pressure measurements and medication usage.
#'
#' @param SBP_adj [integer] An integer representing the adjusted systolic blood pressure measurement of the respondent.
#' @param DBP_adj [integer] An integer representing the adjusted diastolic blood pressure measurement of the respondent.
#' @param ANYMED2 [integer] An integer indicating whether the respondent is on medication for hypertension.
#'   - 1: Yes
#'   - 0: No
#' @param CCC_32 [integer] An optional integer indicating whether the respondent is actually on medication for hypertension.
#'   - 1: Yes
#'   - 2: No (default)
#' @param CARDIOV [integer] An optional integer indicating the presence of cardiovascular disease, affecting medication status.
#'   - 1: Yes
#'   - 2: No (default)
#' @param DIABX [integer] An optional integer indicating the presence of diabetes, affecting blood pressure thresholds.
#'   - 1: Yes
#'   - 2: No (default)
#' @param CKD [integer] An optional integer indicating the presence of chronic kidney disease, affecting blood pressure thresholds.
#'   - 1: Yes
#'   - 2: No (default)
#'
#' @return [integer] The hypertension status:
#'   - 1: Hypertension controlled (BP < 140/90 mmHg (or < 130/80 mmHg if diabetes or CKD) when on hypertension medication)
#'   - 2: Hypertension not controlled (BP >= 140/90 mmHg (or >= 130/80 mmHg if diabetes or CKD) when on hypertension medication)
#'   - `haven::tagged_na("a")`: Not applicable
#'   - `haven::tagged_na("b")`: Missing
#'
#' @details This function assesses whether a respondent's hypertension is controlled using adjusted BP values:
#'
#'          **Control Thresholds:**
#'          - General population: < 140/90 mmHg
#'          - Diabetes or CKD patients: < 130/80 mmHg
#'
#'          **Logic:**
#'          - Only applies to respondents taking hypertension medication.
#'          - If adjusted BP is below the threshold, hypertension is "controlled" (1).
#'          - If adjusted BP is at or above the threshold, it is "not controlled" (2).
#'
#'          **Missing Data Codes:**
#'          - `SBP_adj`, `DBP_adj`:
#'            - `996`: Valid skip. Handled as `haven::tagged_na("a")`.
#'            - `997-999`: Don't know, refusal, or not stated. Handled as `haven::tagged_na("b")`.
#'          - `ANYMED2`:
#'            - Tagged NA "a": Valid skip.
#'            - Tagged NA "b": Don't know, refusal, or not stated.
#'          - `CCC_32`, `CARDIOV`, `DIABX`, `CKD`:
#'            - `6`: Valid skip. Handled as `haven::tagged_na("a")`.
#'            - `7-9`: Don't know, refusal, or not stated. Handled as `haven::tagged_na("b")`.
#'
#' @examples
#' # Scalar usage: Single respondent
#' # Example 1: Respondent has adjusted SBP = 150, adjusted DBP = 95, and on medication.
#' determine_controlled_adjusted_hypertension(SBP_adj = 150, DBP_adj = 95, ANYMED2 = 1)
#' # Output: 2 (Hypertension not controlled due to high adjusted SBP and DBP despite medication usage).
#'
#' # Example 2: Respondent has adjusted SBP = 120, adjusted DBP = 80, and on medication.
#' determine_controlled_adjusted_hypertension(SBP_adj = 120, DBP_adj = 80, ANYMED2 = 1)
#' # Output: 1 (Hypertension controlled as adjusted BP is below 140/90 mmHg and on medication).
#'
#' # Example 3: Respondent has non-response BP values of 996 for both systolic and diastolic.
#' result <- determine_controlled_adjusted_hypertension(SBP_adj = 996, DBP_adj = 996, ANYMED2 = 0)
#' result # Shows: NA
#' haven::is_tagged_na(result, "a") # Shows: TRUE (confirms it's tagged NA(a))
#' format(result, tag = TRUE) # Shows: "NA(a)" (displays the tag)
#'
#' # Multiple respondents
#' determine_controlled_adjusted_hypertension(
#'   SBP_adj = c(150, 120, 135), DBP_adj = c(95, 80, 85),
#'   ANYMED2 = c(1, 1, 1), DIABX = c(2, 2, 1)
#' )
#' # Returns: c(2, 1, 2)
#'
#' @seealso [determine_controlled_hypertension()] for controlled status with unadjusted BP
#' @export
determine_controlled_adjusted_hypertension <- function(SBP_adj, DBP_adj, ANYMED2, CCC_32 = 2, CARDIOV = 2, DIABX = 2, CKD = 2) {
  ANYMED2 <- dplyr::case_when(
    CCC_32 == 2 & (CARDIOV == 1 | CKD == 1 | DIABX == 1) ~ 0,
    haven::is_tagged_na(ANYMED2, "a") ~ haven::tagged_na("a"),
    haven::is_tagged_na(ANYMED2, "b") ~ haven::tagged_na("b"),
    TRUE ~ as.numeric(ANYMED2)
  )
  
  highsys140_adj <- dplyr::case_when(
    SBP_adj == 996 ~ haven::tagged_na("a"),
    SBP_adj %in% 997:999 ~ haven::tagged_na("b"),
    is.na(SBP_adj) ~ haven::tagged_na("b"),
    
    (DIABX == 1 | CKD == 1) & !is.na(SBP_adj) & SBP_adj >= 130 ~ 1,
    (DIABX == 1 | CKD == 1) & !is.na(SBP_adj) & SBP_adj < 130 ~ 2,
    !(DIABX == 1 | CKD == 1) & !is.na(SBP_adj) & SBP_adj >= 140 ~ 1,
    !(DIABX == 1 | CKD == 1) & !is.na(SBP_adj) & SBP_adj < 140 ~ 2,
    .default = haven::tagged_na("b")
  )
  
  highdias90_adj <- dplyr::case_when(
    DBP_adj == 996 ~ haven::tagged_na("a"),
    DBP_adj %in% 997:999 ~ haven::tagged_na("b"),
    is.na(DBP_adj) ~ haven::tagged_na("b"),
    
    (DIABX == 1 | CKD == 1) & !is.na(DBP_adj) & DBP_adj >= 80 ~ 1,
    (DIABX == 1 | CKD == 1) & !is.na(DBP_adj) & DBP_adj < 80 ~ 2,
    !(DIABX == 1 | CKD == 1) & !is.na(DBP_adj) & DBP_adj >= 90 ~ 1,
    !(DIABX == 1 | CKD == 1) & !is.na(DBP_adj) & DBP_adj < 90 ~ 2,
    .default = haven::tagged_na("b")
  )
  
  dplyr::case_when(
    ANYMED2 == 1 & (highsys140_adj == 1 | highdias90_adj == 1) ~ 2,
    ANYMED2 == 1 & (highsys140_adj == 2 & highdias90_adj == 2) ~ 1,
    ANYMED2 == 0 ~ 2,
    
    haven::is_tagged_na(highsys140_adj, "a") |
      haven::is_tagged_na(highdias90_adj, "a") |
      haven::is_tagged_na(ANYMED2, "a") ~ haven::tagged_na("a"),
    
    haven::is_tagged_na(highsys140_adj, "b") |
      haven::is_tagged_na(highdias90_adj, "b") |
      haven::is_tagged_na(ANYMED2, "b") ~ haven::tagged_na("b"),
    
    .default = haven::tagged_na("b")
  )
}

#' @title Hypertension derived variable (130/80) with adjusted blood pressures
#'
#' @description
#' This function determines the hypertension status of a respondent based on their adjusted systolic and diastolic blood pressure measurements and medication usage.
#'
#' @param SBP_adj [integer] An integer representing the adjusted systolic blood pressure measurement of the respondent.
#' @param DBP_adj [integer] An integer representing the adjusted diastolic blood pressure measurement of the respondent.
#' @param ANYMED2 [integer] An integer indicating whether the respondent is on medication for hypertension.
#'   - 1: Yes
#'   - 0: No
#' @param CCC_32 [integer] An optional integer indicating whether the respondent is actually on medication for hypertension.
#'   - 1: Yes
#'   - 2: No (default)
#' @param CARDIOV [integer] An optional integer indicating the presence of cardiovascular disease, affecting medication status.
#'   - 1: Yes
#'   - 2: No (default)
#' @param DIABX [integer] An optional integer indicating the presence of diabetes, affecting blood pressure thresholds.
#'   - 1: Yes
#'   - 2: No (default)
#' @param CKD [integer] An optional integer indicating the presence of chronic kidney disease, affecting blood pressure thresholds.
#'   - 1: Yes
#'   - 2: No (default)
#'
#' @return [integer] The hypertension status:
#'   - 1: High blood pressure (adjusted BP ≥ 130/80 mmHg or on hypertension medication)
#'   - 2: Normal blood pressure (adjusted BP < 130/80 mmHg and not on hypertension medication)
#'   - `haven::tagged_na("a")`: Not applicable
#'   - `haven::tagged_na("b")`: Missing
#'
#' @details This function implements clinical guidelines for hypertension classification using adjusted blood pressure values:
#'
#'          **Blood Pressure Thresholds:**
#'          - General population: >= 130/80 mmHg indicates hypertension
#'
#'          **Medication Logic:**
#'          - Anyone taking hypertension medication is classified as hypertensive
#'          - Medication status may be adjusted based on comorbidities (diabetes, CKD, cardiovascular disease)
#'
#'          **Missing Data Codes:**
#'          - `SBP_adj`, `DBP_adj`:
#'            - `996`: Valid skip. Handled as `haven::tagged_na("a")`.
#'            - `997-999`: Don't know, refusal, or not stated. Handled as `haven::tagged_na("b")`.
#'          - `ANYMED2`:
#'            - Tagged NA "a": Valid skip.
#'            - Tagged NA "b": Don't know, refusal, or not stated.
#'          - `CCC_32`, `CARDIOV`, `DIABX`, `CKD`:
#'            - `6`: Valid skip. Handled as `haven::tagged_na("a")`.
#'            - `7-9`: Don't know, refusal, or not stated. Handled as `haven::tagged_na("b")`.
#'
#' @examples
#' # Scalar usage: Single respondent
#' # Example 1: Respondent has adjusted SBP = 150, adjusted DBP = 95, and on medication.
#' determine_adjusted_hypertension(SBP_adj = 150, DBP_adj = 95, ANYMED2 = 1)
#' # Output: 1 (High blood pressure due to adjusted SBP, adjusted DBP, and medication usage).
#'
#' # Example 2: Respondent has adjusted SBP = 120, adjusted DBP = 80, and not on medication.
#' determine_adjusted_hypertension(SBP_adj = 120, DBP_adj = 70, ANYMED2 = 0)
#' # Output: 2 (Normal blood pressure as adjusted BP is below 130/80 mmHg and not on medication).
#'
#' # Example 3: Respondent has non-response BP values of 996 for both systolic and diastolic.
#' result <- determine_adjusted_hypertension(SBP_adj = 996, DBP_adj = 996, ANYMED2 = 0)
#' result # Shows: NA
#' haven::is_tagged_na(result, "a") # Shows: TRUE (confirms it's tagged NA(a))
#' format(result, tag = TRUE) # Shows: "NA(a)" (displays the tag)
#'
#' # Multiple respondents
#' determine_adjusted_hypertension(
#'   SBP_adj = c(150, 120, 135), DBP_adj = c(95, 70, 85),
#'   ANYMED2 = c(1, 0, 1), DIABX = c(2, 2, 1)
#' )
#' # Returns: c(1, 2, 1)
#'
#' @seealso [determine_hypertension()] for unadjusted BP classification
#' @export
determine_adjusted_hypertension_new <- function(SBP_adj, DBP_adj, ANYMED2,
                                                CCC_32 = 2, CARDIOV = 2, DIABX = 2, CKD = 2) {
  ANYMED2 <- dplyr::case_when(
    CCC_32 == 2 & (CARDIOV == 1 | CKD == 1 | DIABX == 1) ~ 0,
    haven::is_tagged_na(ANYMED2, "a") ~ haven::tagged_na("a"),
    haven::is_tagged_na(ANYMED2, "b") ~ haven::tagged_na("b"),
    TRUE ~ as.numeric(ANYMED2)
  )
  
  highsys130 <- dplyr::case_when(
    SBP_adj == 996 ~ haven::tagged_na("a"),
    SBP_adj %in% 997:999 ~ haven::tagged_na("b"),
    SBP_adj >= 130 ~ 1,
    SBP_adj < 130 ~ 2,
    .default = haven::tagged_na("b")
  )
  
  highsys80 <- dplyr::case_when(
    DBP_adj == 996 ~ haven::tagged_na("a"),
    DBP_adj %in% 997:999 ~ haven::tagged_na("b"),
    DBP_adj >= 80 ~ 1,
    DBP_adj < 80 ~ 2,
    .default = haven::tagged_na("b")
  )
  
  dplyr::case_when(
    !is.na(ANYMED2) & ANYMED2 == 1 ~ 1,
    highsys130 == 1 | highsys80 == 1 ~ 1,
    highsys130 == 2 & highsys80 == 2 & (ANYMED2 == 0 | is.na(ANYMED2)) ~ 2,
    haven::is_tagged_na(highsys130, "a") | haven::is_tagged_na(highsys80, "a") ~ haven::tagged_na("a"),
    haven::is_tagged_na(highsys130, "b") | haven::is_tagged_na(highsys80, "b") |
      haven::is_tagged_na(ANYMED2, "b") ~ haven::tagged_na("b"),
    .default = haven::tagged_na("b")
  )
}
