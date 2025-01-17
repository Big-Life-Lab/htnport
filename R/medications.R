#' @title Calculate the number of occurrences of a specific drug class based on given conditions.
#' 
#' @description This function calculates the number of occurrences of a specific drug class in the data frame.
#' The calculation is based on custom conditions specified by the user.
#' 
#' @param df The data frame containing medication and last taken information.
#' @param class_var_name The name of the new variable representing the drug class.
#' @param med_vars A character vector containing the names of medication variables in the data frame.
#' @param last_taken_vars A character vector containing the names of last taken variables in the data frame.
#' @param class_condition_fun A custom condition function that determines whether a medication belongs to the drug class.
#'                            The function should accept two arguments: med_code (character) and last_taken (numeric).
#'                            It should return an integer, 1 if the medication belongs to the class, 0 otherwise.
#' @param log_level The log level for logging messages (default is "INFO").
#' @param overwrite Logical value indicating whether to overwrite the 'class_var_name' if it already exists in the data frame (default is FALSE).
#' 
#' @return The input data frame 'df' with an additional column representing the drug class.
#' 
#' @details The 'class_condition_fun' is applied to each pair of medication and last taken variables.
#'          The resulting values (0 or 1) are summed for each row, and the sum is stored in the new 'class_var_name' column.
#'          The function performs logging to provide information about the process and potential issues.
#'          If 'overwrite' is TRUE, the function will overwrite the existing 'class_var_name' column in the data frame.
#'          If 'overwrite' is FALSE and the variable already exists, the function will log an error and stop the execution.
#'          The function also checks if 'med_vars' and 'last_taken_vars' are present in the data frame and have the same length.
#'          If any issues are encountered, appropriate log messages are generated, and the function stops.
#'          
#' @export
is_taking_drug_class <- function(df, class_var_name, med_vars, last_taken_vars, class_condition_fun, log_level = "INFO", overwrite = FALSE) {
  # Validate input parameters
  if (!is.character(class_var_name) || class_var_name == "") {
    logger::log_fatal("The 'class_var_name' must be a non-empty character string.")
    stop()
  }
  
  if (!is.logical(overwrite)) {
    logger::log_fatal("'overwrite' must be a logical value (TRUE or FALSE).")
    stop()
  }
  
  if (!all(med_vars %in% names(df))) {
    missing_vars <- med_vars[!(med_vars %in% names(df))]
    error_msg <- paste0("The following medication variables are not in the data frame: ", paste(missing_vars, collapse = ", "))
    logger::log_error(error_msg)
    stop()
  }
  
  if (!all(last_taken_vars %in% names(df))) {
    missing_vars <- last_taken_vars[!(last_taken_vars %in% names(df))]
    error_msg <- paste0("The following 'last_taken' variables are not in the data frame: ", paste(missing_vars, collapse = ", "))
    logger::log_error(error_msg)
    stop()
  }
  
  if (length(med_vars) != length(last_taken_vars)) {
    error_msg <- "The lists of medication variables and 'last_taken' variables are not of the same length."
    logger::log_warn(error_msg)
    stop()
  }
  
  # Set the log level
  logger::log_threshold(log_level)
  
  # Check if class_var_name already exists in the data frame
  if (class_var_name %in% names(df)) {
    error_msg <- paste0("Variable '", class_var_name, "' already exists in the data frame.")
    if (overwrite) {
      logger::log_warn(paste0(error_msg, " The variable will be overwritten."))
    } else {
      logger::log_fatal(paste0(error_msg, " Use a new variable name or change 'overwrite=TRUE'."))
      stop()
    }
  }
  
  logger::log_info(paste0("Adding variable '", class_var_name, "' to the data frame."))
  
  # Initialize the class variable column
  df[[class_var_name]] <- 0 
  
  # Apply the condition function to each pair of med and last_taken vars using a loop
  for (i in seq_along(med_vars)) {
    med_values <- df[[med_vars[i]]]
    last_taken_values <- df[[last_taken_vars[i]]]
    class_values <- numeric(nrow(df))
    
    for (j in seq_along(med_values)) {
      class_values[j] <- class_condition_fun(med_values[j], last_taken_values[j])
    }
    
    df[[class_var_name]] <- df[[class_var_name]] + class_values
  }
  
  return(df)
}

#' @title Determine if a CHMS respondent's medication belongs to the beta blocker class.
#' 
#' @description This function determines whether a given medication, taken by a CHMS respondent,
#' is classified as a beta blocker. The identification is based on Anatomical Therapeutic Chemical (ATC) codes and the
#' timing of the last medication intake.
#' 
#' @param MEUCATC A character vector representing the Anatomical Therapeutic Chemical (ATC) code of the medication.
#' @param NPI_25B An integer representing the CHMS response for the time when the medication was last taken.
#'                        1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                        5 = More than a month ago, 6 = Never taken
#' 
#' @return A numeric, 1 if medication is in the beta blocker class and 0 if it is not.
#' 
#' @details This function identifies whether a medication is a beta blocker based on their ATC codes, which
#'          typically start with "C07". Additionally, specific sub-codes 'C07AA07', 'C07AA12', and 'C07AG02' are excluded 
#'          from the beta blocker class. A respondent is classified as taking a beta blocker (return = 1) if the ATC code matches the pattern and is not in the exclusion list, and the 
#'          medication was taken within the last month (NPI_25B <= 4), otherwise the respondent is not taking a beta blocker (return = 0)
#'
#' @examples 
#' 
#' # Example 1: Medication ATC code is "C07AA13", and it was taken within the last week
#' is_beta_blocker("C07AA13", 3) # Should return 1 (TRUE)
#'
#' # Example 2: Medication ATC code is "C07AA07" (excluded code), and it was taken within last month
#' is_beta_blocker("C07AA07", 4) # Should return 0 (FALSE)
#' 
#' @export
is_beta_blocker <- function(MEUCATC, NPI_25B) {
  
  if (is.na(MEUCATC) | is.na(NPI_25B)) {
    return(haven::tagged_na("b"))
  }
  
  starts_with_C07 <- startsWith(MEUCATC, "C07")
  not_in_specific_codes <- !(MEUCATC %in% c('C07AA07', 'C07AA12', 'C07AG02'))
  time_condition <- NPI_25B <= 4
  
  is_beta_blocker <- starts_with_C07 & not_in_specific_codes & time_condition
  return(as.numeric(is_beta_blocker))
}

#' @title Determine if a CHMS respondent's medication is an ACE inhibitor.
#' 
#' @description This function checks if a given medication for a CHMS respondent belongs to the ACE inhibitor drug class.
#' The identification is based on the Anatomical Therapeutic Chemical (ATC) code of the medication and the time when the
#' medication was last taken.
#' 
#' @param MEUCATC A character vector representing the Anatomical Therapeutic Chemical (ATC) code of the medication.
#' @param NPI_25B An integer representing the CHMS response for the time when the medication was last taken.
#'                        1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                        5 = More than a month ago, 6 = Never taken
#' 
#' @return A numeric, 1 if medication is in the ACE inhibitor class and 0 if it is not.
#' 
#' @details This function uses the `startsWith` function to identify ACE inhibitors based on their ATC codes, which
#'          typically start with "C09". If the ATC code matches the pattern and the medication was taken within the last
#'          month (NPI_25B <= 4), the medication is considered an ACE inhibitor and the function returns TRUE.
#'          Otherwise, it returns FALSE.
#' 
#' @examples
#' 
#' # Let's say the ATC code is "C09AB03" and the time last taken was yesterday (2). 
#' 
#' is_ace_inhibitor("C09AB03", 2) # Should return 1 (TRUE)
#' 
#' @export
is_ace_inhibitor <- function(MEUCATC, NPI_25B) {
  
  if (is.na(MEUCATC) | is.na(NPI_25B)) {
    return(haven::tagged_na("b"))
  }
  
  as.numeric(startsWith(MEUCATC, "C09") && NPI_25B <= 4)
}

#' @title Determine if a CHMS respondent's medication is a diuretic.
#' 
#' @description This function checks if a given medication for a CHMS respondent belongs to the diuretic drug class.
#' The identification is based on the Anatomical Therapeutic Chemical (ATC) code of the medication and the time when the
#' medication was last taken.
#' 
#' @param MEUCATC A character vector representing the Anatomical Therapeutic Chemical (ATC) code of the medication.
#' @param NPI_25B An integer representing the CHMS response for the time when the medication was last taken.
#'                        1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                        5 = More than a month ago, 6 = Never taken
#' 
#' @return A numeric, 1 if medication is in the diuretic class and 0 if it is not.
#' 
#' @details This function uses the `startsWith` function to identify diuretics based on their ATC codes, which
#'          typically start with "C03". Additionally, specific sub-codes 'C03BA08' and 'C03CA01' are excluded from the
#'          diuretic class. If the ATC code matches the pattern and is not in the exclusion list, and the medication was
#'          taken within the last month (NPI_25B <= 4), the medication is considered a diuretic, and the function
#'          returns TRUE. Otherwise, it returns FALSE.
#' 
#' @examples
#' 
#' # Let's say the ATC code is "C03AA03" and the time last taken was within last week (3). 
#' 
#' is_diuretic("C03AA03", 3) # Should return 1 (TRUE)
#' 
#' @export
is_diuretic <- function(MEUCATC, NPI_25B) {
  
  if (is.na(MEUCATC) | is.na(NPI_25B)) {
    return(haven::tagged_na("b"))
  }
  
  as.numeric(startsWith(MEUCATC, "C03") && !(MEUCATC %in% c('C03BA08', 'C03CA01')) && NPI_25B <= 4)
}

#' @title Determine if a CHMS respondent's medication is a calcium channel blocker.
#' 
#' @description This function checks if a given medication for a CHMS respondent belongs to the calcium channel blocker drug class.
#' The identification is based on the Anatomical Therapeutic Chemical (ATC) code of the medication and the time when the
#' medication was last taken.
#' 
#' @param MEUCATC A character vector representing the Anatomical Therapeutic Chemical (ATC) code of the medication.
#' @param NPI_25B An integer representing the CHMS response for the time when the medication was last taken.
#'                        1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                        5 = More than a month ago, 6 = Never taken
#' 
#' @return A numeric, 1 if medication is in the calcium channel blocker class and 0 if it is not.
#' 
#' @details This function uses the `startsWith` function to identify calcium channel blockers based on their ATC codes,
#'          which typically start with "C08". If the ATC code matches the pattern and the medication was taken within
#'          the last month (NPI_25B <= 4), the medication is considered a calcium channel blocker, and the
#'          function returns TRUE. Otherwise, it returns FALSE.
#' 
#' @examples
#' 
#' # Let's say the ATC code is "C08CA05" and the time last taken was today (1).
#' 
#' is_calcium_channel_blocker("C08CA05", 1) # Should return 1 (TRUE)
#' 
#' @export
is_calcium_channel_blocker <- function(MEUCATC, NPI_25B) {
  
  if (is.na(MEUCATC) | is.na(NPI_25B)) {
    return(haven::tagged_na("b"))
  }
  
  as.numeric(startsWith(MEUCATC, "C08") && NPI_25B <= 4)
}

#' @title Determine if a CHMS respondent's medication is another anti-hypertensive drug.
#' 
#' @description This function checks if a given medication for a CHMS respondent belongs to another anti-hypertensive drug class.
#' The identification is based on the Anatomical Therapeutic Chemical (ATC) code of the medication and the time when the
#' medication was last taken.
#' 
#' @param MEUCATC A character vector representing the Anatomical Therapeutic Chemical (ATC) code of the medication.
#' @param NPI_25B An integer representing the CHMS response for the time when the medication was last taken.
#'                        1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                        5 = More than a month ago, 6 = Never taken
#' 
#' @return A numeric, 1 if medication is in another anti-hypertensive drug class and 0 if it is not.
#' 
#' @details This function uses the `startsWith` function to identify other anti-hypertensive drugs based on their ATC
#'          codes, which typically start with "C02". The sub-code 'C02KX01' is excluded from the class. If the ATC code
#'          matches the pattern and is not in the exclusion list, and the medication was taken within the last month
#'          (NPI_25B <= 4), the medication is considered another anti-hypertensive drug, and the function returns
#'          TRUE. Otherwise, it returns FALSE.
#' 
#' @examples
#' 
#' # Let's say the ATC code is "C02AC04" and the time last taken was within last week (3).
#' 
#' is_other_antiHTN_med("C02AC04", 3) # Should return 1 (TRUE)
#' 
#' @export
is_other_antiHTN_med <- function(MEUCATC, NPI_25B) {
  
  if (is.na(MEUCATC) | is.na(NPI_25B)) {
    return(haven::tagged_na("b"))
  }
  
  as.numeric(startsWith(MEUCATC, "C02") && !(MEUCATC %in% c('C02KX01')) && NPI_25B <= 4)
}

#' @title Determine if a CHMS respondent's medication is any anti-hypertensive drug.
#' 
#' @description This function checks if a given medication for a CHMS respondent belongs to any anti-hypertensive drug class.
#' The identification is based on the Anatomical Therapeutic Chemical (ATC) code of the medication and the time when the
#' medication was last taken.
#' 
#' @param MEUCATC A character vector representing the Anatomical Therapeutic Chemical (ATC) code of the medication.
#' @param NPI_25B An integer representing the CHMS response for the time when the medication was last taken.
#'                        1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                        5 = More than a month ago, 6 = Never taken
#' 
#' @return A numeric, 1 if medication is in any anti-hypertensive drug class and 0 if it is not.
#' 
#' @details This function first identifies any anti-hypertensive drugs based on their ATC codes using the regular expression 
#'          "C02-3" and "CO7-9" which matches ATC codes that start with "C02", "C03", "C07", "C08", and "C09". Specific sub-codes 
#'          'C07AA07', 'C07AA12', 'C07AG02', 'C03BA08', 'C03CA01', and 'C02KX01' are excluded from the class. If the ATC 
#'          code matches the pattern and is not in the exclusion list, and the medication was taken within the last month 
#'          (NPI_25B <= 4), the medication is considered an anti-hypertensive drug, and the function returns TRUE. 
#'          Otherwise, it returns FALSE.
#' 
#' @examples
#' 
#' # Let's say the ATC code is "C07AB02" and the time last taken was within last month (4).
#' 
#' is_any_antiHTN_med("C07AB02", 4) # Should return 1 (TRUE)
#' 
#' @export
is_any_antiHTN_med <- function(MEUCATC, NPI_25B) {
  
  if (is.na(MEUCATC) | is.na(NPI_25B)) {
    return(haven::tagged_na("b"))
  }
  
  as.numeric(grepl('^C0[2, 3, 7, 8, 9]', MEUCATC) && !(MEUCATC %in% c('C07AA07', 'C07AA12', 'C07AG02', 'C03BA08', 'C03CA01', 'C02KX01')) && NPI_25B <= 4)
}

#' @title Determine if a CHMS respondent's medication is a non-steroidal anti-inflammatory drug (NSAID).
#' 
#' @description This function checks if a given medication for a CHMS respondent belongs to the non-steroidal anti-inflammatory drug
#' (NSAID) class. The identification is based on the Anatomical Therapeutic Chemical (ATC) code of the medication and the
#' time when the medication was last taken.
#' 
#' @param MEUCATC A character vector representing the Anatomical Therapeutic Chemical (ATC) code of the medication.
#' @param NPI_25B An integer representing the CHMS response for the time when the medication was last taken.
#'                        1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                        5 = More than a month ago, 6 = Never taken
#' 
#' @return A numeric, 1 if medication is in the NSAID class and 0 if it is not.
#' 
#' @details This function uses the `startsWith` function to identify NSAIDs based on their ATC codes, which typically
#'          start with "M01A". If the ATC code matches the pattern and the medication was taken within the last month
#'          (NPI_25B <= 4), the medication is considered an NSAID, and the function returns TRUE. Otherwise, it
#'          returns FALSE.
#' 
#' @examples
#' 
#' # Let's say the ATC code is "M01AB05" and the time last taken was today (1).
#' 
#' is_NSAID("M01AB05", 1) # Should return 1 (TRUE)
#' 
#' @export
is_NSAID <- function(MEUCATC, NPI_25B) {
  
  if (is.na(MEUCATC) | is.na(NPI_25B)) {
    return(haven::tagged_na("b"))
  }
  
  as.numeric(startsWith(MEUCATC, "M01A") && NPI_25B <= 4)
}

#' @title Determine if a CHMS respondent's medication is a diabetes drug.
#' 
#' @description This function checks if a given medication for a CHMS respondent belongs to the diabetes drug class.
#' The identification is based on the Anatomical Therapeutic Chemical (ATC) code of the medication and the time when the
#' medication was last taken.
#' 
#' @param MEUCATC A character vector representing the Anatomical Therapeutic Chemical (ATC) code of the medication.
#' @param NPI_25B An integer representing the CHMS response for the time when the medication was last taken.
#'                        1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                        5 = More than a month ago, 6 = Never taken
#' 
#' @return A numeric, 1 if medication is in the diabetes drug class and 0 if it is not.
#' 
#' @details This function uses the `startsWith` function to identify diabetes drugs based on their ATC codes, which
#'          typically start with "A10". If the ATC code matches the pattern and the medication was taken within the last
#'          month (NPI_25B <= 4), the medication is considered a diabetes drug, and the function returns TRUE.
#'          Otherwise, it returns FALSE.
#' 
#' @examples
#' 
#' # Let's say the ATC code is "A10BB09" and the time last taken was within last week (3).
#' 
#' is_diabetes_drug("A10BB09", 3) # Should return 1 (TRUE)
#' 
#' @export
is_diabetes_drug <- function(MEUCATC, NPI_25B) {
  
  if (is.na(MEUCATC) | is.na(NPI_25B)) {
    return(haven::tagged_na("b"))
  }
  
  as.numeric(startsWith(MEUCATC, "A10") && NPI_25B <= 4)
}

#' @title Determine if a person is taking beta-blockers in CHMS cycles 1 to 2
#' 
#' @description This function checks if a given medication for a CHMS respondent belongs to the beta-blocker drug class.
#' The specific conditions for identifying a beta-blocker are based on Anatomical Therapeutic Chemical (ATC) codes
#' and the time when the medication was last taken.
#'
#' @param atc_101a Character vector representing the ATC code of respondent's first prescription medication.
#' @param atc_102a Character vector representing the ATC code of respondent's second prescription medication.
#' @param atc_103a Character vector representing the ATC code of respondent's third  prescription medication.
#' @param atc_104a Character vector representing the ATC code of respondent's fourth prescription medication.
#' @param atc_105a Character vector representing the ATC code of respondent's fifth prescription medication.
#' @param atc_106a Character vector representing the ATC code of respondent's sixth prescription medication.
#' @param atc_107a Character vector representing the ATC code of respondent's seventh prescription medication.
#' @param atc_108a Character vector representing the ATC code of respondent's eighth prescription medication.
#' @param atc_109a Character vector representing the ATC code of respondent's ninth prescription medication.
#' @param atc_110a Character vector representing the ATC code of respondent's tenth prescription medication.
#' @param atc_111a Character vector representing the ATC code of respondent's eleventh prescription medication.
#' @param atc_112a Character vector representing the ATC code of respondent's twelfth prescription medication.
#' @param atc_113a Character vector representing the ATC code of respondent's thirteenth prescription medication.
#' @param atc_114a Character vector representing the ATC code of respondent's fourteenth prescription medication.
#' @param atc_115a Character vector representing the ATC code of respondent's fifteenth prescription medication.
#' @param atc_201a Character vector representing the ATC code of respondent's first over-the-counter medication.
#' @param atc_202a Character vector representing the ATC code of respondent's second over-the-counter medication.
#' @param atc_203a Character vector representing the ATC code of respondent's third over-the-counter medication.
#' @param atc_204a Character vector representing the ATC code of respondent's fourth over-the-counter medication.
#' @param atc_205a Character vector representing the ATC code of respondent's fifth over-the-counter medication.
#' @param atc_206a Character vector representing the ATC code of respondent's sixth over-the-counter medication.
#' @param atc_207a Character vector representing the ATC code of respondent's seventh over-the-counter medication.
#' @param atc_208a Character vector representing the ATC code of respondent's eighth over-the-counter medication.
#' @param atc_209a Character vector representing the ATC code of respondent's ninth over-the-counter medication.
#' @param atc_210a Character vector representing the ATC code of respondent's tenth over-the-counter medication.
#' @param atc_211a Character vector representing the ATC code of respondent's eleventh over-the-counter medication.
#' @param atc_212a Character vector representing the ATC code of respondent's twelfth over-the-counter medication.
#' @param atc_213a Character vector representing the ATC code of respondent's thirteenth over-the-counter medication.
#' @param atc_214a Character vector representing the ATC code of respondent's fourteenth over-the-counter medication.
#' @param atc_215a Character vector representing the ATC code of respondent's fifteenth over-the-counter medication.
#' @param atc_131a Character vector representing the ATC code of respondent's first new prescription medication.
#' @param atc_132a Character vector representing the ATC code of respondent's second new prescription medication.
#' @param atc_133a Character vector representing the ATC code of respondent's third new prescription medication.
#' @param atc_134a Character vector representing the ATC code of respondent's fourth new prescription medication.
#' @param atc_135a Character vector representing the ATC code of respondent's fifth new prescription medication.
#' @param atc_231a Character vector representing the ATC code of respondent's first new over-the-counter medication.
#' @param atc_232a Character vector representing the ATC code of respondent's second new over-the-counter medication.
#' @param atc_233a Character vector representing the ATC code of respondent's third new over-the-counter medication.
#' @param atc_234a Character vector representing the ATC code of respondent's fourth new over-the-counter medication.
#' @param atc_235a Character vector representing the ATC code of respondent's fifth new over-the-counter medication.
#' @param mhr_101b Integer representing the response for when the first prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_102b Integer representing the response for when the second prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_103b Integer representing the response for when the third prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_104b Integer representing the response for when the fourth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_105b Integer representing the response for when the fifth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_106b Integer representing the response for when the sixth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_107b Integer representing the response for when the seventh prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_108b Integer representing the response for when the eighth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_109b Integer representing the response for when the ninth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_110b Integer representing the response for when the tenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_111b Integer representing the response for when the eleventh prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_112b Integer representing the response for when the twelfth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_113b Integer representing the response for when the thirteenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_114b Integer representing the response for when the fourteenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_115b Integer representing the response for when the fifteenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_201b Integer representing the response for when the first over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_202b Integer representing the response for when the second over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_203b Integer representing the response for when the third over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_204b Integer representing the response for when the fourth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_205b Integer representing the response for when the fifth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_206b Integer representing the response for when the sixth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_207b Integer representing the response for when the seventh over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_208b Integer representing the response for when the eighth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_209b Integer representing the response for when the ninth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_210b Integer representing the response for when the tenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_211b Integer representing the response for when the eleventh over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_212b Integer representing the response for when the twelfth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_213b Integer representing the response for when the thirteenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_214b Integer representing the response for when the fourteenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_215b Integer representing the response for when the fifteenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_131b Integer representing the response for when the first new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_132b Integer representing the response for when the second new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_133b Integer representing the response for when the third new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_134b Integer representing the response for when the fourth new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_135b Integer representing the response for when the fifth new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_231b Integer representing the response for when the first new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_232b Integer representing the response for when the second new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_233b Integer representing the response for when the third new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_234b Integer representing the response for when the fourth new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_235b Integer representing the response for when the fifth new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' 
#' @return bbmed, a numeric set to 1 if the person is taking beta-blockers, NA if no information is available, 0 otherwise.
#'
#' @seealso `is_beta_blocker`
#'
#' @export
cycles1to2_beta_blockers <- function(
    atc_101a = NULL, atc_102a = NULL, atc_103a = NULL, atc_104a = NULL, atc_105a = NULL,
    atc_106a = NULL, atc_107a = NULL, atc_108a = NULL, atc_109a = NULL, atc_110a = NULL,
    atc_111a = NULL, atc_112a = NULL, atc_113a = NULL, atc_114a = NULL, atc_115a = NULL,
    atc_201a = NULL, atc_202a = NULL, atc_203a = NULL, atc_204a = NULL, atc_205a = NULL,
    atc_206a = NULL, atc_207a = NULL, atc_208a = NULL, atc_209a = NULL, atc_210a = NULL,
    atc_211a = NULL, atc_212a = NULL, atc_213a = NULL, atc_214a = NULL, atc_215a = NULL,
    atc_131a = NULL, atc_132a = NULL, atc_133a = NULL, atc_134a = NULL, atc_135a = NULL,
    atc_231a = NULL, atc_232a = NULL, atc_233a = NULL, atc_234a = NULL, atc_235a = NULL,
    mhr_101b = NULL, mhr_102b = NULL, mhr_103b = NULL, mhr_104b = NULL, mhr_105b = NULL,
    mhr_106b = NULL, mhr_107b = NULL, mhr_108b = NULL, mhr_109b = NULL, mhr_110b = NULL,
    mhr_111b = NULL, mhr_112b = NULL, mhr_113b = NULL, mhr_114b = NULL, mhr_115b = NULL,
    mhr_201b = NULL, mhr_202b = NULL, mhr_203b = NULL, mhr_204b = NULL, mhr_205b = NULL,
    mhr_206b = NULL, mhr_207b = NULL, mhr_208b = NULL, mhr_209b = NULL, mhr_210b = NULL,
    mhr_211b = NULL, mhr_212b = NULL, mhr_213b = NULL, mhr_214b = NULL, mhr_215b = NULL,
    mhr_131b = NULL, mhr_132b = NULL, mhr_133b = NULL, mhr_134b = NULL, mhr_135b = NULL,
    mhr_231b = NULL, mhr_232b = NULL, mhr_233b = NULL, mhr_234b = NULL, mhr_235b = NULL
) {
  # Identify variables for which a value was provided
  atc_vars <- as.data.frame(Filter(Negate(is.null), mget(ls(pattern = "^atc_"))))
  mhr_vars <- as.data.frame(Filter(Negate(is.null), mget(ls(pattern = "^mhr_"))))
  
  drugs <- cbind(atc_vars, mhr_vars)
  
  med_vars <- names(Filter(Negate(is.null), mget(ls(pattern = "^atc_.*a$"))))
  last_taken_vars <- names(Filter(Negate(is.null), mget(ls(pattern = "^mhr_.*b$"))))
  
  bb <- is_taking_drug_class(drugs, "BBmed", med_vars, last_taken_vars, is_beta_blocker, log_level="INFO", overwrite = TRUE)
  
  bbmed <- 0
  
  if (bb$BBmed > 0 %in% TRUE) {
    bbmed <- 1
  }
  else if (all(is.na(bb$BBmed))) {
    bbmed <- haven::tagged_na("b")
  }
  
  return(bbmed)
}

#' @title Determine if a person is taking ACE inhibitors in CHMS cycles 1 to 2.
#'
#' @description This function checks if a person is taking ACE inhibitors based on the provided Anatomical Therapeutic Chemical (ATC) codes for medications 
#' and the Canadian Health Measures Survey (CHMS) response for the time when the medication was last taken.
#'
#' @param atc_101a Character vector representing the ATC code of respondent's first prescription medication.
#' @param atc_102a Character vector representing the ATC code of respondent's second prescription medication.
#' @param atc_103a Character vector representing the ATC code of respondent's third  prescription medication.
#' @param atc_104a Character vector representing the ATC code of respondent's fourth prescription medication.
#' @param atc_105a Character vector representing the ATC code of respondent's fifth prescription medication.
#' @param atc_106a Character vector representing the ATC code of respondent's sixth prescription medication.
#' @param atc_107a Character vector representing the ATC code of respondent's seventh prescription medication.
#' @param atc_108a Character vector representing the ATC code of respondent's eighth prescription medication.
#' @param atc_109a Character vector representing the ATC code of respondent's ninth prescription medication.
#' @param atc_110a Character vector representing the ATC code of respondent's tenth prescription medication.
#' @param atc_111a Character vector representing the ATC code of respondent's eleventh prescription medication.
#' @param atc_112a Character vector representing the ATC code of respondent's twelfth prescription medication.
#' @param atc_113a Character vector representing the ATC code of respondent's thirteenth prescription medication.
#' @param atc_114a Character vector representing the ATC code of respondent's fourteenth prescription medication.
#' @param atc_115a Character vector representing the ATC code of respondent's fifteenth prescription medication.
#' @param atc_201a Character vector representing the ATC code of respondent's first over-the-counter medication.
#' @param atc_202a Character vector representing the ATC code of respondent's second over-the-counter medication.
#' @param atc_203a Character vector representing the ATC code of respondent's third over-the-counter medication.
#' @param atc_204a Character vector representing the ATC code of respondent's fourth over-the-counter medication.
#' @param atc_205a Character vector representing the ATC code of respondent's fifth over-the-counter medication.
#' @param atc_206a Character vector representing the ATC code of respondent's sixth over-the-counter medication.
#' @param atc_207a Character vector representing the ATC code of respondent's seventh over-the-counter medication.
#' @param atc_208a Character vector representing the ATC code of respondent's eighth over-the-counter medication.
#' @param atc_209a Character vector representing the ATC code of respondent's ninth over-the-counter medication.
#' @param atc_210a Character vector representing the ATC code of respondent's tenth over-the-counter medication.
#' @param atc_211a Character vector representing the ATC code of respondent's eleventh over-the-counter medication.
#' @param atc_212a Character vector representing the ATC code of respondent's twelfth over-the-counter medication.
#' @param atc_213a Character vector representing the ATC code of respondent's thirteenth over-the-counter medication.
#' @param atc_214a Character vector representing the ATC code of respondent's fourteenth over-the-counter medication.
#' @param atc_215a Character vector representing the ATC code of respondent's fifteenth over-the-counter medication.
#' @param atc_131a Character vector representing the ATC code of respondent's first new prescription medication.
#' @param atc_132a Character vector representing the ATC code of respondent's second new prescription medication.
#' @param atc_133a Character vector representing the ATC code of respondent's third new prescription medication.
#' @param atc_134a Character vector representing the ATC code of respondent's fourth new prescription medication.
#' @param atc_135a Character vector representing the ATC code of respondent's fifth new prescription medication.
#' @param atc_231a Character vector representing the ATC code of respondent's first new over-the-counter medication.
#' @param atc_232a Character vector representing the ATC code of respondent's second new over-the-counter medication.
#' @param atc_233a Character vector representing the ATC code of respondent's third new over-the-counter medication.
#' @param atc_234a Character vector representing the ATC code of respondent's fourth new over-the-counter medication.
#' @param atc_235a Character vector representing the ATC code of respondent's fifth new over-the-counter medication.
#' @param mhr_101b Integer representing the response for when the first prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_102b Integer representing the response for when the second prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_103b Integer representing the response for when the third prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_104b Integer representing the response for when the fourth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_105b Integer representing the response for when the fifth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_106b Integer representing the response for when the sixth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_107b Integer representing the response for when the seventh prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_108b Integer representing the response for when the eighth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_109b Integer representing the response for when the ninth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_110b Integer representing the response for when the tenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_111b Integer representing the response for when the eleventh prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_112b Integer representing the response for when the twelfth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_113b Integer representing the response for when the thirteenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_114b Integer representing the response for when the fourteenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_115b Integer representing the response for when the fifteenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_201b Integer representing the response for when the first over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_202b Integer representing the response for when the second over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_203b Integer representing the response for when the third over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_204b Integer representing the response for when the fourth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_205b Integer representing the response for when the fifth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_206b Integer representing the response for when the sixth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_207b Integer representing the response for when the seventh over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_208b Integer representing the response for when the eighth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_209b Integer representing the response for when the ninth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_210b Integer representing the response for when the tenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_211b Integer representing the response for when the eleventh over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_212b Integer representing the response for when the twelfth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_213b Integer representing the response for when the thirteenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_214b Integer representing the response for when the fourteenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_215b Integer representing the response for when the fifteenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_131b Integer representing the response for when the first new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_132b Integer representing the response for when the second new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_133b Integer representing the response for when the third new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_134b Integer representing the response for when the fourth new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_135b Integer representing the response for when the fifth new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_231b Integer representing the response for when the first new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_232b Integer representing the response for when the second new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_233b Integer representing the response for when the third new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_234b Integer representing the response for when the fourth new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_235b Integer representing the response for when the fifth new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#'
#' @return acemed, a numeric set to 1 if the person is taking ACE inhibitors, NA if no information is available, 0 otherwise.
#'
#' @seealso `is_ace_inhibitor`
#'
#' @export
cycles1to2_ace_inhibitors <- function(
    atc_101a = NULL, atc_102a = NULL, atc_103a = NULL, atc_104a = NULL, atc_105a = NULL,
    atc_106a = NULL, atc_107a = NULL, atc_108a = NULL, atc_109a = NULL, atc_110a = NULL,
    atc_111a = NULL, atc_112a = NULL, atc_113a = NULL, atc_114a = NULL, atc_115a = NULL,
    atc_201a = NULL, atc_202a = NULL, atc_203a = NULL, atc_204a = NULL, atc_205a = NULL,
    atc_206a = NULL, atc_207a = NULL, atc_208a = NULL, atc_209a = NULL, atc_210a = NULL,
    atc_211a = NULL, atc_212a = NULL, atc_213a = NULL, atc_214a = NULL, atc_215a = NULL,
    atc_131a = NULL, atc_132a = NULL, atc_133a = NULL, atc_134a = NULL, atc_135a = NULL,
    atc_231a = NULL, atc_232a = NULL, atc_233a = NULL, atc_234a = NULL, atc_235a = NULL,
    mhr_101b = NULL, mhr_102b = NULL, mhr_103b = NULL, mhr_104b = NULL, mhr_105b = NULL,
    mhr_106b = NULL, mhr_107b = NULL, mhr_108b = NULL, mhr_109b = NULL, mhr_110b = NULL,
    mhr_111b = NULL, mhr_112b = NULL, mhr_113b = NULL, mhr_114b = NULL, mhr_115b = NULL,
    mhr_201b = NULL, mhr_202b = NULL, mhr_203b = NULL, mhr_204b = NULL, mhr_205b = NULL,
    mhr_206b = NULL, mhr_207b = NULL, mhr_208b = NULL, mhr_209b = NULL, mhr_210b = NULL,
    mhr_211b = NULL, mhr_212b = NULL, mhr_213b = NULL, mhr_214b = NULL, mhr_215b = NULL,
    mhr_131b = NULL, mhr_132b = NULL, mhr_133b = NULL, mhr_134b = NULL, mhr_135b = NULL,
    mhr_231b = NULL, mhr_232b = NULL, mhr_233b = NULL, mhr_234b = NULL, mhr_235b = NULL
) {
  # Identify variables for which a value was provided
  atc_vars <- as.data.frame(Filter(Negate(is.null), mget(ls(pattern = "^atc_"))))
  mhr_vars <- as.data.frame(Filter(Negate(is.null), mget(ls(pattern = "^mhr_"))))
  
  drugs <- cbind(atc_vars, mhr_vars)
  
  med_vars <- names(Filter(Negate(is.null), mget(ls(pattern = "^atc_.*a$"))))
  last_taken_vars <- names(Filter(Negate(is.null), mget(ls(pattern = "^mhr_.*b$"))))
  
  ace <- is_taking_drug_class(drugs, "ACEmed", med_vars, last_taken_vars, is_ace_inhibitor, log_level="INFO", overwrite = TRUE)
  
  acemed <- 0
  
  if (ace$ACEmed > 0 %in% TRUE) {
    acemed <- 1
  }
  else if (all(is.na(ace$ACEmed))) {
    acemed <- haven::tagged_na("b")
  }
  
  return(acemed)
}

#' @title Determine if a person is taking diuretics in CHMS cycles 1 to 2.
#'
#' @description This function checks if a person is taking diuretics based on the provided Anatomical Therapeutic Chemical (ATC) codes for medications 
#' and the Canadian Health Measures Survey (CHMS) response for the time when the medication was last taken.
#'
#' @param atc_101a Character vector representing the ATC code of respondent's first prescription medication.
#' @param atc_102a Character vector representing the ATC code of respondent's second prescription medication.
#' @param atc_103a Character vector representing the ATC code of respondent's third  prescription medication.
#' @param atc_104a Character vector representing the ATC code of respondent's fourth prescription medication.
#' @param atc_105a Character vector representing the ATC code of respondent's fifth prescription medication.
#' @param atc_106a Character vector representing the ATC code of respondent's sixth prescription medication.
#' @param atc_107a Character vector representing the ATC code of respondent's seventh prescription medication.
#' @param atc_108a Character vector representing the ATC code of respondent's eighth prescription medication.
#' @param atc_109a Character vector representing the ATC code of respondent's ninth prescription medication.
#' @param atc_110a Character vector representing the ATC code of respondent's tenth prescription medication.
#' @param atc_111a Character vector representing the ATC code of respondent's eleventh prescription medication.
#' @param atc_112a Character vector representing the ATC code of respondent's twelfth prescription medication.
#' @param atc_113a Character vector representing the ATC code of respondent's thirteenth prescription medication.
#' @param atc_114a Character vector representing the ATC code of respondent's fourteenth prescription medication.
#' @param atc_115a Character vector representing the ATC code of respondent's fifteenth prescription medication.
#' @param atc_201a Character vector representing the ATC code of respondent's first over-the-counter medication.
#' @param atc_202a Character vector representing the ATC code of respondent's second over-the-counter medication.
#' @param atc_203a Character vector representing the ATC code of respondent's third over-the-counter medication.
#' @param atc_204a Character vector representing the ATC code of respondent's fourth over-the-counter medication.
#' @param atc_205a Character vector representing the ATC code of respondent's fifth over-the-counter medication.
#' @param atc_206a Character vector representing the ATC code of respondent's sixth over-the-counter medication.
#' @param atc_207a Character vector representing the ATC code of respondent's seventh over-the-counter medication.
#' @param atc_208a Character vector representing the ATC code of respondent's eighth over-the-counter medication.
#' @param atc_209a Character vector representing the ATC code of respondent's ninth over-the-counter medication.
#' @param atc_210a Character vector representing the ATC code of respondent's tenth over-the-counter medication.
#' @param atc_211a Character vector representing the ATC code of respondent's eleventh over-the-counter medication.
#' @param atc_212a Character vector representing the ATC code of respondent's twelfth over-the-counter medication.
#' @param atc_213a Character vector representing the ATC code of respondent's thirteenth over-the-counter medication.
#' @param atc_214a Character vector representing the ATC code of respondent's fourteenth over-the-counter medication.
#' @param atc_215a Character vector representing the ATC code of respondent's fifteenth over-the-counter medication.
#' @param atc_131a Character vector representing the ATC code of respondent's first new prescription medication.
#' @param atc_132a Character vector representing the ATC code of respondent's second new prescription medication.
#' @param atc_133a Character vector representing the ATC code of respondent's third new prescription medication.
#' @param atc_134a Character vector representing the ATC code of respondent's fourth new prescription medication.
#' @param atc_135a Character vector representing the ATC code of respondent's fifth new prescription medication.
#' @param atc_231a Character vector representing the ATC code of respondent's first new over-the-counter medication.
#' @param atc_232a Character vector representing the ATC code of respondent's second new over-the-counter medication.
#' @param atc_233a Character vector representing the ATC code of respondent's third new over-the-counter medication.
#' @param atc_234a Character vector representing the ATC code of respondent's fourth new over-the-counter medication.
#' @param atc_235a Character vector representing the ATC code of respondent's fifth new over-the-counter medication.
#' @param mhr_101b Integer representing the response for when the first prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_102b Integer representing the response for when the second prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_103b Integer representing the response for when the third prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_104b Integer representing the response for when the fourth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_105b Integer representing the response for when the fifth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_106b Integer representing the response for when the sixth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_107b Integer representing the response for when the seventh prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_108b Integer representing the response for when the eighth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_109b Integer representing the response for when the ninth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_110b Integer representing the response for when the tenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_111b Integer representing the response for when the eleventh prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_112b Integer representing the response for when the twelfth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_113b Integer representing the response for when the thirteenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_114b Integer representing the response for when the fourteenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_115b Integer representing the response for when the fifteenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_201b Integer representing the response for when the first over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_202b Integer representing the response for when the second over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_203b Integer representing the response for when the third over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_204b Integer representing the response for when the fourth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_205b Integer representing the response for when the fifth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_206b Integer representing the response for when the sixth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_207b Integer representing the response for when the seventh over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_208b Integer representing the response for when the eighth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_209b Integer representing the response for when the ninth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_210b Integer representing the response for when the tenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_211b Integer representing the response for when the eleventh over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_212b Integer representing the response for when the twelfth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_213b Integer representing the response for when the thirteenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_214b Integer representing the response for when the fourteenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_215b Integer representing the response for when the fifteenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_131b Integer representing the response for when the first new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_132b Integer representing the response for when the second new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_133b Integer representing the response for when the third new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_134b Integer representing the response for when the fourth new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_135b Integer representing the response for when the fifth new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_231b Integer representing the response for when the first new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_232b Integer representing the response for when the second new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_233b Integer representing the response for when the third new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_234b Integer representing the response for when the fourth new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_235b Integer representing the response for when the fifth new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#'
#' @return diurmed, a numeric set to 1 if the person is taking diuretics, NA if no information is available, 0 otherwise.
#'
#' @seealso `is_diuretic`
#'
#' @export
cycles1to2_diuretics <- function(
    atc_101a = NULL, atc_102a = NULL, atc_103a = NULL, atc_104a = NULL, atc_105a = NULL,
    atc_106a = NULL, atc_107a = NULL, atc_108a = NULL, atc_109a = NULL, atc_110a = NULL,
    atc_111a = NULL, atc_112a = NULL, atc_113a = NULL, atc_114a = NULL, atc_115a = NULL,
    atc_201a = NULL, atc_202a = NULL, atc_203a = NULL, atc_204a = NULL, atc_205a = NULL,
    atc_206a = NULL, atc_207a = NULL, atc_208a = NULL, atc_209a = NULL, atc_210a = NULL,
    atc_211a = NULL, atc_212a = NULL, atc_213a = NULL, atc_214a = NULL, atc_215a = NULL,
    atc_131a = NULL, atc_132a = NULL, atc_133a = NULL, atc_134a = NULL, atc_135a = NULL,
    atc_231a = NULL, atc_232a = NULL, atc_233a = NULL, atc_234a = NULL, atc_235a = NULL,
    mhr_101b = NULL, mhr_102b = NULL, mhr_103b = NULL, mhr_104b = NULL, mhr_105b = NULL,
    mhr_106b = NULL, mhr_107b = NULL, mhr_108b = NULL, mhr_109b = NULL, mhr_110b = NULL,
    mhr_111b = NULL, mhr_112b = NULL, mhr_113b = NULL, mhr_114b = NULL, mhr_115b = NULL,
    mhr_201b = NULL, mhr_202b = NULL, mhr_203b = NULL, mhr_204b = NULL, mhr_205b = NULL,
    mhr_206b = NULL, mhr_207b = NULL, mhr_208b = NULL, mhr_209b = NULL, mhr_210b = NULL,
    mhr_211b = NULL, mhr_212b = NULL, mhr_213b = NULL, mhr_214b = NULL, mhr_215b = NULL,
    mhr_131b = NULL, mhr_132b = NULL, mhr_133b = NULL, mhr_134b = NULL, mhr_135b = NULL,
    mhr_231b = NULL, mhr_232b = NULL, mhr_233b = NULL, mhr_234b = NULL, mhr_235b = NULL
) {
  # Identify variables for which a value was provided
  atc_vars <- as.data.frame(Filter(Negate(is.null), mget(ls(pattern = "^atc_"))))
  mhr_vars <- as.data.frame(Filter(Negate(is.null), mget(ls(pattern = "^mhr_"))))
  
  drugs <- cbind(atc_vars, mhr_vars)
  
  med_vars <- names(Filter(Negate(is.null), mget(ls(pattern = "^atc_.*a$"))))
  last_taken_vars <- names(Filter(Negate(is.null), mget(ls(pattern = "^mhr_.*b$"))))
  
  diur <- is_taking_drug_class(drugs, "DIURmed", med_vars, last_taken_vars, is_diuretic, log_level="INFO", overwrite = TRUE)
  
  diurmed <- 0
  
  if (diur$DIURmed > 0 %in% TRUE) {
    diurmed <- 1
  }
  else if (all(is.na(diur$DIURmed))) {
    diurmed <- haven::tagged_na("b")
  }
  
  return(diurmed)
}

#' @title Determine if a person is taking calcium channel blockers in CHMS cycles 1 to 2.
#'
#' @description This function checks if a person is taking calcium channel blockers based on the provided Anatomical Therapeutic Chemical (ATC) codes for medications 
#' and the Canadian Health Measures Survey (CHMS) response for the time when the medication was last taken.
#'
#' @param atc_101a Character vector representing the ATC code of respondent's first prescription medication.
#' @param atc_102a Character vector representing the ATC code of respondent's second prescription medication.
#' @param atc_103a Character vector representing the ATC code of respondent's third  prescription medication.
#' @param atc_104a Character vector representing the ATC code of respondent's fourth prescription medication.
#' @param atc_105a Character vector representing the ATC code of respondent's fifth prescription medication.
#' @param atc_106a Character vector representing the ATC code of respondent's sixth prescription medication.
#' @param atc_107a Character vector representing the ATC code of respondent's seventh prescription medication.
#' @param atc_108a Character vector representing the ATC code of respondent's eighth prescription medication.
#' @param atc_109a Character vector representing the ATC code of respondent's ninth prescription medication.
#' @param atc_110a Character vector representing the ATC code of respondent's tenth prescription medication.
#' @param atc_111a Character vector representing the ATC code of respondent's eleventh prescription medication.
#' @param atc_112a Character vector representing the ATC code of respondent's twelfth prescription medication.
#' @param atc_113a Character vector representing the ATC code of respondent's thirteenth prescription medication.
#' @param atc_114a Character vector representing the ATC code of respondent's fourteenth prescription medication.
#' @param atc_115a Character vector representing the ATC code of respondent's fifteenth prescription medication.
#' @param atc_201a Character vector representing the ATC code of respondent's first over-the-counter medication.
#' @param atc_202a Character vector representing the ATC code of respondent's second over-the-counter medication.
#' @param atc_203a Character vector representing the ATC code of respondent's third over-the-counter medication.
#' @param atc_204a Character vector representing the ATC code of respondent's fourth over-the-counter medication.
#' @param atc_205a Character vector representing the ATC code of respondent's fifth over-the-counter medication.
#' @param atc_206a Character vector representing the ATC code of respondent's sixth over-the-counter medication.
#' @param atc_207a Character vector representing the ATC code of respondent's seventh over-the-counter medication.
#' @param atc_208a Character vector representing the ATC code of respondent's eighth over-the-counter medication.
#' @param atc_209a Character vector representing the ATC code of respondent's ninth over-the-counter medication.
#' @param atc_210a Character vector representing the ATC code of respondent's tenth over-the-counter medication.
#' @param atc_211a Character vector representing the ATC code of respondent's eleventh over-the-counter medication.
#' @param atc_212a Character vector representing the ATC code of respondent's twelfth over-the-counter medication.
#' @param atc_213a Character vector representing the ATC code of respondent's thirteenth over-the-counter medication.
#' @param atc_214a Character vector representing the ATC code of respondent's fourteenth over-the-counter medication.
#' @param atc_215a Character vector representing the ATC code of respondent's fifteenth over-the-counter medication.
#' @param atc_131a Character vector representing the ATC code of respondent's first new prescription medication.
#' @param atc_132a Character vector representing the ATC code of respondent's second new prescription medication.
#' @param atc_133a Character vector representing the ATC code of respondent's third new prescription medication.
#' @param atc_134a Character vector representing the ATC code of respondent's fourth new prescription medication.
#' @param atc_135a Character vector representing the ATC code of respondent's fifth new prescription medication.
#' @param atc_231a Character vector representing the ATC code of respondent's first new over-the-counter medication.
#' @param atc_232a Character vector representing the ATC code of respondent's second new over-the-counter medication.
#' @param atc_233a Character vector representing the ATC code of respondent's third new over-the-counter medication.
#' @param atc_234a Character vector representing the ATC code of respondent's fourth new over-the-counter medication.
#' @param atc_235a Character vector representing the ATC code of respondent's fifth new over-the-counter medication.
#' @param mhr_101b Integer representing the response for when the first prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_102b Integer representing the response for when the second prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_103b Integer representing the response for when the third prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_104b Integer representing the response for when the fourth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_105b Integer representing the response for when the fifth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_106b Integer representing the response for when the sixth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_107b Integer representing the response for when the seventh prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_108b Integer representing the response for when the eighth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_109b Integer representing the response for when the ninth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_110b Integer representing the response for when the tenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_111b Integer representing the response for when the eleventh prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_112b Integer representing the response for when the twelfth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_113b Integer representing the response for when the thirteenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_114b Integer representing the response for when the fourteenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_115b Integer representing the response for when the fifteenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_201b Integer representing the response for when the first over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_202b Integer representing the response for when the second over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_203b Integer representing the response for when the third over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_204b Integer representing the response for when the fourth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_205b Integer representing the response for when the fifth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_206b Integer representing the response for when the sixth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_207b Integer representing the response for when the seventh over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_208b Integer representing the response for when the eighth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_209b Integer representing the response for when the ninth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_210b Integer representing the response for when the tenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_211b Integer representing the response for when the eleventh over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_212b Integer representing the response for when the twelfth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_213b Integer representing the response for when the thirteenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_214b Integer representing the response for when the fourteenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_215b Integer representing the response for when the fifteenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_131b Integer representing the response for when the first new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_132b Integer representing the response for when the second new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_133b Integer representing the response for when the third new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_134b Integer representing the response for when the fourth new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_135b Integer representing the response for when the fifth new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_231b Integer representing the response for when the first new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_232b Integer representing the response for when the second new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_233b Integer representing the response for when the third new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_234b Integer representing the response for when the fourth new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_235b Integer representing the response for when the fifth new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#'
#' @return ccbmed, a numeric set to 1 if the person is taking calcium channel blockers, NA if no information is available, 0 otherwise.
#'
#' @seealso `is_calcium_channel_blocker`
#'
#' @export
cycles1to2_calcium_channel_blockers <- function(
    atc_101a = NULL, atc_102a = NULL, atc_103a = NULL, atc_104a = NULL, atc_105a = NULL,
    atc_106a = NULL, atc_107a = NULL, atc_108a = NULL, atc_109a = NULL, atc_110a = NULL,
    atc_111a = NULL, atc_112a = NULL, atc_113a = NULL, atc_114a = NULL, atc_115a = NULL,
    atc_201a = NULL, atc_202a = NULL, atc_203a = NULL, atc_204a = NULL, atc_205a = NULL,
    atc_206a = NULL, atc_207a = NULL, atc_208a = NULL, atc_209a = NULL, atc_210a = NULL,
    atc_211a = NULL, atc_212a = NULL, atc_213a = NULL, atc_214a = NULL, atc_215a = NULL,
    atc_131a = NULL, atc_132a = NULL, atc_133a = NULL, atc_134a = NULL, atc_135a = NULL,
    atc_231a = NULL, atc_232a = NULL, atc_233a = NULL, atc_234a = NULL, atc_235a = NULL,
    mhr_101b = NULL, mhr_102b = NULL, mhr_103b = NULL, mhr_104b = NULL, mhr_105b = NULL,
    mhr_106b = NULL, mhr_107b = NULL, mhr_108b = NULL, mhr_109b = NULL, mhr_110b = NULL,
    mhr_111b = NULL, mhr_112b = NULL, mhr_113b = NULL, mhr_114b = NULL, mhr_115b = NULL,
    mhr_201b = NULL, mhr_202b = NULL, mhr_203b = NULL, mhr_204b = NULL, mhr_205b = NULL,
    mhr_206b = NULL, mhr_207b = NULL, mhr_208b = NULL, mhr_209b = NULL, mhr_210b = NULL,
    mhr_211b = NULL, mhr_212b = NULL, mhr_213b = NULL, mhr_214b = NULL, mhr_215b = NULL,
    mhr_131b = NULL, mhr_132b = NULL, mhr_133b = NULL, mhr_134b = NULL, mhr_135b = NULL,
    mhr_231b = NULL, mhr_232b = NULL, mhr_233b = NULL, mhr_234b = NULL, mhr_235b = NULL
) {
  # Identify variables for which a value was provided
  atc_vars <- as.data.frame(Filter(Negate(is.null), mget(ls(pattern = "^atc_"))))
  mhr_vars <- as.data.frame(Filter(Negate(is.null), mget(ls(pattern = "^mhr_"))))
  
  drugs <- cbind(atc_vars, mhr_vars)
  
  med_vars <- names(Filter(Negate(is.null), mget(ls(pattern = "^atc_.*a$"))))
  last_taken_vars <- names(Filter(Negate(is.null), mget(ls(pattern = "^mhr_.*b$"))))
  
  ccb <- is_taking_drug_class(drugs, "CCBmed", med_vars, last_taken_vars, is_calcium_channel_blocker, log_level="INFO", overwrite = TRUE)
  
  ccbmed <- 0
  
  if (ccb$CCBmed > 0 %in% TRUE) {
    ccbmed <- 1
    return(ccbmed)
  }
  else if (all(is.na(ccb$CCBmed))) {
    ccbmed <- haven::tagged_na("b")
  }
  
  return(ccbmed)
  
}

#' @title Determine if a person is taking another type of anti-hypertensive medication in CHMS cycles 1 to 2.
#'
#' @description This function checks if a person is taking another type of anti-hypertensive medication based on the provided Anatomical Therapeutic Chemical (ATC) codes for medications 
#' and the Canadian Health Measures Survey (CHMS) response for the time when the medication was last taken.
#'
#' @param atc_101a Character vector representing the ATC code of respondent's first prescription medication.
#' @param atc_102a Character vector representing the ATC code of respondent's second prescription medication.
#' @param atc_103a Character vector representing the ATC code of respondent's third  prescription medication.
#' @param atc_104a Character vector representing the ATC code of respondent's fourth prescription medication.
#' @param atc_105a Character vector representing the ATC code of respondent's fifth prescription medication.
#' @param atc_106a Character vector representing the ATC code of respondent's sixth prescription medication.
#' @param atc_107a Character vector representing the ATC code of respondent's seventh prescription medication.
#' @param atc_108a Character vector representing the ATC code of respondent's eighth prescription medication.
#' @param atc_109a Character vector representing the ATC code of respondent's ninth prescription medication.
#' @param atc_110a Character vector representing the ATC code of respondent's tenth prescription medication.
#' @param atc_111a Character vector representing the ATC code of respondent's eleventh prescription medication.
#' @param atc_112a Character vector representing the ATC code of respondent's twelfth prescription medication.
#' @param atc_113a Character vector representing the ATC code of respondent's thirteenth prescription medication.
#' @param atc_114a Character vector representing the ATC code of respondent's fourteenth prescription medication.
#' @param atc_115a Character vector representing the ATC code of respondent's fifteenth prescription medication.
#' @param atc_201a Character vector representing the ATC code of respondent's first over-the-counter medication.
#' @param atc_202a Character vector representing the ATC code of respondent's second over-the-counter medication.
#' @param atc_203a Character vector representing the ATC code of respondent's third over-the-counter medication.
#' @param atc_204a Character vector representing the ATC code of respondent's fourth over-the-counter medication.
#' @param atc_205a Character vector representing the ATC code of respondent's fifth over-the-counter medication.
#' @param atc_206a Character vector representing the ATC code of respondent's sixth over-the-counter medication.
#' @param atc_207a Character vector representing the ATC code of respondent's seventh over-the-counter medication.
#' @param atc_208a Character vector representing the ATC code of respondent's eighth over-the-counter medication.
#' @param atc_209a Character vector representing the ATC code of respondent's ninth over-the-counter medication.
#' @param atc_210a Character vector representing the ATC code of respondent's tenth over-the-counter medication.
#' @param atc_211a Character vector representing the ATC code of respondent's eleventh over-the-counter medication.
#' @param atc_212a Character vector representing the ATC code of respondent's twelfth over-the-counter medication.
#' @param atc_213a Character vector representing the ATC code of respondent's thirteenth over-the-counter medication.
#' @param atc_214a Character vector representing the ATC code of respondent's fourteenth over-the-counter medication.
#' @param atc_215a Character vector representing the ATC code of respondent's fifteenth over-the-counter medication.
#' @param atc_131a Character vector representing the ATC code of respondent's first new prescription medication.
#' @param atc_132a Character vector representing the ATC code of respondent's second new prescription medication.
#' @param atc_133a Character vector representing the ATC code of respondent's third new prescription medication.
#' @param atc_134a Character vector representing the ATC code of respondent's fourth new prescription medication.
#' @param atc_135a Character vector representing the ATC code of respondent's fifth new prescription medication.
#' @param atc_231a Character vector representing the ATC code of respondent's first new over-the-counter medication.
#' @param atc_232a Character vector representing the ATC code of respondent's second new over-the-counter medication.
#' @param atc_233a Character vector representing the ATC code of respondent's third new over-the-counter medication.
#' @param atc_234a Character vector representing the ATC code of respondent's fourth new over-the-counter medication.
#' @param atc_235a Character vector representing the ATC code of respondent's fifth new over-the-counter medication.
#' @param mhr_101b Integer representing the response for when the first prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_102b Integer representing the response for when the second prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_103b Integer representing the response for when the third prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_104b Integer representing the response for when the fourth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_105b Integer representing the response for when the fifth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_106b Integer representing the response for when the sixth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_107b Integer representing the response for when the seventh prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_108b Integer representing the response for when the eighth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_109b Integer representing the response for when the ninth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_110b Integer representing the response for when the tenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_111b Integer representing the response for when the eleventh prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_112b Integer representing the response for when the twelfth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_113b Integer representing the response for when the thirteenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_114b Integer representing the response for when the fourteenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_115b Integer representing the response for when the fifteenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_201b Integer representing the response for when the first over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_202b Integer representing the response for when the second over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_203b Integer representing the response for when the third over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_204b Integer representing the response for when the fourth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_205b Integer representing the response for when the fifth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_206b Integer representing the response for when the sixth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_207b Integer representing the response for when the seventh over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_208b Integer representing the response for when the eighth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_209b Integer representing the response for when the ninth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_210b Integer representing the response for when the tenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_211b Integer representing the response for when the eleventh over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_212b Integer representing the response for when the twelfth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_213b Integer representing the response for when the thirteenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_214b Integer representing the response for when the fourteenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_215b Integer representing the response for when the fifteenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_131b Integer representing the response for when the first new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_132b Integer representing the response for when the second new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_133b Integer representing the response for when the third new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_134b Integer representing the response for when the fourth new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_135b Integer representing the response for when the fifth new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_231b Integer representing the response for when the first new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_232b Integer representing the response for when the second new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_233b Integer representing the response for when the third new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_234b Integer representing the response for when the fourth new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_235b Integer representing the response for when the fifth new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#'
#' @return miscmed, a numeric set to 1 if the person is taking another type of anti-hypertensive medication, NA if no information is available, 0 otherwise.
#'
#' @seealso `is_other_antiHTN_med`
#'
#' @export
cycles1to2_other_antiHTN_meds <- function(
    atc_101a = NULL, atc_102a = NULL, atc_103a = NULL, atc_104a = NULL, atc_105a = NULL,
    atc_106a = NULL, atc_107a = NULL, atc_108a = NULL, atc_109a = NULL, atc_110a = NULL,
    atc_111a = NULL, atc_112a = NULL, atc_113a = NULL, atc_114a = NULL, atc_115a = NULL,
    atc_201a = NULL, atc_202a = NULL, atc_203a = NULL, atc_204a = NULL, atc_205a = NULL,
    atc_206a = NULL, atc_207a = NULL, atc_208a = NULL, atc_209a = NULL, atc_210a = NULL,
    atc_211a = NULL, atc_212a = NULL, atc_213a = NULL, atc_214a = NULL, atc_215a = NULL,
    atc_131a = NULL, atc_132a = NULL, atc_133a = NULL, atc_134a = NULL, atc_135a = NULL,
    atc_231a = NULL, atc_232a = NULL, atc_233a = NULL, atc_234a = NULL, atc_235a = NULL,
    mhr_101b = NULL, mhr_102b = NULL, mhr_103b = NULL, mhr_104b = NULL, mhr_105b = NULL,
    mhr_106b = NULL, mhr_107b = NULL, mhr_108b = NULL, mhr_109b = NULL, mhr_110b = NULL,
    mhr_111b = NULL, mhr_112b = NULL, mhr_113b = NULL, mhr_114b = NULL, mhr_115b = NULL,
    mhr_201b = NULL, mhr_202b = NULL, mhr_203b = NULL, mhr_204b = NULL, mhr_205b = NULL,
    mhr_206b = NULL, mhr_207b = NULL, mhr_208b = NULL, mhr_209b = NULL, mhr_210b = NULL,
    mhr_211b = NULL, mhr_212b = NULL, mhr_213b = NULL, mhr_214b = NULL, mhr_215b = NULL,
    mhr_131b = NULL, mhr_132b = NULL, mhr_133b = NULL, mhr_134b = NULL, mhr_135b = NULL,
    mhr_231b = NULL, mhr_232b = NULL, mhr_233b = NULL, mhr_234b = NULL, mhr_235b = NULL
) {
  # Identify variables for which a value was provided
  atc_vars <- as.data.frame(Filter(Negate(is.null), mget(ls(pattern = "^atc_"))))
  mhr_vars <- as.data.frame(Filter(Negate(is.null), mget(ls(pattern = "^mhr_"))))
  
  drugs <- cbind(atc_vars, mhr_vars)
  
  med_vars <- names(Filter(Negate(is.null), mget(ls(pattern = "^atc_.*a$"))))
  last_taken_vars <- names(Filter(Negate(is.null), mget(ls(pattern = "^mhr_.*b$"))))
  
  misc <- is_taking_drug_class(drugs, "MISCmed", med_vars, last_taken_vars, is_other_antiHTN_med, log_level="INFO", overwrite = TRUE)
  
  miscmed <- 0
  
  if (misc$MISCmed > 0 %in% TRUE) {
    miscmed <- 1
  }
  else if (all(is.na(misc$MISCmed))) {
    miscmed <- haven::tagged_na("b")
  }
  
  return(miscmed)
  
}

#' @title Determine if a person is taking any anti-hypertensive medication in CHMS cycles 1 to 2.
#'
#' @description This function checks if a person is taking any anti-hypertensive medication based on the provided Anatomical Therapeutic Chemical (ATC) codes for medications 
#' and the Canadian Health Measures Survey (CHMS) response for the time when the medication was last taken.
#'
#' @param atc_101a Character vector representing the ATC code of respondent's first prescription medication.
#' @param atc_102a Character vector representing the ATC code of respondent's second prescription medication.
#' @param atc_103a Character vector representing the ATC code of respondent's third  prescription medication.
#' @param atc_104a Character vector representing the ATC code of respondent's fourth prescription medication.
#' @param atc_105a Character vector representing the ATC code of respondent's fifth prescription medication.
#' @param atc_106a Character vector representing the ATC code of respondent's sixth prescription medication.
#' @param atc_107a Character vector representing the ATC code of respondent's seventh prescription medication.
#' @param atc_108a Character vector representing the ATC code of respondent's eighth prescription medication.
#' @param atc_109a Character vector representing the ATC code of respondent's ninth prescription medication.
#' @param atc_110a Character vector representing the ATC code of respondent's tenth prescription medication.
#' @param atc_111a Character vector representing the ATC code of respondent's eleventh prescription medication.
#' @param atc_112a Character vector representing the ATC code of respondent's twelfth prescription medication.
#' @param atc_113a Character vector representing the ATC code of respondent's thirteenth prescription medication.
#' @param atc_114a Character vector representing the ATC code of respondent's fourteenth prescription medication.
#' @param atc_115a Character vector representing the ATC code of respondent's fifteenth prescription medication.
#' @param atc_201a Character vector representing the ATC code of respondent's first over-the-counter medication.
#' @param atc_202a Character vector representing the ATC code of respondent's second over-the-counter medication.
#' @param atc_203a Character vector representing the ATC code of respondent's third over-the-counter medication.
#' @param atc_204a Character vector representing the ATC code of respondent's fourth over-the-counter medication.
#' @param atc_205a Character vector representing the ATC code of respondent's fifth over-the-counter medication.
#' @param atc_206a Character vector representing the ATC code of respondent's sixth over-the-counter medication.
#' @param atc_207a Character vector representing the ATC code of respondent's seventh over-the-counter medication.
#' @param atc_208a Character vector representing the ATC code of respondent's eighth over-the-counter medication.
#' @param atc_209a Character vector representing the ATC code of respondent's ninth over-the-counter medication.
#' @param atc_210a Character vector representing the ATC code of respondent's tenth over-the-counter medication.
#' @param atc_211a Character vector representing the ATC code of respondent's eleventh over-the-counter medication.
#' @param atc_212a Character vector representing the ATC code of respondent's twelfth over-the-counter medication.
#' @param atc_213a Character vector representing the ATC code of respondent's thirteenth over-the-counter medication.
#' @param atc_214a Character vector representing the ATC code of respondent's fourteenth over-the-counter medication.
#' @param atc_215a Character vector representing the ATC code of respondent's fifteenth over-the-counter medication.
#' @param atc_131a Character vector representing the ATC code of respondent's first new prescription medication.
#' @param atc_132a Character vector representing the ATC code of respondent's second new prescription medication.
#' @param atc_133a Character vector representing the ATC code of respondent's third new prescription medication.
#' @param atc_134a Character vector representing the ATC code of respondent's fourth new prescription medication.
#' @param atc_135a Character vector representing the ATC code of respondent's fifth new prescription medication.
#' @param atc_231a Character vector representing the ATC code of respondent's first new over-the-counter medication.
#' @param atc_232a Character vector representing the ATC code of respondent's second new over-the-counter medication.
#' @param atc_233a Character vector representing the ATC code of respondent's third new over-the-counter medication.
#' @param atc_234a Character vector representing the ATC code of respondent's fourth new over-the-counter medication.
#' @param atc_235a Character vector representing the ATC code of respondent's fifth new over-the-counter medication.
#' @param mhr_101b Integer representing the response for when the first prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_102b Integer representing the response for when the second prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_103b Integer representing the response for when the third prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_104b Integer representing the response for when the fourth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_105b Integer representing the response for when the fifth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_106b Integer representing the response for when the sixth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_107b Integer representing the response for when the seventh prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_108b Integer representing the response for when the eighth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_109b Integer representing the response for when the ninth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_110b Integer representing the response for when the tenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_111b Integer representing the response for when the eleventh prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_112b Integer representing the response for when the twelfth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_113b Integer representing the response for when the thirteenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_114b Integer representing the response for when the fourteenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_115b Integer representing the response for when the fifteenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_201b Integer representing the response for when the first over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_202b Integer representing the response for when the second over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_203b Integer representing the response for when the third over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_204b Integer representing the response for when the fourth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_205b Integer representing the response for when the fifth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_206b Integer representing the response for when the sixth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_207b Integer representing the response for when the seventh over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_208b Integer representing the response for when the eighth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_209b Integer representing the response for when the ninth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_210b Integer representing the response for when the tenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_211b Integer representing the response for when the eleventh over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_212b Integer representing the response for when the twelfth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_213b Integer representing the response for when the thirteenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_214b Integer representing the response for when the fourteenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_215b Integer representing the response for when the fifteenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_131b Integer representing the response for when the first new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_132b Integer representing the response for when the second new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_133b Integer representing the response for when the third new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_134b Integer representing the response for when the fourth new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_135b Integer representing the response for when the fifth new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_231b Integer representing the response for when the first new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_232b Integer representing the response for when the second new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_233b Integer representing the response for when the third new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_234b Integer representing the response for when the fourth new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_235b Integer representing the response for when the fifth new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#'
#' @return anymed, a numeric set to 1 if the person is taking any anti-hypertensive medication, NA if no information is available, 0 otherwise.
#'
#' @seealso `is_any_antiHTN_med`
#'
#' @export
cycles1to2_any_antiHTN_meds <- function(
    atc_101a = NULL, atc_102a = NULL, atc_103a = NULL, atc_104a = NULL, atc_105a = NULL,
    atc_106a = NULL, atc_107a = NULL, atc_108a = NULL, atc_109a = NULL, atc_110a = NULL,
    atc_111a = NULL, atc_112a = NULL, atc_113a = NULL, atc_114a = NULL, atc_115a = NULL,
    atc_201a = NULL, atc_202a = NULL, atc_203a = NULL, atc_204a = NULL, atc_205a = NULL,
    atc_206a = NULL, atc_207a = NULL, atc_208a = NULL, atc_209a = NULL, atc_210a = NULL,
    atc_211a = NULL, atc_212a = NULL, atc_213a = NULL, atc_214a = NULL, atc_215a = NULL,
    atc_131a = NULL, atc_132a = NULL, atc_133a = NULL, atc_134a = NULL, atc_135a = NULL,
    atc_231a = NULL, atc_232a = NULL, atc_233a = NULL, atc_234a = NULL, atc_235a = NULL,
    mhr_101b = NULL, mhr_102b = NULL, mhr_103b = NULL, mhr_104b = NULL, mhr_105b = NULL,
    mhr_106b = NULL, mhr_107b = NULL, mhr_108b = NULL, mhr_109b = NULL, mhr_110b = NULL,
    mhr_111b = NULL, mhr_112b = NULL, mhr_113b = NULL, mhr_114b = NULL, mhr_115b = NULL,
    mhr_201b = NULL, mhr_202b = NULL, mhr_203b = NULL, mhr_204b = NULL, mhr_205b = NULL,
    mhr_206b = NULL, mhr_207b = NULL, mhr_208b = NULL, mhr_209b = NULL, mhr_210b = NULL,
    mhr_211b = NULL, mhr_212b = NULL, mhr_213b = NULL, mhr_214b = NULL, mhr_215b = NULL,
    mhr_131b = NULL, mhr_132b = NULL, mhr_133b = NULL, mhr_134b = NULL, mhr_135b = NULL,
    mhr_231b = NULL, mhr_232b = NULL, mhr_233b = NULL, mhr_234b = NULL, mhr_235b = NULL
) {
  # Identify variables for which a value was provided
  atc_vars <- as.data.frame(Filter(Negate(is.null), mget(ls(pattern = "^atc_"))))
  mhr_vars <- as.data.frame(Filter(Negate(is.null), mget(ls(pattern = "^mhr_"))))
  
  drugs <- cbind(atc_vars, mhr_vars)
  
  med_vars <- names(Filter(Negate(is.null), mget(ls(pattern = "^atc_.*a$"))))
  last_taken_vars <- names(Filter(Negate(is.null), mget(ls(pattern = "^mhr_.*b$"))))
  
  anyHTN <- is_taking_drug_class(drugs, "ANYmed", med_vars, last_taken_vars, is_any_antiHTN_med, log_level="INFO", overwrite = TRUE)
  
  anymed <- 0
  
  if (anyHTN$ANYmed > 0 %in% TRUE) {
    anymed <- 1
  }
  else if (all(is.na(anyHTN$ANYmed))) {
    anymed <- haven::tagged_na("b")
  }
  
  return(anymed)
}

#' @title Determine if a person is taking non-steroidal anti-inflammatory drugs (NSAID) in CHMS cycles 1 to 2.
#' 
#' @description This function checks if a person is taking any NSAIDs based on the provided Anatomical Therapeutic Chemical (ATC) codes for medications 
#' and the Canadian Health Measures Survey (CHMS) response for the time when the medication was last taken.
#' 
#' @param atc_101a Character vector representing the ATC code of respondent's first prescription medication.
#' @param atc_102a Character vector representing the ATC code of respondent's second prescription medication.
#' @param atc_103a Character vector representing the ATC code of respondent's third  prescription medication.
#' @param atc_104a Character vector representing the ATC code of respondent's fourth prescription medication.
#' @param atc_105a Character vector representing the ATC code of respondent's fifth prescription medication.
#' @param atc_106a Character vector representing the ATC code of respondent's sixth prescription medication.
#' @param atc_107a Character vector representing the ATC code of respondent's seventh prescription medication.
#' @param atc_108a Character vector representing the ATC code of respondent's eighth prescription medication.
#' @param atc_109a Character vector representing the ATC code of respondent's ninth prescription medication.
#' @param atc_110a Character vector representing the ATC code of respondent's tenth prescription medication.
#' @param atc_111a Character vector representing the ATC code of respondent's eleventh prescription medication.
#' @param atc_112a Character vector representing the ATC code of respondent's twelfth prescription medication.
#' @param atc_113a Character vector representing the ATC code of respondent's thirteenth prescription medication.
#' @param atc_114a Character vector representing the ATC code of respondent's fourteenth prescription medication.
#' @param atc_115a Character vector representing the ATC code of respondent's fifteenth prescription medication.
#' @param atc_201a Character vector representing the ATC code of respondent's first over-the-counter medication.
#' @param atc_202a Character vector representing the ATC code of respondent's second over-the-counter medication.
#' @param atc_203a Character vector representing the ATC code of respondent's third over-the-counter medication.
#' @param atc_204a Character vector representing the ATC code of respondent's fourth over-the-counter medication.
#' @param atc_205a Character vector representing the ATC code of respondent's fifth over-the-counter medication.
#' @param atc_206a Character vector representing the ATC code of respondent's sixth over-the-counter medication.
#' @param atc_207a Character vector representing the ATC code of respondent's seventh over-the-counter medication.
#' @param atc_208a Character vector representing the ATC code of respondent's eighth over-the-counter medication.
#' @param atc_209a Character vector representing the ATC code of respondent's ninth over-the-counter medication.
#' @param atc_210a Character vector representing the ATC code of respondent's tenth over-the-counter medication.
#' @param atc_211a Character vector representing the ATC code of respondent's eleventh over-the-counter medication.
#' @param atc_212a Character vector representing the ATC code of respondent's twelfth over-the-counter medication.
#' @param atc_213a Character vector representing the ATC code of respondent's thirteenth over-the-counter medication.
#' @param atc_214a Character vector representing the ATC code of respondent's fourteenth over-the-counter medication.
#' @param atc_215a Character vector representing the ATC code of respondent's fifteenth over-the-counter medication.
#' @param atc_131a Character vector representing the ATC code of respondent's first new prescription medication.
#' @param atc_132a Character vector representing the ATC code of respondent's second new prescription medication.
#' @param atc_133a Character vector representing the ATC code of respondent's third new prescription medication.
#' @param atc_134a Character vector representing the ATC code of respondent's fourth new prescription medication.
#' @param atc_135a Character vector representing the ATC code of respondent's fifth new prescription medication.
#' @param atc_231a Character vector representing the ATC code of respondent's first new over-the-counter medication.
#' @param atc_232a Character vector representing the ATC code of respondent's second new over-the-counter medication.
#' @param atc_233a Character vector representing the ATC code of respondent's third new over-the-counter medication.
#' @param atc_234a Character vector representing the ATC code of respondent's fourth new over-the-counter medication.
#' @param atc_235a Character vector representing the ATC code of respondent's fifth new over-the-counter medication.
#' @param mhr_101b Integer representing the response for when the first prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_102b Integer representing the response for when the second prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_103b Integer representing the response for when the third prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_104b Integer representing the response for when the fourth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_105b Integer representing the response for when the fifth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_106b Integer representing the response for when the sixth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_107b Integer representing the response for when the seventh prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_108b Integer representing the response for when the eighth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_109b Integer representing the response for when the ninth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_110b Integer representing the response for when the tenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_111b Integer representing the response for when the eleventh prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_112b Integer representing the response for when the twelfth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_113b Integer representing the response for when the thirteenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_114b Integer representing the response for when the fourteenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_115b Integer representing the response for when the fifteenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_201b Integer representing the response for when the first over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_202b Integer representing the response for when the second over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_203b Integer representing the response for when the third over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_204b Integer representing the response for when the fourth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_205b Integer representing the response for when the fifth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_206b Integer representing the response for when the sixth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_207b Integer representing the response for when the seventh over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_208b Integer representing the response for when the eighth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_209b Integer representing the response for when the ninth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_210b Integer representing the response for when the tenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_211b Integer representing the response for when the eleventh over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_212b Integer representing the response for when the twelfth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_213b Integer representing the response for when the thirteenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_214b Integer representing the response for when the fourteenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_215b Integer representing the response for when the fifteenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_131b Integer representing the response for when the first new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_132b Integer representing the response for when the second new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_133b Integer representing the response for when the third new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_134b Integer representing the response for when the fourth new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_135b Integer representing the response for when the fifth new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_231b Integer representing the response for when the first new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_232b Integer representing the response for when the second new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_233b Integer representing the response for when the third new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_234b Integer representing the response for when the fourth new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_235b Integer representing the response for when the fifth new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#'
#' @return nsaid, a numeric set to 1 if the person is taking any NSAIDs, NA if no information is available, 0 otherwise.
#'
#' @seealso `is_NSAID`
#'
#' @export
cycles1to2_nsaid <- function(
    atc_101a = NULL, atc_102a = NULL, atc_103a = NULL, atc_104a = NULL, atc_105a = NULL,
    atc_106a = NULL, atc_107a = NULL, atc_108a = NULL, atc_109a = NULL, atc_110a = NULL,
    atc_111a = NULL, atc_112a = NULL, atc_113a = NULL, atc_114a = NULL, atc_115a = NULL,
    atc_201a = NULL, atc_202a = NULL, atc_203a = NULL, atc_204a = NULL, atc_205a = NULL,
    atc_206a = NULL, atc_207a = NULL, atc_208a = NULL, atc_209a = NULL, atc_210a = NULL,
    atc_211a = NULL, atc_212a = NULL, atc_213a = NULL, atc_214a = NULL, atc_215a = NULL,
    atc_131a = NULL, atc_132a = NULL, atc_133a = NULL, atc_134a = NULL, atc_135a = NULL,
    atc_231a = NULL, atc_232a = NULL, atc_233a = NULL, atc_234a = NULL, atc_235a = NULL,
    mhr_101b = NULL, mhr_102b = NULL, mhr_103b = NULL, mhr_104b = NULL, mhr_105b = NULL,
    mhr_106b = NULL, mhr_107b = NULL, mhr_108b = NULL, mhr_109b = NULL, mhr_110b = NULL,
    mhr_111b = NULL, mhr_112b = NULL, mhr_113b = NULL, mhr_114b = NULL, mhr_115b = NULL,
    mhr_201b = NULL, mhr_202b = NULL, mhr_203b = NULL, mhr_204b = NULL, mhr_205b = NULL,
    mhr_206b = NULL, mhr_207b = NULL, mhr_208b = NULL, mhr_209b = NULL, mhr_210b = NULL,
    mhr_211b = NULL, mhr_212b = NULL, mhr_213b = NULL, mhr_214b = NULL, mhr_215b = NULL,
    mhr_131b = NULL, mhr_132b = NULL, mhr_133b = NULL, mhr_134b = NULL, mhr_135b = NULL,
    mhr_231b = NULL, mhr_232b = NULL, mhr_233b = NULL, mhr_234b = NULL, mhr_235b = NULL
) {
  # Identify variables for which a value was provided
  atc_vars <- as.data.frame(Filter(Negate(is.null), mget(ls(pattern = "^atc_"))))
  mhr_vars <- as.data.frame(Filter(Negate(is.null), mget(ls(pattern = "^mhr_"))))
  
  drugs <- cbind(atc_vars, mhr_vars)
  
  med_vars <- names(Filter(Negate(is.null), mget(ls(pattern = "^atc_.*a$"))))
  last_taken_vars <- names(Filter(Negate(is.null), mget(ls(pattern = "^mhr_.*b$"))))
  
  nsaid <- is_taking_drug_class(drugs, "NSAID", med_vars, last_taken_vars, is_NSAID, log_level="INFO", overwrite = TRUE)
  
  nsaid_drug <- 0
  
  if (nsaid$NSAID > 0 %in% TRUE) {
    nsaid_drug <- 1
  }
  else if (all(is.na(nsaid$NSAID))) {
    nsaid_drug <- haven::tagged_na("b")
  }
  
  return(nsaid_drug)
  
}

#' @title Determine if a person is taking diabetes drugs in CHMS cycles 1 to 2.
#' 
#' @description This function checks if a person is taking diabetes drugs based on the provided Anatomical Therapeutic Chemical (ATC) codes for medications 
#' and the Canadian Health Measures Survey (CHMS) response for the time when the medication was last taken.
#' 
#' @param atc_101a Character vector representing the ATC code of respondent's first prescription medication.
#' @param atc_102a Character vector representing the ATC code of respondent's second prescription medication.
#' @param atc_103a Character vector representing the ATC code of respondent's third  prescription medication.
#' @param atc_104a Character vector representing the ATC code of respondent's fourth prescription medication.
#' @param atc_105a Character vector representing the ATC code of respondent's fifth prescription medication.
#' @param atc_106a Character vector representing the ATC code of respondent's sixth prescription medication.
#' @param atc_107a Character vector representing the ATC code of respondent's seventh prescription medication.
#' @param atc_108a Character vector representing the ATC code of respondent's eighth prescription medication.
#' @param atc_109a Character vector representing the ATC code of respondent's ninth prescription medication.
#' @param atc_110a Character vector representing the ATC code of respondent's tenth prescription medication.
#' @param atc_111a Character vector representing the ATC code of respondent's eleventh prescription medication.
#' @param atc_112a Character vector representing the ATC code of respondent's twelfth prescription medication.
#' @param atc_113a Character vector representing the ATC code of respondent's thirteenth prescription medication.
#' @param atc_114a Character vector representing the ATC code of respondent's fourteenth prescription medication.
#' @param atc_115a Character vector representing the ATC code of respondent's fifteenth prescription medication.
#' @param atc_201a Character vector representing the ATC code of respondent's first over-the-counter medication.
#' @param atc_202a Character vector representing the ATC code of respondent's second over-the-counter medication.
#' @param atc_203a Character vector representing the ATC code of respondent's third over-the-counter medication.
#' @param atc_204a Character vector representing the ATC code of respondent's fourth over-the-counter medication.
#' @param atc_205a Character vector representing the ATC code of respondent's fifth over-the-counter medication.
#' @param atc_206a Character vector representing the ATC code of respondent's sixth over-the-counter medication.
#' @param atc_207a Character vector representing the ATC code of respondent's seventh over-the-counter medication.
#' @param atc_208a Character vector representing the ATC code of respondent's eighth over-the-counter medication.
#' @param atc_209a Character vector representing the ATC code of respondent's ninth over-the-counter medication.
#' @param atc_210a Character vector representing the ATC code of respondent's tenth over-the-counter medication.
#' @param atc_211a Character vector representing the ATC code of respondent's eleventh over-the-counter medication.
#' @param atc_212a Character vector representing the ATC code of respondent's twelfth over-the-counter medication.
#' @param atc_213a Character vector representing the ATC code of respondent's thirteenth over-the-counter medication.
#' @param atc_214a Character vector representing the ATC code of respondent's fourteenth over-the-counter medication.
#' @param atc_215a Character vector representing the ATC code of respondent's fifteenth over-the-counter medication.
#' @param atc_131a Character vector representing the ATC code of respondent's first new prescription medication.
#' @param atc_132a Character vector representing the ATC code of respondent's second new prescription medication.
#' @param atc_133a Character vector representing the ATC code of respondent's third new prescription medication.
#' @param atc_134a Character vector representing the ATC code of respondent's fourth new prescription medication.
#' @param atc_135a Character vector representing the ATC code of respondent's fifth new prescription medication.
#' @param atc_231a Character vector representing the ATC code of respondent's first new over-the-counter medication.
#' @param atc_232a Character vector representing the ATC code of respondent's second new over-the-counter medication.
#' @param atc_233a Character vector representing the ATC code of respondent's third new over-the-counter medication.
#' @param atc_234a Character vector representing the ATC code of respondent's fourth new over-the-counter medication.
#' @param atc_235a Character vector representing the ATC code of respondent's fifth new over-the-counter medication.
#' @param mhr_101b Integer representing the response for when the first prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_102b Integer representing the response for when the second prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_103b Integer representing the response for when the third prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_104b Integer representing the response for when the fourth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_105b Integer representing the response for when the fifth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_106b Integer representing the response for when the sixth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_107b Integer representing the response for when the seventh prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_108b Integer representing the response for when the eighth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_109b Integer representing the response for when the ninth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_110b Integer representing the response for when the tenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_111b Integer representing the response for when the eleventh prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_112b Integer representing the response for when the twelfth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_113b Integer representing the response for when the thirteenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_114b Integer representing the response for when the fourteenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_115b Integer representing the response for when the fifteenth prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_201b Integer representing the response for when the first over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_202b Integer representing the response for when the second over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_203b Integer representing the response for when the third over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_204b Integer representing the response for when the fourth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_205b Integer representing the response for when the fifth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_206b Integer representing the response for when the sixth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_207b Integer representing the response for when the seventh over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_208b Integer representing the response for when the eighth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_209b Integer representing the response for when the ninth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_210b Integer representing the response for when the tenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_211b Integer representing the response for when the eleventh over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_212b Integer representing the response for when the twelfth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_213b Integer representing the response for when the thirteenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_214b Integer representing the response for when the fourteenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_215b Integer representing the response for when the fifteenth over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_131b Integer representing the response for when the first new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_132b Integer representing the response for when the second new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_133b Integer representing the response for when the third new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_134b Integer representing the response for when the fourth new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_135b Integer representing the response for when the fifth new prescription medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_231b Integer representing the response for when the first new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_232b Integer representing the response for when the second new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_233b Integer representing the response for when the third new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_234b Integer representing the response for when the fourth new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#' @param mhr_235b Integer representing the response for when the fifth new over-the-counter medication was last taken.
#'                 1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                 5 = More than a month ago, 6 = Never taken
#'
#' @return diab_drug, a numeric set to 1 if the person is taking any diabetes drugs, NA if no information is available, 0 otherwise.
#'
#' @seealso `is_diabetes_drug`
#'
#' @export
cycles1to2_diabetes_drugs <- function(
    atc_101a = NULL, atc_102a = NULL, atc_103a = NULL, atc_104a = NULL, atc_105a = NULL,
    atc_106a = NULL, atc_107a = NULL, atc_108a = NULL, atc_109a = NULL, atc_110a = NULL,
    atc_111a = NULL, atc_112a = NULL, atc_113a = NULL, atc_114a = NULL, atc_115a = NULL,
    atc_201a = NULL, atc_202a = NULL, atc_203a = NULL, atc_204a = NULL, atc_205a = NULL,
    atc_206a = NULL, atc_207a = NULL, atc_208a = NULL, atc_209a = NULL, atc_210a = NULL,
    atc_211a = NULL, atc_212a = NULL, atc_213a = NULL, atc_214a = NULL, atc_215a = NULL,
    atc_131a = NULL, atc_132a = NULL, atc_133a = NULL, atc_134a = NULL, atc_135a = NULL,
    atc_231a = NULL, atc_232a = NULL, atc_233a = NULL, atc_234a = NULL, atc_235a = NULL,
    mhr_101b = NULL, mhr_102b = NULL, mhr_103b = NULL, mhr_104b = NULL, mhr_105b = NULL,
    mhr_106b = NULL, mhr_107b = NULL, mhr_108b = NULL, mhr_109b = NULL, mhr_110b = NULL,
    mhr_111b = NULL, mhr_112b = NULL, mhr_113b = NULL, mhr_114b = NULL, mhr_115b = NULL,
    mhr_201b = NULL, mhr_202b = NULL, mhr_203b = NULL, mhr_204b = NULL, mhr_205b = NULL,
    mhr_206b = NULL, mhr_207b = NULL, mhr_208b = NULL, mhr_209b = NULL, mhr_210b = NULL,
    mhr_211b = NULL, mhr_212b = NULL, mhr_213b = NULL, mhr_214b = NULL, mhr_215b = NULL,
    mhr_131b = NULL, mhr_132b = NULL, mhr_133b = NULL, mhr_134b = NULL, mhr_135b = NULL,
    mhr_231b = NULL, mhr_232b = NULL, mhr_233b = NULL, mhr_234b = NULL, mhr_235b = NULL
) {
  # Identify variables for which a value was provided
  atc_vars <- as.data.frame(Filter(Negate(is.null), mget(ls(pattern = "^atc_"))))
  mhr_vars <- as.data.frame(Filter(Negate(is.null), mget(ls(pattern = "^mhr_"))))
  
  drugs <- cbind(atc_vars, mhr_vars)
  
  med_vars <- names(Filter(Negate(is.null), mget(ls(pattern = "^atc_.*a$"))))
  last_taken_vars <- names(Filter(Negate(is.null), mget(ls(pattern = "^mhr_.*b$"))))
  
  diab <- is_taking_drug_class(drugs, "diabetes_drug", med_vars, last_taken_vars, is_diabetes_drug, log_level="INFO", overwrite = TRUE)
  
  diab_drug <- 0
  
  if (diab$diabetes_drug > 0 %in% TRUE) {
    diab_drug <- 1
  }
  else if (all(is.na(diab$diabetes_drug))) {
    diab_drug <- haven::tagged_na("b")
  }
  
  return(diab_drug)
  
}