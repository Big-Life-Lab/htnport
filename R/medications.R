#' @file
#' Load packages
library(logger)

#' @title Calculate the number of occurrences of a specific drug class based on given conditions.
#' 
#' This function calculates the number of occurrences of a specific drug class in the data frame.
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
    log_fatal("The 'class_var_name' must be a non-empty character string.")
    stop()
  }
  
  if (!is.logical(overwrite)) {
    log_fatal("'overwrite' must be a logical value (TRUE or FALSE).")
    stop()
  }
  
  if (!all(med_vars %in% names(df))) {
    missing_vars <- med_vars[!(med_vars %in% names(df))]
    error_msg <- paste0("The following medication variables are not in the data frame: ", paste(missing_vars, collapse = ", "))
    log_error(error_msg)
    stop()
  }
  
  if (!all(last_taken_vars %in% names(df))) {
    missing_vars <- last_taken_vars[!(last_taken_vars %in% names(df))]
    error_msg <- paste0("The following 'last_taken' variables are not in the data frame: ", paste(missing_vars, collapse = ", "))
    log_error(error_msg)
    stop()
  }
  
  if (length(med_vars) != length(last_taken_vars)) {
    error_msg <- "The lists of medication variables and 'last_taken' variables are not of the same length."
    log_warn(error_msg)
    stop()
  }
  
  # Set the log level
  log_threshold(log_level)
  
  # Check if class_var_name already exists in the data frame
  if (class_var_name %in% names(df)) {
    error_msg <- paste0("Variable '", class_var_name, "' already exists in the data frame.")
    if (overwrite) {
      log_warn(paste0(error_msg, " The variable will be overwritten."))
    } else {
      log_fatal(paste0(error_msg, " Use a new variable name or change 'overwrite=TRUE'."))
      stop()
    }
  }
  
  log_info(paste0("Adding variable '", class_var_name, "' to the data frame."))
  
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

#' @title Determine if a CHMS respondent's medication is a beta blocker.
#' 
#' This function checks if a given medication for a CHMS respondent belongs to the beta blocker drug class.
#' The specific conditions for identifying a beta blocker are based on Anatomical Therapeutic Chemical (ATC) codes
#' and the time when the medication was last taken.
#' 
#' @param MEUCATC A character vector representing the Anatomical Therapeutic Chemical (ATC) code of the medication.
#' @param NPI_25B An integer representing the CHMS response for the time when the medication was last taken.
#'                        1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                        5 = More than a month ago, 6 = Never taken
#' 
#' @return A logical, TRUE if medication is in the beta blocker class and FALSE if it is not.
#' 
#' @details This function uses the `startsWith` function to identify beta blockers based on their ATC codes, which
#'          typically start with "C07". Additionally, specific sub-codes 'C07AA07', 'C07AA12', and 'C07AG02' are excluded 
#'          from the beta blocker class. If the ATC code matches the pattern and is not in the exclusion list, and the 
#'          medication was taken within the last month (NPI_25B <= 4), the medication is considered a beta blocker, and the function
#'          returns TRUE. Otherwise, it returns FALSE.
#' 
#' @examples
#' 
#' Let's say a for one respondent's medication, the ATC code is C07AA13 and the time they last took it was within the last 
#' week. By passing in the code and integer time response (3 in this case) into the function, it can be checked if the 
#' given respondent's medication is a beta blocker (1) or not (0).
#' 
#' is_beta_blocker('C07AA13', 3)
#' 
#' @export
is_beta_blocker <- function(MEUCATC, NPI_25B) {
  starts_with_C07 <- startsWith(MEUCATC, "C07")
  not_in_specific_codes <- !(MEUCATC %in% c('C07AA07', 'C07AA12', 'C07AG02'))
  time_condition <- NPI_25B <= 4
  
  is_beta_blocker <- starts_with_C07 & not_in_specific_codes & time_condition
  return(as.numeric(is_beta_blocker))
}

#' @title Determine if a CHMS respondent's medication is an ACE inhibitor.
#' 
#' This function checks if a given medication for a CHMS respondent belongs to the ACE inhibitor drug class.
#' The identification is based on the Anatomical Therapeutic Chemical (ATC) code of the medication and the time when the
#' medication was last taken.
#' 
#' @param MEUCATC A character vector representing the Anatomical Therapeutic Chemical (ATC) code of the medication.
#' @param NPI_25B An integer representing the CHMS response for the time when the medication was last taken.
#'                        1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                        5 = More than a month ago, 6 = Never taken
#' 
#' @return A logical, TRUE if medication is in the ACE inhibitor class and FALSE if it is not.
#' 
#' @details This function uses the `startsWith` function to identify ACE inhibitors based on their ATC codes, which
#'          typically start with "C09". If the ATC code matches the pattern and the medication was taken within the last
#'          month (NPI_25B <= 4), the medication is considered an ACE inhibitor and the function returns TRUE.
#'          Otherwise, it returns FALSE.
#' 
#' @examples
#' 
#' Let's say a for one respondent's medication, the ATC code is C09AB03 and the time they last took it was yesterday. By 
#' passing in the code and integer time response (2 in this case) into the function, it can be checked if the given 
#' respondent's medication is an ACE inhibitor (1) or not (0).
#' 
#' is_ace_inhibitor('C09AB03', 2)
#' 
#' @export
is_ace_inhibitor <- function(MEUCATC, NPI_25B) {
  as.numeric(startsWith(MEUCATC, "C09") && NPI_25B <= 4)
}

#' @title Determine if a CHMS respondent's medication is a diuretic.
#' 
#' This function checks if a given medication for a CHMS respondent belongs to the diuretic drug class.
#' The identification is based on the Anatomical Therapeutic Chemical (ATC) code of the medication and the time when the
#' medication was last taken.
#' 
#' @param MEUCATC A character vector representing the Anatomical Therapeutic Chemical (ATC) code of the medication.
#' @param NPI_25B An integer representing the CHMS response for the time when the medication was last taken.
#'                        1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                        5 = More than a month ago, 6 = Never taken
#' 
#' @return A logical, TRUE if medication is in the diuretic class and FALSE if it is not.
#' 
#' @details This function uses the `startsWith` function to identify diuretics based on their ATC codes, which
#'          typically start with "C03". Additionally, specific sub-codes 'C03BA08' and 'C03CA01' are excluded from the
#'          diuretic class. If the ATC code matches the pattern and is not in the exclusion list, and the medication was
#'          taken within the last month (NPI_25B <= 4), the medication is considered a diuretic, and the function
#'          returns TRUE. Otherwise, it returns FALSE.
#' 
#' @examples
#' 
#' Let's say a for one respondent's medication, the ATC code is C03AA03 and the time they last took it was within the last 
#' week. By passing in the code and integer time response (3 in this case) into the function, it can be checked if the 
#' given respondent's medication is a diuretic (1) or not (0).
#' 
#' is_diuretic('C03AA03', 3)
#' 
#' @export
is_diuretic <- function(MEUCATC, NPI_25B) {
  as.numeric(startsWith(MEUCATC, "C03") && !(MEUCATC %in% c('C03BA08', 'C03CA01')) && NPI_25B <= 4)
}

#' @title Determine if a CHMS respondent's medication is a calcium channel blocker.
#' 
#' This function checks if a given medication for a CHMS respondent belongs to the calcium channel blocker drug class.
#' The identification is based on the Anatomical Therapeutic Chemical (ATC) code of the medication and the time when the
#' medication was last taken.
#' 
#' @param MEUCATC A character vector representing the Anatomical Therapeutic Chemical (ATC) code of the medication.
#' @param NPI_25B An integer representing the CHMS response for the time when the medication was last taken.
#'                        1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                        5 = More than a month ago, 6 = Never taken
#' 
#' @return A logical, TRUE if medication is in the calcium channel blocker class and FALSE if it is not.
#' 
#' @details This function uses the `startsWith` function to identify calcium channel blockers based on their ATC codes,
#'          which typically start with "C08". If the ATC code matches the pattern and the medication was taken within
#'          the last month (NPI_25B <= 4), the medication is considered a calcium channel blocker, and the
#'          function returns TRUE. Otherwise, it returns FALSE.
#' 
#' @examples
#' 
#' Let's say a for one respondent's medication, the ATC code is C08CA05 and the time they last took it was today. By 
#' passing in the code and integer time response (1 in this case) into the function, it can be checked if the given 
#' respondent's medication is a calcium channel blocker (1) or not (0).
#' 
#' is_calcium_channel_blocker('C08CA05', 1)
#' 
#' @export
is_calcium_channel_blocker <- function(MEUCATC, NPI_25B) {
  as.numeric(startsWith(MEUCATC, "C08") && NPI_25B <= 4)
}

#' @title Determine if a CHMS respondent's medication is another anti-hypertensive drug.
#' 
#' This function checks if a given medication for a CHMS respondent belongs to another anti-hypertensive drug class.
#' The identification is based on the Anatomical Therapeutic Chemical (ATC) code of the medication and the time when the
#' medication was last taken.
#' 
#' @param MEUCATC A character vector representing the Anatomical Therapeutic Chemical (ATC) code of the medication.
#' @param NPI_25B An integer representing the CHMS response for the time when the medication was last taken.
#'                        1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                        5 = More than a month ago, 6 = Never taken
#' 
#' @return A logical, TRUE if medication is in the other anti-hypertensive drug class and FALSE if it is not.
#' 
#' @details This function uses the `startsWith` function to identify other anti-hypertensive drugs based on their ATC
#'          codes, which typically start with "C02". The sub-code 'C02KX01' is excluded from the class. If the ATC code
#'          matches the pattern and is not in the exclusion list, and the medication was taken within the last month
#'          (NPI_25B <= 4), the medication is considered another anti-hypertensive drug, and the function returns
#'          TRUE. Otherwise, it returns FALSE.
#' 
#' @examples
#' 
#' Let's say a for one respondent's medication, the ATC code is C02AC04 and the time they last took it was within the last 
#' week. By passing in the code and integer time response (3 in this case) into the function, it can be checked if the 
#' given respondent's medication is another anti-hypertensive drug (1) or not (0).
#' 
#' is_other_antiHTN_med('C02AC04', 3)
#' 
#' @export
is_other_antiHTN_med <- function(MEUCATC, NPI_25B) {
  as.numeric(startsWith(MEUCATC, "C02") && !(MEUCATC %in% c('C02KX01')) && NPI_25B <= 4)
}

#' @title Determine if a CHMS respondent's medication is any anti-hypertensive drug.
#' 
#' This function checks if a given medication for a CHMS respondent belongs to any anti-hypertensive drug class.
#' The identification is based on the Anatomical Therapeutic Chemical (ATC) code of the medication and the time when the
#' medication was last taken.
#' 
#' @param MEUCATC A character vector representing the Anatomical Therapeutic Chemical (ATC) code of the medication.
#' @param NPI_25B An integer representing the CHMS response for the time when the medication was last taken.
#'                        1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                        5 = More than a month ago, 6 = Never taken
#' 
#' @return A logical, TRUE if medication is in any anti-hypertensive drug class and FALSE if it is not.
#' 
#' @details This function first identifies any anti-hypertensive drugs based on their ATC codesm using the regular expression 
#'          'C0[2, 3, 7, 8, 9]' which matches ATC codes that start with "C02", "C03", "C07", "C08", and "C09". Specific sub-codes 
#'          'C07AA07', 'C07AA12', 'C07AG02', 'C03BA08', 'C03CA01', and 'C02KX01' are excluded from the class. If the ATC 
#'          code matches the pattern and is not in the exclusion list, and the medication was taken within the last month 
#'          (NPI_25B <= 4), the medication is considered an anti-hypertensive drug, and the function returns TRUE. 
#'          Otherwise, it returns FALSE.
#' 
#' @examples
#' 
#' Let's say a for one respondent's medication, the ATC code is C07AB02 and the time they last took it was within the last 
#' month. By passing in the code and integer time response (4 in this case) into the function, it can be checked if the 
#' given respondent's medication is an anti-hypertensive drug (1) or not (0).
#' 
#' is_any_antiHTN_med('C07AB02', 4)
#' 
#' @export
is_any_antiHTN_med <- function(MEUCATC, NPI_25B) {
  as.numeric(grepl('^C0[2, 3, 7, 8, 9]', MEUCATC) && !(MEUCATC %in% c('C07AA07', 'C07AA12', 'C07AG02', 'C03BA08', 'C03CA01', 'C02KX01')) && NPI_25B <= 4)
}

#' @title Determine if a CHMS respondent's medication is a non-steroidal anti-inflammatory drug (NSAID).
#' 
#' This function checks if a given medication for a CHMS respondent belongs to the non-steroidal anti-inflammatory drug
#' (NSAID) class. The identification is based on the Anatomical Therapeutic Chemical (ATC) code of the medication and the
#' time when the medication was last taken.
#' 
#' @param MEUCATC A character vector representing the Anatomical Therapeutic Chemical (ATC) code of the medication.
#' @param NPI_25B An integer representing the CHMS response for the time when the medication was last taken.
#'                        1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                        5 = More than a month ago, 6 = Never taken
#' 
#' @return A logical, TRUE if medication is in the NSAID class and FALSE if it is not.
#' 
#' @details This function uses the `startsWith` function to identify NSAIDs based on their ATC codes, which typically
#'          start with "M01A". If the ATC code matches the pattern and the medication was taken within the last month
#'          (NPI_25B <= 4), the medication is considered an NSAID, and the function returns TRUE. Otherwise, it
#'          returns FALSE.
#' 
#' @examples
#' 
#' Let's say a for one respondent's medication, the ATC code is M01AB05 and the time they last took it was today. By
#' passing in the code and integer time response (1 in this case) into the function, it can be checked if the given
#' respondent's medication is an NSAID (1) or not (0).
#' 
#' is_NSAID('M01AB05', 1)
#' 
#' @export
is_NSAID <- function(MEUCATC, NPI_25B) {
  as.numeric(startsWith(MEUCATC, "M01A") && NPI_25B <= 4)
}

#' @title Determine if a CHMS respondent's medication is a diabetes drug.
#' 
#' This function checks if a given medication for a CHMS respondent belongs to the diabetes drug class.
#' The identification is based on the Anatomical Therapeutic Chemical (ATC) code of the medication and the time when the
#' medication was last taken.
#' 
#' @param MEUCATC A character vector representing the Anatomical Therapeutic Chemical (ATC) code of the medication.
#' @param NPI_25B An integer representing the CHMS response for the time when the medication was last taken.
#'                        1 = Today, 2 = Yesterday, 3 = Within the last week, 4 = Within the last month, 
#'                        5 = More than a month ago, 6 = Never taken
#' 
#' @return A logical, TRUE if medication is in the diabetes drug class and FALSE if it is not.
#' 
#' @details This function uses the `startsWith` function to identify diabetes drugs based on their ATC codes, which
#'          typically start with "A10". If the ATC code matches the pattern and the medication was taken within the last
#'          month (NPI_25B <= 4), the medication is considered a diabetes drug, and the function returns TRUE.
#'          Otherwise, it returns FALSE.
#' 
#' @examples
#' 
#' Let's say a for one respondent's medication, the ATC code is A10BB09 and the time they last took it was within the last 
#' week. By passing in the code and integer time response (3 in this case) into the function, it can be checked if the 
#' given respondent's medication is a diabetes drug (1) or not (0).
#' 
#' is_diabetes_drug('A10BB09', 3)
#' 
#' @export
is_diabetes_drug <- function(MEUCATC, NPI_25B) {
  as.numeric(startsWith(MEUCATC, "A10") && NPI_25B <= 4)
}