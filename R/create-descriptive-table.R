source("R/get-descriptive-data.R")
source("R/variables-sheet-utils.R")

create_descriptive_table <- function(
  descriptive_data,
  variables_sheet,
  variable_details_sheet,
  variables,
  column_stratifier = NULL,
  row_stratifiers = list(),
  subjects_order = NULL
) {
  stratify_config <- row_stratifiers
  if(!is.null(column_stratifier)) {
    stratify_config[["all"]] <- c(column_stratifier)
  }

  formatted_descriptive_data <- descriptive_data %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ signif(.x, digits = 4)))
  
  subjects_in_table <- c()
  for(variable in variables) {
    variable_sheet_row <- variables_sheet[variables_sheet$variable == variable, ]
    if(!variable_sheet_row[1, ]$subject %in% subjects_in_table) {
      subjects_in_table <- c(subjects_in_table, variable_sheet_row[1, ]$subject)
    }
  }
  # Make sure the section_order parameter is properly set
  # If the user did not pass a value for it, set it to the sections in the
  # variables sheet
  if(!is.null(subjects_order)) {
    subject_not_in_table_subjects <- subjects_order[
      !subjects_in_table %in% subjects_order]
    if(length(subject_not_in_table_subjects) != 0) {
      stop_message <- paste(
        "Subjects \"", 
        paste(subject_not_in_table_subjects, collapse = "; "), 
        "\" not included in order", 
        sep = ""
      )
      stop(stop_message)
    }
    
    subjects_not_in_variables_sheet <- subjects_order[
      !subjects_order %in% variables_sheet$subject]
    if(length(subjects_not_in_variables_sheet) != 0) {
      stop_message <- paste(
        "Subjects \"", 
        paste(subjects_not_in_variables_sheet, collapse = "; "), 
        "\" not found in variables sheet", 
        sep = ""
      )
      stop(stop_message)
    }
    
    subjects_in_table <- subjects_order
  }
  
  stratifier_rows <- get_unique_rec_end_rows(
    variable_details_sheet, 
    column_stratifier
  )
  stratify_by_stats <- list()
  for(stratifier_row_index in seq_len(nrow(stratifier_rows))) {
    stratify_by_stats[[stratifier_rows[stratifier_row_index, "catLabel"]]] <- c()
  }
  descriptive_table_variables <- c()
  descriptive_table_type <- c()
  subject_row_indexes <- c()
  variable_row_indexes <- c()
  category_row_indexes <- c()
  stratify_for_indexes <- c()
  # Keeps track of all the rows which are stratified by another variable
  # excluding the main column stratifier
  # We will use this to triple indent these rows in the final table
  row_stratifier_indexes <- c()
  for(subject in subjects_in_table) {
    descriptive_table_variables <- c(descriptive_table_variables, subject)
    descriptive_table_type <- c(descriptive_table_type, "")
    for(stratifier_row_index in seq_len(nrow(stratifier_rows))) {
      stratify_by_stats[[stratifier_rows[stratifier_row_index, "catLabel"]]] <- c(
        stratify_by_stats[[stratifier_rows[stratifier_row_index, "catLabel"]]],
        ""
      )
    }
    subject_row_indexes <- c(subject_row_indexes, length(descriptive_table_variables))
    
    for(variable in variables) {
      variable_sheet_row <- get_row_for_variable(
        variable,
        variables_sheet
      )
      
      if(variable_sheet_row[1, ]$subject != subject) {
        next
      }
      
      if(!is.null(row_stratifiers[[variable]])) {
        next
      }
      
      data_for_current_variable <- formatted_descriptive_data[formatted_descriptive_data$variable == variable, ]
      if(variable_sheet_row[1, ]$variableType == "Categorical") {
        descriptive_table_variables <- c(descriptive_table_variables, variable_sheet_row[1, "label"])
        descriptive_table_type <- c(descriptive_table_type, "Categorical")
        for(stratifier_row_index in seq_len(nrow(stratifier_rows))) {
          stratify_by_stats[[stratifier_rows[stratifier_row_index, "catLabel"]]] <- c(
            stratify_by_stats[[stratifier_rows[stratifier_row_index, "catLabel"]]],
            ""
          )
        }
        variable_row_indexes <- c(variable_row_indexes, length(descriptive_table_variables))
        
        stratifier_fors <- c()
        for(row_stratifier in names(row_stratifiers)) {
          if(row_stratifiers[[row_stratifier]][1] == variable) {
            stratifier_fors <- c(
              stratifier_fors,
              row_stratifier
            )
          }  
        }
        
        categories <- get_unique_rec_end_rows(
          variable_details_sheet,
          variable,
          TRUE
        )
        for(category_index in seq_len(nrow(categories))) {
          descriptive_table_variables <- c(descriptive_table_variables, categories[category_index, "catLabel"])
          descriptive_table_type <- c(descriptive_table_type, "")
          category_row_indexes <- c(category_row_indexes, length(descriptive_table_variables))
          data_for_current_category <- data_for_current_variable[data_for_current_variable$cat == categories[category_index, ]$recEnd, ]
          for(stratifier_row_index in seq_len(nrow(stratifier_rows))) {
            data_for_current_stratifier_val <- data_for_current_category[
              data_for_current_category$groupBy_1 == column_stratifier & 
                data_for_current_category$groupByValue_1 == stratifier_rows[stratifier_row_index, "recEnd"], ]
            stratify_by_stats[[stratifier_rows[stratifier_row_index, "catLabel"]]] <- c(
              stratify_by_stats[[stratifier_rows[stratifier_row_index, "catLabel"]]],
              format_cat_descriptive_data(data_for_current_stratifier_val)
            )

            for(stratifier_for_index in seq_len(length(stratifier_fors))) {
              stratifier_for <- stratifier_fors[stratifier_for_index]
              
              stratifier_for_variables_sheet_row <- get_row_for_variable(
                stratifier_for,
                variables_sheet
              )
              
              if(stratifier_row_index == 1) {
                descriptive_table_variables <- c(
                  descriptive_table_variables,
                  stratifier_for_variables_sheet_row[1, "label"]
                )  
                descriptive_table_type <- c(descriptive_table_type, "")
                stratify_for_indexes <- c(
                  stratify_for_indexes,
                  length(descriptive_table_variables)
                )
              }
              
              if(is_categorical_variable(stratifier_for_variables_sheet_row)) {
                stop(paste(
                  "Code currently does not handle a row stratification of type categorical",
                  stratifier_for
                ))
              }
              
              data_for_current_stratifier_for <- formatted_descriptive_data[
                formatted_descriptive_data$variable == stratifier_for &
                  formatted_descriptive_data$groupBy_1 == column_stratifier &
                  formatted_descriptive_data$groupByValue_1 == stratifier_rows[stratifier_row_index, "recEnd"] &
                  formatted_descriptive_data$groupBy_2 == variable &
                  formatted_descriptive_data$groupByValue_2 == categories[category_index, "recEnd"] &
                  is.na(formatted_descriptive_data$cat),
                
              ]
              
              stratify_by_stats[[stratifier_rows[stratifier_row_index, "catLabel"]]] <- c(
                stratify_by_stats[[stratifier_rows[stratifier_row_index, "catLabel"]]],
                format_cont_descriptive_data(data_for_current_stratifier_for[1, ])
              )
              
              if(!is.na(data_for_current_stratifier_for[1, "groupBy_2"])) {
                row_stratifier_indexes <- c(row_stratifier_indexes, length(descriptive_table_variables))
              }
            }
          }
        }
      } 
      else {
        descriptive_table_variables <- c(descriptive_table_variables, variable_sheet_row[1, "label"])
        descriptive_table_type <- c(descriptive_table_type, "Continuous")
        variable_row_indexes <- c(variable_row_indexes, length(descriptive_table_variables))
        
        missing_categories <- get_unique_rec_end_rows(
          variable_details_sheet,
          variable,
          TRUE
        ) %>%
        dplyr::filter(recEnd %in% c("NA::a", "NA::b", "NA::c"))
        for(missing_categories_index in seq_len(nrow(missing_categories))) {
          descriptive_table_variables <- c(descriptive_table_variables, missing_categories[missing_categories_index, "catLabel"])
          descriptive_table_type <- c(descriptive_table_type, "")
          category_row_indexes <- c(category_row_indexes, length(descriptive_table_variables))
          
          for(stratifier_row_index in seq_len(nrow(stratifier_rows))) {
            data_for_current_stratifier_val <- formatted_descriptive_data[
              formatted_descriptive_data$groupBy_1 == column_stratifier & 
                formatted_descriptive_data$groupByValue_1 == stratifier_rows[stratifier_row_index, "recEnd"], ]
            data_for_current_variable <- data_for_current_stratifier_val[data_for_current_stratifier_val$variable == variable, ]
            
            if(missing_categories_index == 1) {
              data_for_continuous_stats <- data_for_current_variable[is.na(data_for_current_variable$cat), ]
              stratify_by_stats[[stratifier_rows[stratifier_row_index, "catLabel"]]] <- c(
                stratify_by_stats[[stratifier_rows[stratifier_row_index, "catLabel"]]],
                format_cont_descriptive_data(data_for_continuous_stats[1, ])
              )
            }
            
            data_for_current_missing_category <- data_for_current_variable[data_for_current_variable$cat == missing_categories[missing_categories_index, "recEnd"], ]
            stratify_by_stats[[stratifier_rows[stratifier_row_index, "catLabel"]]] <- c(
              stratify_by_stats[[stratifier_rows[stratifier_row_index, "catLabel"]]],
              # Take the second row because the first row for some reason has
              # all NAs
              format_cat_descriptive_data(data_for_current_missing_category[2, ])
            )
          }
        }
      }
    }
  }
  
  descriptive_table <- data.frame(
    "variable" = descriptive_table_variables,
    "type" = descriptive_table_type
  )
  
  header_labels <- list(
    variable = "Variable",
    type = "Type"
  )
  for(stratifier_row_index in seq_len(nrow(stratifier_rows))) {
    descriptive_table[[stratifier_rows[stratifier_row_index, "catLabel"]]] <- stratify_by_stats[[stratifier_rows[stratifier_row_index, "catLabel"]]]
    header_labels[[stratifier_rows[stratifier_row_index, "catLabel"]]] <- stratifier_rows[stratifier_row_index, "catLabel"]
  }
  
  return(
    flextable::flextable(descriptive_table) %>%
      flextable::set_header_labels(
        values = header_labels
      ) %>%
      flextable::set_caption("Sex stratified population characteristics") %>%
      flextable::bold(subject_row_indexes) %>%
      flextable::padding(i = variable_row_indexes, j = 1, padding.left = 10) %>%
      flextable::padding(i = category_row_indexes, j = 1, padding.left = 20) %>%
      flextable::padding(i = row_stratifier_indexes, j = 1, padding.left = 30) %>%
      flextable::fontsize(size = 8, part = c("all")) %>%
      flextable::theme_vanilla() %>%
      flextable::autofit()
  )
}

format_cat_descriptive_data <- function(descriptive_data_row) {
  if(is.na(descriptive_data_row[1, "n"]) | descriptive_data_row[1, "n"] == 0) {
    return("No data")
  }
  
  formatted_n <- format(descriptive_data_row[1, "n"], big.mark = ",")
  return(paste(formatted_n, "(", descriptive_data_row[1, "percent"]*100, ")"))
}

format_cont_descriptive_data <- function(descriptive_data_row) {
  if(descriptive_data_row[1, "n"] == 0) {
    return("No data")
  }
  
  return(paste(
    descriptive_data_row[1, "min"], "-", descriptive_data_row[1, "max"], ",",
    descriptive_data_row[1, "median"], "(", descriptive_data_row[1, "percentile25"], "-", descriptive_data_row[1, "percentile75"], ")"
  ))
}