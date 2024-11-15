library(flextable)
library(magrittr)
library(dplyr)

source("R/variables-sheet-utils.R")
source("R/variable-details-sheet-utils.R")

create_unstratified_descriptive_table <- function(
  descriptive_data,
  variables_sheet,
  variable_details_sheet,
  subjects_order = NULL
) {
  descriptive_data <- descriptive_data %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ signif(.x, digits = 4)))
  
  table_subjects <- c()
  for(descriptive_data_index in seq_len(nrow(descriptive_data))) {
    variable <- descriptive_data[descriptive_data_index, "variable"]
    variable_variables_sheet_row <- get_row_for_variable(
      variable,
      variables_sheet
    )
    if(!variable_variables_sheet_row$subject %in% table_subjects) {
      table_subjects <- c(
        table_subjects,
        variable_variables_sheet_row$subject
      )
    }
  }
  # Make sure the section_order parameter is properly set
  # If the user did not pass a value for it, set it to the sections in the
  # variables sheet
  if(!is.null(subjects_order)) {
    subject_not_in_table_subjects <- subjects_order[
      !table_subjects %in% subjects_order]
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
    
    table_subjects <- subjects_order
  }
  
  all_variables <- unique(descriptive_data$variable)
  descriptive_table <- data.frame(
    variable = c(),
    type = c(),
    statistic = c()
  )
  subject_indexes <- c()
  variable_indexes <- c()
  category_indexes <- c()
  nested_variable_indexes <- c()
  for(subject in table_subjects) {
    descriptive_table <- rbind(
      descriptive_table,
      data.frame(
        variable = c(subject),
        type = c(""),
        statistic = c("")
      )
    )
    subject_indexes <- c(subject_indexes, nrow(descriptive_table))
    for(variable in all_variables) {
      variable_variables_sheet_row <- get_row_for_variable(
        variable,
        variables_sheet
      )
      
      if(nrow(descriptive_data[descriptive_data$variable == variable &
                               !is.na(descriptive_data$groupBy_1), ]) != 0) {
        next
      }
      
      if(variable_variables_sheet_row$subject == subject) {
        variable_categories <- get_unique_rec_end_rows(
          variable_details_sheet,
          variable,
          include_NA = TRUE
        )$recEnd
        for(i in seq_len(length(variable_categories))) {
          variable_category <- variable_categories[[i]]
          
          variable_label <- ""
          if(i == 1) {
            variable_label <- variable_variables_sheet_row$label
          }
          
          type <- ""
          if(i == 1) {
            if(is_continuous_variable(variable_variables_sheet_row)) {
              type <- "Continuous"
            }
            else {
              type <- "Categorical"
            }
          }
          
          if(is_continuous_variable(variable_variables_sheet_row) & i == 1) {
            descriptive_table <- rbind(
              descriptive_table,
              data.frame(
                variable = c(variable_label),
                type = c(type),
                statistic = c(format_cont_descriptive_data(
                  descriptive_data[descriptive_data$variable == variable & is.na(descriptive_data$cat), ]
                ))
              )
            )
            variable_indexes <- c(variable_indexes, nrow(descriptive_table))
          }
          
          if(is_categorical_variable(variable_variables_sheet_row) & i == 1) {
            descriptive_table <- rbind(
              descriptive_table,
              data.frame(
                variable = c(variable_label),
                type = c(type),
                statistic = c("")
              )
            )
            variable_indexes <- c(variable_indexes, nrow(descriptive_table))
          }
          
          
          if(variable_category == "NA::a" | variable_category == "NA::b") {
            next
          }
          
          if(is_categorical_variable(variable_variables_sheet_row)) {
            category_label <- variable_details_sheet[
              variable_details_sheet$variable == variable &
                variable_details_sheet$recEnd == variable_category,
            ][1, "catLabel"]
            descriptive_table <- rbind(
              descriptive_table,
              data.frame(
                variable = c(category_label),
                type = c(""),
                statistic = c(format_cat_descriptive_data(
                  descriptive_data[
                    descriptive_data$variable == variable &
                      descriptive_data$cat == variable_category, ]
                ))
              )
            )
            category_indexes <- c(
              category_indexes,
              nrow(descriptive_table)
            )
          }
          
          group_by_variables <- get_group_by_variable(
            descriptive_data, 
            variable
          )
          for(group_by_variable in group_by_variables) {
            group_by_variable_variables_sheet_row <- get_row_for_variable(
              group_by_variable,
              variables_sheet
            )
            if(is_continuous_variable(group_by_variable_variables_sheet_row)) {
              group_by_variable_descriptive_data_row <- descriptive_data[
                descriptive_data$variable == group_by_variable &
                  descriptive_data$groupBy_1 == variable &
                    descriptive_data$groupByValue == variable_category,
              ][1, ]
              if(group_by_variable == "pack_years_der") {
                if(!is.na(group_by_variable_descriptive_data_row[1, "min"]) & 
                          group_by_variable_descriptive_data_row[1, "min"] < 0) {
                  group_by_variable_descriptive_data_row[1, "min"] <- 0
                }
              }
              descriptive_table <- rbind(
                descriptive_table,
                data.frame(
                  variable = c(group_by_variable_variables_sheet_row$label),
                  type = c("Continuous"),
                  statistic = c(format_cont_descriptive_data(
                    group_by_variable_descriptive_data_row
                  ))
                )
              )
              
              nested_variable_indexes <- c(
                nested_variable_indexes,
                nrow(descriptive_table)
              )
            }
            else {
              stop()
            }
          }
        }
      }
    }
  }
  
  return(
    flextable::flextable(descriptive_table) %>%
      flextable::set_header_labels(
        values = list(
          variable = "Variable",
          type = "Type",
          statistic = "Min - Max, Median (IQR) or n (%)"
        )
      ) %>%
      flextable::fontsize(size = 8, part = c("all")) %>%
      flextable::autofit() %>%
      flextable::set_caption("Population characteristics") %>%
      flextable::bold(subject_indexes) %>%
      flextable::padding(i = variable_indexes, j = 1, padding.left = 20) %>%
      flextable::padding(i = category_indexes, j = 1, padding.left = 30) %>%
      flextable::padding(i = nested_variable_indexes, j = 1, padding.left = 40)
  )
}

format_cat_descriptive_data <- function(descriptive_data_row) {
  if(is.na(descriptive_data_row[1, "n"]) | descriptive_data_row[1, "n"] == 0) {
    return("No data")
  }
  
  formatted_n <- format(descriptive_data_row[1, "n"], big.mark = ",")
  formatted_percent <- signif(descriptive_data_row[1, "percent"], digits = 3)*100
  return(paste(formatted_n, "(", formatted_percent, ")"))
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

get_group_by_variable <- function(descriptive_data, group_by) {
  found_rows <- descriptive_data[!is.na(descriptive_data$groupBy_1) &
                                   descriptive_data$groupBy_1 == group_by, ]
  return(unique(found_rows$variable))
}