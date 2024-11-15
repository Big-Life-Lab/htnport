library(tidyr)

source("R/variables-sheet-utils.R")
source("R/variable-details-sheet-utils.R")

get_descriptive_data <- function(
  data,
  variables_sheet,
  variables_details_sheet,
  variables,
  stratify_config
) {
  descriptive_data <- data.frame(
    variable = c(),
    cat = c(),
    median = c(),
    percentile25 = c(),
    percentile75 = c(),
    min = c(),
    max = c(),
    n = c(),
    percent = c()
  )
  
  largest_num_stratifiers <- 0
  for(variable in names(stratify_config)) {
    for(stratify_config_for_variable_index in seq_len(length(stratify_config[[variable]]))) {
      current_num_stratifiers <- length(stratify_config[[variable]][[stratify_config_for_variable_index]])
      if(!is.null(stratify_config[["all"]])) {
        current_num_stratifiers <- current_num_stratifiers + length(
          stratify_config[["all"]]
        )
      }
      if(largest_num_stratifiers < current_num_stratifiers) {
        largest_num_stratifiers <- current_num_stratifiers
      } 
    }
  }
  
  for(variable in variables) {
    variable_sheet_row <- variables_sheet[variables_sheet$variable == variable, ]
    
    if(is_continuous_variable(variable_sheet_row)) {
      map_stratifier_data(
        data,
        variables_sheet,
        variables_details_sheet,
        variable,
        stratify_config,
        function(current_stratifier_info) {
          new_descriptive_data_row <- data.frame(
            variable = c(variable),
            cat = c(NA),
            median = c(NA),
            percentile25 = c(NA),
            percentile75 = c(NA),
            min = c(NA),
            max = c(NA),
            n = c(NA),
            percent = c(NA)
          )
          
          for(stratifier_index in seq_len(largest_num_stratifiers)) {
            new_descriptive_data_row[[
              paste(
                "groupBy_",
                stratifier_index,
                sep = ""
              )
            ]] <- c(NA)
            new_descriptive_data_row[[
              paste(
                "groupByValue_",
                stratifier_index,
                sep = ""
              )
            ]] <- c(NA)
          }
          for(stratifier_index in seq_len(length(current_stratifier_info$stratifiers))) {
            stratifier <- current_stratifier_info$stratifiers[[stratifier_index]]
            new_descriptive_data_row[[
              paste(
                "groupBy_",
                stratifier_index,
                sep = ""
              )
            ]] <- c(stratifier)
            new_descriptive_data_row[[
              paste(
                "groupByValue_",
                stratifier_index,
                sep = ""
              )
            ]] <- c(
              current_stratifier_info$stratifier_combination[1, stratifier]
            ) 
          }
          
          data_for_new_descriptive_data_row <- current_stratifier_info$data[[variable]][
            !is.na(current_stratifier_info$data[[variable]])
          ]
          data_for_new_descriptive_data_row_summary <- summary(
            data_for_new_descriptive_data_row
          )
          
          new_descriptive_data_row$median <- data_for_new_descriptive_data_row_summary[[3]]
          new_descriptive_data_row$percentile25 <- data_for_new_descriptive_data_row_summary[[2]]
          new_descriptive_data_row$percentile75 <- data_for_new_descriptive_data_row_summary[[5]]
          new_descriptive_data_row$min <- data_for_new_descriptive_data_row_summary[[1]]
          new_descriptive_data_row$max <- data_for_new_descriptive_data_row_summary[[6]]
          new_descriptive_data_row$n <- length(data_for_new_descriptive_data_row)
          
          descriptive_data <<- rbind(
            descriptive_data,
            new_descriptive_data_row
          )
        }
      )
    }
    
    for(na_type in c("NA::a", "NA::b", "NA::c")) {
      tagged_na_type <- "a"
      if(na_type == "NA::b") {
        tagged_na_type <- "b"
      } else if(na_type == "NA::c") {
        tagged_na_type <- "c"
      }
      
      map_stratifier_data(
        data,
        variables_sheet,
        variables_details_sheet,
        variable,
        stratify_config,
        function(current_stratifier_info) {
          new_descriptive_data_row <- data.frame(
            variable = c(variable),
            cat = na_type,
            median = c(NA),
            percentile25 = c(NA),
            percentile75 = c(NA),
            min = c(NA),
            max = c(NA)
          )
          
          for(stratifier_index in seq_len(largest_num_stratifiers)) {
            new_descriptive_data_row[[
              paste(
                "groupBy_",
                stratifier_index,
                sep = ""
              )
            ]] <- c(NA)
            new_descriptive_data_row[[
              paste(
                "groupByValue_",
                stratifier_index,
                sep = ""
              )
            ]] <- c(NA)
          }
          for(stratifier_index in seq_len(length(current_stratifier_info$stratifiers))) {
            stratifier <- current_stratifier_info$stratifiers[[stratifier_index]]
            new_descriptive_data_row[[
              paste(
                "groupBy_",
                stratifier_index,
                sep = ""
              )
            ]] <- c(stratifier)
            new_descriptive_data_row[[
              paste(
                "groupByValue_",
                stratifier_index,
                sep = ""
              )
            ]] <- c(
              current_stratifier_info$stratifier_combination[1, stratifier]
            ) 
          }
          
          data_for_new_descriptive_data_row <- dplyr::filter(
            current_stratifier_info$data,
            haven::is_tagged_na(
              .data[[variable]], 
              tagged_na_type
            ) | .data[[variable]] == paste("NA(", tagged_na_type, ")", sep = "")
          )
                    
          new_descriptive_data_row$n <- c(
            nrow(data_for_new_descriptive_data_row)
          )
          new_descriptive_data_row$percent <- c(
            new_descriptive_data_row$n[1]/nrow(current_stratifier_info$data)
          )
          
          descriptive_data <<- rbind(
            descriptive_data,
            new_descriptive_data_row
          )
        }
      )
    }
    
    if(is_categorical_variable(variable_sheet_row)) {
      variable_detail_rows <- get_unique_rec_end_rows(variables_details_sheet, variable, FALSE)
      for(variable_details_row_index in seq_len(nrow(variable_detail_rows))) {
        current_rec_end <- variable_detail_rows[variable_details_row_index, "recEnd"]
        if(is_categorical_variable(variable_sheet_row)) {
          map_stratifier_data(
            data,
            variables_sheet,
            variables_details_sheet,
            variable,
            stratify_config,
            function(current_stratifier_info) {
              new_descriptive_data_row <- data.frame(
                variable = c(variable),
                median = c(NA),
                percentile25 = c(NA),
                percentile75 = c(NA),
                min = c(NA),
                max = c(NA),
                cat = c(variable_detail_rows[variable_details_row_index, "recEnd"])
              )
              
              for(stratifier_index in seq_len(largest_num_stratifiers)) {
                new_descriptive_data_row[[
                  paste(
                    "groupBy_",
                    stratifier_index,
                    sep = ""
                  )
                ]] <- c(NA)
                new_descriptive_data_row[[
                  paste(
                    "groupByValue_",
                    stratifier_index,
                    sep = ""
                  )
                ]] <- c(NA)
              }
              for(stratifier_index in seq_len(length(current_stratifier_info$stratifiers))) {
                stratifier <- current_stratifier_info$stratifiers[[stratifier_index]]
                new_descriptive_data_row[[
                  paste(
                    "groupBy_",
                    stratifier_index,
                    sep = ""
                  )
                ]] <- c(stratifier)
                new_descriptive_data_row[[
                  paste(
                    "groupByValue_",
                    stratifier_index,
                    sep = ""
                  )
                ]] <- c(
                  current_stratifier_info$stratifier_combination[1, stratifier]
                ) 
              }
              
              data_for_new_descriptive_data_row <- dplyr::filter(
                current_stratifier_info$data,
                .data[[variable]] == current_rec_end
              )
              
              new_descriptive_data_row$n <- c(nrow(data_for_new_descriptive_data_row))
              new_descriptive_data_row$percent <- c(
                new_descriptive_data_row$n[1]/nrow(current_stratifier_info$data)
              )
              
              descriptive_data <<- rbind(
                descriptive_data,
                new_descriptive_data_row
              )
            }
          )
        }
      }
    }
  }
  
  return(descriptive_data)
}

map_stratifier_data <- function(
  data,
  variables_sheet,
  variables_details_sheet,
  variable,
  stratify_config,
  iterator
) {
  stratifier_config_for_variable <- NA
  if(!is.null(stratify_config[[variable]])) {
    stratifier_config_for_variable <- stratify_config[[variable]]
  }
  if(!is.null(stratify_config[["all"]])) {
    if(is.na(stratifier_config_for_variable)) {
      stratifier_config_for_variable <- list(stratify_config[["all"]])
    }
    else {
      for(stratifiers_index in seq_len(length(stratifier_config_for_variable))) {
        stratifier_config_for_variable[[stratifiers_index]] <- append(
          stratifier_config_for_variable[[stratifiers_index]],
          stratify_config[["all"]],
          0
        )
      }
    }
  }
  
  if(is.na(stratifier_config_for_variable)) {
    return(iterator(
      list(
        data = data,
        stratifiers = list(),
        stratifier_combination = data.frame()
      )
    ))
  }
  
  for(stratifiers in stratifier_config_for_variable) {
    expand_grid_arguments <- list()
    for(stratifier in stratifiers) {
      stratifier_variables_sheet_row <- get_row_for_variable(
        stratifier,
        variables_sheet
      )
      if(is_continuous_variable(stratifier_variables_sheet_row)) {
        stop(paste(
          "Stratifier",
          stratifier,
          "for variable",
          variable,
          "is continuous which is currently not supported"
        ))
      }
      
      expand_grid_arguments[[stratifier]] <- get_unique_rec_end_rows(
        variables_details_sheet,
        stratifier,
        include_NA = TRUE
      )$recEnd
    }
    all_stratifier_combinations <- do.call(
      tidyr::expand_grid,
      expand_grid_arguments
    )
    
    for(stratifier_combination_index in seq_len(nrow(all_stratifier_combinations))) {
      data_for_current_stratifier_combination <- data
      stratifier_combination <- all_stratifier_combinations[stratifier_combination_index, ]
      for(stratifier_index in seq_len(length(stratifiers))) {
        stratifier <- stratifiers[[stratifier_index]]
        stratifier_category <- stratifier_combination[1, ][[stratifier]]
        formatted_stratifier_category <- stratifier_category
        # TODO Refactor this
        if(stratifier_category == "NA::a") {
          formatted_stratifier_category <- "NA(a)"
        } else if(stratifier_category == "NA::b") {
          formatted_stratifier_category <- "NA(b)"
        }
      
        data_for_current_stratifier_combination <- dplyr::filter(
          data_for_current_stratifier_combination,
          !!as.symbol(stratifier) == formatted_stratifier_category
        )
      }
    
      iterator(
        list(
          data = data_for_current_stratifier_combination,
          stratifiers = stratifiers,
          stratifier_combination = stratifier_combination
        )
      )
    } 
  }
}
