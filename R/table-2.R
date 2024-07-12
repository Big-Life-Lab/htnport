setwd("P:/10619/Dropbox/chmsflow")

source("R/table-1.R")

table2_data <- get_descriptive_data(
  imputed_cycles1to6_data,
  my_variables,
  my_variable_details,
  # All the variables whose descriptive statistics we want
  "highbp14090_adj",
  # Sets the stratifier
  list("all" = list("clc_sex"))
)

create_descriptive_table(
  table2_data,
  my_variables,
  my_variable_details,
  "highbp14090_adj",
  column_stratifier = c("clc_sex")
)

table2b_data <- get_descriptive_data(
  imputed_cycles1to6_data,
  my_variables,
  my_variable_details,
  # All the variables whose descriptive statistics we want
  recodeflow:::select_vars_by_role(
    c("Predictor"),
    my_variables
  ),
  # Sets the stratifier
  list("all" = list("highbp14090_adj"))
)

create_descriptive_table(
  table2b_data,
  my_variables,
  my_variable_details,
  recodeflow:::select_vars_by_role(
    c("Predictor"),
    my_variables
  ),
  column_stratifier = c("highbp14090_adj"),
  subjects_order = c("Age", "Marital status", "Education", "Occupation", "Family history", "Exercise", "Diet", "Weight", "Chronic disease", "Alcohol", "Smoking", "Sleep", "General")
)