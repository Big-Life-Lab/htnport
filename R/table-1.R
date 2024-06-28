setwd("P:/10619/Dropbox/chmsflow")

library(recodeflow)
library(cchsflow)
library(dplyr)
library(readr)
library(haven)
library(cli)
library(pastecs)
library(mice)

source("R/alcohol.R")
source("R/blood-pressure.R")
source("R/diabetes.R")
source("R/diet.R")
source("R/exercise.R")
source("R/family-history.R")
source("R/income.R")
source("R/kidney.R")
source("R/sample.R")
source("R/smoking.R")

source("R/get-descriptive-data.R")
source("R/create-descriptive-table.R")
source("R/impute-variables.R")

my_variables <- read.csv("P:/10619/Dropbox/chmsflow/worksheets/variables.csv")
my_variable_details <- read.csv("P:/10619/Dropbox/chmsflow/worksheets/variable-details.csv")

cycle1 <- read_stata("data/cycle1/cycle1.dta")
cycle2 <- read_stata("data/cycle2/cycle2.dta")
cycle3 <- read_stata("data/cycle3/cycle3.dta")
cycle4 <- read_stata("data/cycle4/cycle4.dta")
cycle5 <- read_stata("data/cycle5/cycle5.dta")
cycle6 <- read_stata("data/cycle6/cycle6.dta")
names(cycle6) <- tolower(names(cycle6)) 

cycle1_table1_data <- recodeflow::rec_with_table(cycle1, recodeflow:::select_vars_by_role(c("Recode", "Fam"), my_variables), variable_details = my_variable_details, log = TRUE)
cycle2_table1_data <- recodeflow::rec_with_table(cycle2, recodeflow:::select_vars_by_role(c("Recode", "Fam"), my_variables), variable_details = my_variable_details, log = TRUE)
cycle3_table1_data <- recodeflow::rec_with_table(cycle3, recodeflow:::select_vars_by_role(c("Recode", "Fam"), my_variables), variable_details = my_variable_details, log = TRUE)
cycle4_table1_data <- recodeflow::rec_with_table(cycle4, recodeflow:::select_vars_by_role(c("Recode", "Fam"), my_variables), variable_details = my_variable_details, log = TRUE)
cycle5_table1_data <- recodeflow::rec_with_table(cycle5, recodeflow:::select_vars_by_role(c("Recode"), my_variables), variable_details = my_variable_details, log = TRUE)
cycle6_table1_data <- recodeflow::rec_with_table(cycle6, recodeflow:::select_vars_by_role(c("Recode"), my_variables), variable_details = my_variable_details, log = TRUE)

cycles1to6_table1_data <- cchsflow::merge_rec_data(cycle1_table1_data, cycle2_table1_data, cycle3_table1_data, cycle4_table1_data, cycle5_table1_data, cycle6_table1_data)
cycles1to6_table1_data <- dplyr::filter(cycles1to6_table1_data, insample == 1)

recode_na_b <- function(column) {
  # Convert the column to character if it's a factor
  if (is.factor(column)) {
    column <- as.character(column)
  }
  # Replace NAs with "NA(b)"
  column[column == "NA(c)"] <- "NA(b)"
  return(column)
}

cycles1to6_table1_data$ckd <- recode_na_b(cycles1to6_table1_data$ckd)
cycles1to6_table1_data$highbp14090 <- recode_na_b(cycles1to6_table1_data$highbp14090)
cycles1to6_table1_data$low_drink_score1 <- recode_na_b(cycles1to6_table1_data$low_drink_score1)
cycles1to6_table1_data$mvpa150wk <- recode_na_b(cycles1to6_table1_data$mvpa150wk)
cycles1to6_table1_data$poordiet <- recode_na_b(cycles1to6_table1_data$poordiet)

sex_stratified_huiport_table1_data <- get_descriptive_data(
  cycles1to6_table1_data,
  my_variables,
  my_variable_details,
  # All the variables whose descriptive statistics we want
  recodeflow:::select_vars_by_role(
    c("Table 1"),
    my_variables
  ),
  # Sets the stratifier
  list("all" = list("clc_sex"))
)

create_descriptive_table(
  sex_stratified_huiport_table1_data,
  my_variables,
  my_variable_details,
  recodeflow:::select_vars_by_role(
    c("Table 1"),
    my_variables
  ),
  column_stratifier = c("clc_sex"),
  subjects_order = c("Age", "Sex", "Marital status", "Education", "Occupation", "Family history", "Exercise", "Diet", "Weight", "Chronic disease", "Alcohol", "Smoking", "Sleep", "General")
)

# cycles1to6_table1_data$ckd[cycles1to6_table1_data$ckd %in% c("NA(a)", "NA(b)", "NA(c)")] <- NA
# cycles1to6_table1_data$highbp14090[cycles1to6_table1_data$highbp14090 %in% c("NA(a)", "NA(b)", "NA(c)")] <- NA
# cycles1to6_table1_data$low_drink_score1[cycles1to6_table1_data$low_drink_score1 %in% c("NA(a)", "NA(b)", "NA(c)")] <- NA
# cycles1to6_table1_data$mvpa150wk[cycles1to6_table1_data$mvpa150wk %in% c("NA(a)", "NA(b)", "NA(c)")] <- NA
# cycles1to6_table1_data$poordiet[cycles1to6_table1_data$poordiet %in% c("NA(a)", "NA(b)", "NA(c)")] <- NA

imputed_cycles1to6_table1_data <- impute_variables(cycles1to6_table1_data, recodeflow:::select_vars_by_role(c("Table 1"), my_variables), c("cycle"))

imputed_sex_stratified_huiport_table1_data <- get_descriptive_data(
  imputed_cycles1to6_table1_data,
  my_variables,
  my_variable_details,
  # All the variables whose descriptive statistics we want
  recodeflow:::select_vars_by_role(
    c("Table 1"),
    my_variables
  ),
  # Sets the stratifier
  list("all" = list("clc_sex"))
)

create_descriptive_table(
  imputed_sex_stratified_huiport_table1_data,
  my_variables,
  my_variable_details,
  recodeflow:::select_vars_by_role(
    c("Table 1"),
    my_variables
  ),
  column_stratifier = c("clc_sex"),
  subjects_order = c("Age", "Sex", "Marital status", "Education", "Occupation", "Family history", "Exercise", "Diet", "Weight", "Chronic disease", "Alcohol", "Smoking", "Sleep", "General")
)