setwd("P:/10619/Dropbox/chmsflow")

library(recodeflow)
library(cchsflow)
library(dplyr)
library(readr)
library(haven)
library(cli)
library(mice)
library(broom)

source("R/alcohol.R")
source("R/blood-pressure.R")
source("R/cholesterol-and-obesity.R")
source("R/diabetes.R")
source("R/diet.R")
source("R/exercise.R")
source("R/family-history.R")
source("R/income.R")
source("R/kidney.R")
source("R/sample.R")
source("R/smoking.R")
source("R/working.R")

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

cycle1$cycle <- 1
cycle2$cycle <- 2
cycle3$cycle <- 3
cycle4$cycle <- 4
cycle5$cycle <- 5
cycle6$cycle <- 6

cycle1_data <- recodeflow::rec_with_table(cycle1, recodeflow:::select_vars_by_role(c("Recode", "Fam"), my_variables), variable_details = my_variable_details, log = TRUE)
cycle2_data <- recodeflow::rec_with_table(cycle2, recodeflow:::select_vars_by_role(c("Recode", "Fam"), my_variables), variable_details = my_variable_details, log = TRUE)
cycle3_data <- recodeflow::rec_with_table(cycle3, recodeflow:::select_vars_by_role(c("Recode", "Fam"), my_variables), variable_details = my_variable_details, log = TRUE)
cycle4_data <- recodeflow::rec_with_table(cycle4, recodeflow:::select_vars_by_role(c("Recode", "Fam"), my_variables), variable_details = my_variable_details, log = TRUE)
cycle5_data <- recodeflow::rec_with_table(cycle5, recodeflow:::select_vars_by_role(c("Recode"), my_variables), variable_details = my_variable_details, log = TRUE)
cycle6_data <- recodeflow::rec_with_table(cycle6, recodeflow:::select_vars_by_role(c("Recode"), my_variables), variable_details = my_variable_details, log = TRUE)

cycles1to6_data <- cchsflow::merge_rec_data(cycle1_data, cycle2_data, cycle3_data, cycle4_data, cycle5_data, cycle6_data)
cycles1to6_data <- dplyr::filter(cycles1to6_data, insample == 1)

tracey <- read_sas("data/From Tracy/CHMS_AM_validdays_1to3.sas7bdat")
tracey$clinicid <-tracey$CLINICID
tracey$mvpa_min <- tracey$avg_mvpa
tracey$minperweek <- minperday_to_minperweek(tracey$mvpa_min)
tracey <- subset(tracey, select = -c(CLINICID, avg_mvpa, adultPAG))

cycles1to6_data <- dplyr::left_join(cycles1to6_data, tracey, by = c("clinicid"))
cycles1to6_data <- cycles1to6_data %>%
  mutate(mvpa_min = coalesce(mvpa_min.x, mvpa_min.y),
         minperweek = coalesce(minperweek.x, minperweek.y)) %>%
  mutate(mvpa_min = ifelse(is.na(mvpa_min), haven::tagged_na("b"), mvpa_min),
         minperweek = ifelse(is.na(minperweek), haven::tagged_na("b"), minperweek)) %>%
  select(-c(mvpa_min.x, mvpa_min.y, minperweek.x, minperweek.y))

recode_na_b <- function(column) {
  # Convert the column to character if it's a factor
  if (is.factor(column)) {
    column <- as.character(column)
  }
  # Replace NAs with "NA(b)"
  column[column == "NA(c)"] <- "NA(b)"
  return(column)
}

cycles1to6_data$ckd <- recode_na_b(cycles1to6_data$ckd)
cycles1to6_data$low_drink_score1 <- recode_na_b(cycles1to6_data$low_drink_score1)
cycles1to6_data$working <- recode_na_b(cycles1to6_data$working)

table1_data <- get_descriptive_data(
  cycles1to6_data,
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
  table1_data,
  my_variables,
  my_variable_details,
  recodeflow:::select_vars_by_role(
    c("Table 1"),
    my_variables
  ),
  column_stratifier = c("clc_sex"),
  subjects_order = c("Age", "Sex", "Marital status", "Education", "Occupation", "Family history", "Exercise", "Diet", "Weight", "Chronic disease", "Alcohol", "Smoking", "Sleep", "General")
)

cycles1to6_data$ckd[cycles1to6_data$ckd %in% c("NA(a)", "NA(b)", "NA(c)")] <- NA
cycles1to6_data$low_drink_score1[cycles1to6_data$low_drink_score1 %in% c("NA(a)", "NA(b)", "NA(c)")] <- NA
cycles1to6_data$working[cycles1to6_data$working %in% c("NA(a)", "NA(b)", "NA(c)")] <- NA
  
imputed_cycles1to6_data <- impute_variables(cycles1to6_data, recodeflow:::select_vars_by_role(c("Predictor"), my_variables), recodeflow:::select_vars_by_role(c("imputation-predictor"), my_variables))

imputed_table1_data <- get_descriptive_data(
  imputed_cycles1to6_data,
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
  imputed_table1_data,
  my_variables,
  my_variable_details,
  recodeflow:::select_vars_by_role(
    c("Table 1"),
    my_variables
  ),
  column_stratifier = c("clc_sex"),
  subjects_order = c("Age", "Sex", "Marital status", "Education", "Occupation", "Family history", "Exercise", "Diet", "Weight", "Chronic disease", "Alcohol", "Smoking", "Sleep", "General")
)