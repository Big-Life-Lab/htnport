# Set working directory at RDC
setwd("P:/10619/Dropbox/chmsflow")

# Load packages and functions
source("R/installation.R")

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
source("R/medications.R")
source("R/sample.R")
source("R/smoking.R")

source("R/get-descriptive-data.R")
source("R/create-descriptive-table.R")
source("R/impute-variables.R")

# Load metadata
my_variables <- read.csv("P:/10619/Dropbox/chmsflow/worksheets/variables.csv")
my_variable_details <- read.csv("P:/10619/Dropbox/chmsflow/worksheets/variable-details.csv")

# Load medication data - make them have same name as cycle to recode med vars
cycle1 <- read_stata("data/cycle3/cycle1-meds.dta")
cycle2 <- read_stata("data/cycle3/cycle2-meds.dta")
cycle3 <- read_stata("data/cycle3/cycle3-meds.dta")
cycle4 <- read_stata("data/cycle4/cycle4-meds.dta")
cycle5 <- read_stata("data/cycle5/cycle5-meds.dta")
cycle6 <- read_stata("data/cycle6/cycle6-meds.dta")
names(cycle6) <- tolower(names(cycle6)) 

cycle1_prescription_data <- rec_with_table(cycle1, c("clinicid", recodeflow:::select_vars_by_role("Drugs", my_variables)), variable_details = my_variable_details)
cycle2_prescription_data <- rec_with_table(cycle2, c("clinicid", recodeflow:::select_vars_by_role("Drugs", my_variables)), variable_details = my_variable_details)
cycle3_prescription_data <- rec_with_table(cycle3, c("clinicid", "meucatc", "npi_25b", "anymed", "diab_drug"), variable_details = my_variable_details)
cycle4_prescription_data <- rec_with_table(cycle4, c("clinicid", "meucatc", "npi_25b", "anymed", "diab_drug"), variable_details = my_variable_details)
cycle5_prescription_data <- rec_with_table(cycle5, c("clinicid", "meucatc", "npi_25b", "anymed", "diab_drug"), variable_details = my_variable_details)
cycle6_prescription_data <- rec_with_table(cycle6, c("clinicid", "meucatc", "npi_25b", "anymed", "diab_drug"), variable_details = my_variable_details)

# Load overall data with correct names
cycle1 <- read_stata("data/cycle1/cycle1.dta")
cycle2 <- read_stata("data/cycle2/cycle2.dta")
cycle3 <- read_stata("data/cycle3/cycle3.dta")
cycle4 <- read_stata("data/cycle4/cycle4.dta")
cycle5 <- read_stata("data/cycle5/cycle5.dta")
cycle6 <- read_stata("data/cycle6/cycle6.dta")
names(cycle6) <- tolower(names(cycle6)) 

# Assign cycle variable to each cycle for imputation purposes
cycle1$cycle <- 1
cycle2$cycle <- 2
cycle3$cycle <- 3
cycle4$cycle <- 4
cycle5$cycle <- 5
cycle6$cycle <- 6

# Derive outcome medication criterion in medication data of first two cycles and merge that data to overall data
cycle1_prescription_data$anymed2 <- as.numeric(as.character(cycle1_prescription_data$anymed))
cycle2_prescription_data$anymed2 <- as.numeric(as.character(cycle1_prescription_data$anymed))

cycle1_prescription_data$diab_drug2 <- as.numeric(as.character(cycle1_prescription_data$diab_drug))
cycle2_prescription_data$diab_drug2 <- as.numeric(as.character(cycle1_prescription_data$diab_drug))

cycle1_prescription_data <- select(cycle1_prescription_data, clinicid, anymed2, diab_drug2)
cycle2_prescription_data <- select(cycle2_prescription_data, clinicid, anymed2, diab_drug2)

cycle1 <- merge(cycle1, cycle1_prescription_data, by = "clinicid")
cycle2 <- merge(cycle2, cycle2_prescription_data, by = "clinicid")

# Flatten medication data for latter four cycles and merge that data to overall data
process_long_prescription_data <- function(cycle_data, cycle_prescription_data) {
  
  # Step 1: Group and summarize the prescription data
  cycle_prescription_data <- cycle_prescription_data %>%
    group_by(clinicid) %>%
    summarize(
      meucatc = paste(unique(meucatc), collapse = ", "),   # Concatenate unique values of meucatc
      npi_25b = paste(unique(npi_25b), collapse = ", "),   # Concatenate unique values of npi_25b
      anymed = max(as.numeric(as.character(anymed))),      # Find the maximum of anymed
      diab_drug = max(as.numeric(as.character(diab_drug))) # Find the maximum of diab_drug
    )
  
  # Step 2: Merge the summarized data back to the original cycle_data
  cycle_data <- merge(cycle_data, cycle_prescription_data, by = "clinicid")
  
  # Step 3: Create 'anymed2' and 'diab_drug2', and clean up columns
  cycle_data <- cycle_data %>%
    mutate(anymed2 = pmax(anymed.x, anymed.y, na.rm = TRUE)) %>%
    mutate(diab_drug2 = pmax(diab_drug.x, diab_drug.y, na.rm = TRUE)) %>%
    select(-c(anymed.x, anymed.y, diab_drug.x, diab_drug.y))
  
  return(cycle_data)
}

cycle3 <- process_long_prescription_data(cycle3, cycle3_prescription_data)
cycle4 <- process_long_prescription_data(cycle4, cycle4_prescription_data)
cycle5 <- process_long_prescription_data(cycle5, cycle5_prescription_data)
cycle6 <- process_long_prescription_data(cycle6, cycle6_prescription_data)

# Recode variables, combine cycles, and obtain sample
cycle1_data <- recodeflow::rec_with_table(cycle1, recodeflow:::select_vars_by_role(c("Recode", "Fam"), my_variables), variable_details = my_variable_details, log = TRUE)
cycle2_data <- recodeflow::rec_with_table(cycle2, recodeflow:::select_vars_by_role(c("Recode", "Fam"), my_variables), variable_details = my_variable_details, log = TRUE)
cycle3_data <- recodeflow::rec_with_table(cycle3, recodeflow:::select_vars_by_role(c("Recode", "Fam"), my_variables), variable_details = my_variable_details, log = TRUE)
cycle4_data <- recodeflow::rec_with_table(cycle4, recodeflow:::select_vars_by_role(c("Recode", "Fam"), my_variables), variable_details = my_variable_details, log = TRUE)
cycle5_data <- recodeflow::rec_with_table(cycle5, recodeflow:::select_vars_by_role(c("Recode"), my_variables), variable_details = my_variable_details, log = TRUE)
cycle6_data <- recodeflow::rec_with_table(cycle6, recodeflow:::select_vars_by_role(c("Recode"), my_variables), variable_details = my_variable_details, log = TRUE)

cycles1to6_data <- cchsflow::merge_rec_data(cycle1_data, cycle2_data, cycle3_data, cycle4_data, cycle5_data, cycle6_data)
cycles1to6_data <- dplyr::filter(cycles1to6_data, insample == 1)

# Load and merge extra accelerometer data to sample data
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
  select(c(wgt_full, clc_sex, recodeflow:::select_vars_by_role(c("imputation-predictor"), my_variables)))

# Ensure derived categorical variables present in all six cycles can be properly tabulated
recode_na_b <- function(column) {
  # Convert the column to character if it's a factor
  if (is.factor(column)) {
    column <- as.character(column)
  }
  # Replace "NA(c)" with "NA(b)"
  column[column == "NA(c)"] <- "NA(b)"
  return(column)
}

cycles1to6_data$ckd <- recode_na_b(cycles1to6_data$ckd)
cycles1to6_data$low_drink_score1 <- recode_na_b(cycles1to6_data$low_drink_score1)
cycles1to6_data$working <- recode_na_b(cycles1to6_data$working)

# Generate unimputed Table 1
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

# Repeat combination of cycles, obtaining of sample, and merging of extra accelerometer data
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
  select(c(wgt_full, clc_sex, recodeflow:::select_vars_by_role(c("imputation-predictor"), my_variables), ccc_61, ccc_63, ccc_81, cardiov, anymed2, ccc_32))

# Impute data
set.seed(123)
imputed_cycles1to6_data <- impute_variables(cycles1to6_data, outcomes = recodeflow:::select_vars_by_role(c("Predictor"), my_variables), recodeflow:::select_vars_by_role(c("imputation-predictor"), my_variables))

# Generate imputed Table 1
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