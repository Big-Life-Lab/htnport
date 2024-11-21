# Set working directory at RDC
setwd("P:/10619/Dropbox/htnport")

# Load packages and functions
.libPaths("S:\\_R4.2.3packages_incomplete")
library(dplyr)
library(recodeflow)
library(survey)
library(gtsummary)

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

# Load medication data
cycle1_meds <- read_sas("data/cycle1/cycle1-meds.sas7bdat")
names(cycle1_meds) <- tolower(names(cycle1_meds)) 
cycle2_meds <- read_sas("data/cycle2/cycle2-meds.sas7bdat")
cycle3_meds <- read_stata("data/cycle3/cycle3-meds.dta")
cycle4_meds <- read_stata("data/cycle4/cycle4-meds.dta")
names(cycle4_meds) <- tolower(names(cycle4_meds)) 
cycle5_meds <- read_stata("data/cycle5/cycle5-meds.dta")
cycle6_meds <- read_stata("data/cycle6/cycle6-meds.dta")
names(cycle6_meds) <- tolower(names(cycle6_meds)) 

cycle1_medication_data <- recodeflow::rec_with_table(cycle1_meds, c("clinicid", recodeflow:::select_vars_by_role("Drugs", my_variables)), variable_details = my_variable_details)
cycle2_medication_data <- recodeflow::rec_with_table(cycle2_meds, c("clinicid", recodeflow:::select_vars_by_role("Drugs", my_variables)), variable_details = my_variable_details)
cycle3_medication_data <- recodeflow::rec_with_table(cycle3_meds, c("clinicid", "meucatc", "npi_25b", "anymed", "diab_drug"), variable_details = my_variable_details)
cycle4_medication_data <- recodeflow::rec_with_table(cycle4_meds, c("clinicid", "meucatc", "npi_25b", "anymed", "diab_drug"), variable_details = my_variable_details)
cycle5_medication_data <- recodeflow::rec_with_table(cycle5_meds, c("clinicid", "meucatc", "npi_25b", "anymed", "diab_drug"), variable_details = my_variable_details)
cycle6_medication_data <- recodeflow::rec_with_table(cycle6_meds, c("clinicid", "meucatc", "npi_25b", "anymed", "diab_drug"), variable_details = my_variable_details)

# Load overall cycle data
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

# Derive outcome medication criterion in medication data of first two cycles, and merge that data to overall cycle data
cycle1_medication_data$anymed2 <- as.numeric(as.character(cycle1_medication_data$anymed))
cycle2_medication_data$anymed2 <- as.numeric(as.character(cycle2_medication_data$anymed))

cycle1_medication_data$diab_drug2 <- as.numeric(as.character(cycle1_medication_data$diab_drug))
cycle2_medication_data$diab_drug2 <- as.numeric(as.character(cycle2_medication_data$diab_drug))

cycle1_medication_data <- dplyr::select(cycle1_medication_data, clinicid, anymed2, diab_drug2)
cycle2_medication_data <- dplyr::select(cycle2_medication_data, clinicid, anymed2, diab_drug2)

cycle1 <- merge(cycle1, cycle1_medication_data, by = "clinicid")
cycle2 <- merge(cycle2, cycle2_medication_data, by = "clinicid")

# Flatten medication data for latter four cycles and merge that data to overall cycle data
process_long_medication_data <- function(cycle_data, cycle_medication_data) {
  
  # Step 1: Group and summarize the medication data
  cycle_medication_data <- cycle_medication_data %>%
    dplyr::group_by(clinicid) %>%
    dplyr::summarize(
      meucatc = paste(unique(meucatc), collapse = ", "),   # Concatenate unique values of meucatc
      npi_25b = paste(unique(npi_25b), collapse = ", "),   # Concatenate unique values of npi_25b
      anymed = max(as.numeric(as.character(anymed))),      # Find the maximum of anymed
      diab_drug = max(as.numeric(as.character(diab_drug))) # Find the maximum of diab_drug
    )
  
  # Step 2: Merge the summarized data back to the original cycle_data
  cycle_data <- dplyr::left_join(cycle_data, cycle_medication_data, by = c("clinicid"))
  
  # Step 3: Create 'anymed2' and 'diab_drug2', and clean up columns
  cycle_data <- cycle_data %>%
    dplyr::mutate(anymed2 = anymed) %>%
    dplyr::mutate(diab_drug2 = diab_drug) %>%
    dplyr::select(-c(anymed, diab_drug))
  
  return(cycle_data)
}

cycle3 <- process_long_medication_data(cycle3, cycle3_medication_data)
cycle4 <- process_long_medication_data(cycle4, cycle4_medication_data)
cycle5 <- process_long_medication_data(cycle5, cycle5_medication_data)
cycle6 <- process_long_medication_data(cycle6, cycle6_medication_data)

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
  dplyr::mutate(mvpa_min = coalesce(mvpa_min.x, mvpa_min.y),
         minperweek = coalesce(minperweek.x, minperweek.y)) %>%
  dplyr::mutate(mvpa_min = ifelse(is.na(mvpa_min), haven::tagged_na("b"), mvpa_min),
         minperweek = ifelse(is.na(minperweek), haven::tagged_na("b"), minperweek)) %>%
  dplyr::select(c(wgt_full, clc_sex, recodeflow:::select_vars_by_role(c("imputation-predictor"), my_variables)))

# # Ensure derived categorical variables present in all six cycles can be properly tabulated
# recode_na_b <- function(column) {
#   # Convert the column to character if it's a factor
#   if (is.factor(column)) {
#     column <- as.character(column)
#   }
#   # Replace "NA(c)" with "NA(b)"
#   column[column == "NA(c)"] <- "NA(b)"
#   return(column)
# }
# 
# cycles1to6_data$ckd <- recode_na_b(cycles1to6_data$ckd)
# cycles1to6_data$diabx <- recode_na_b(cycles1to6_data$diabx)
# cycles1to6_data$low_drink_score1 <- recode_na_b(cycles1to6_data$low_drink_score1)
# cycles1to6_data$working <- recode_na_b(cycles1to6_data$working)
# 
# # Generate unimputed Table 1
# table1_data <- get_descriptive_data(
#   cycles1to6_data,
#   my_variables,
#   my_variable_details,
#   # All the variables whose descriptive statistics we want
#   recodeflow:::select_vars_by_role(
#     c("Table 1"),
#     my_variables
#   ),
#   # Sets the stratifier
#   list("all" = list("clc_sex"))
# )
# 
# create_descriptive_table(
#   table1_data,
#   my_variables,
#   my_variable_details,
#   recodeflow:::select_vars_by_role(
#     c("Table 1"),
#     my_variables
#   ),
#   column_stratifier = c("clc_sex"),
#   subjects_order = c("Age", "Sex", "Marital status", "Education", "Occupation", "Family history", "Exercise", "Diet", "Weight", "Chronic disease", "Alcohol", "Smoking", "Sleep", "General")
# )

# Generate unimputed and weighted Table 1
cycles1to6_data <- cycles1to6_data %>%
  dplyr::rename(
    'Age' = clc_age,
    `Marital status` = married,
    `Highest education level` = edudr04,
    `Working status` = working,
    `Hypertension family history` = fmh_15,
    `Minutes of exercise per week` = minperweek,
    `Daily fruit and vegetable consumption` = totalfv,
    `Body mass index` = hwmdbmi,
    `Waist-to-height ratio` = whr,
    `Chronic kidney disease` = ckd,
    'Diabetes' = diabx,
    `Alcohol consumption level` = low_drink_score1,
    `Smoking status` = smoke,
    `Hours of sleep per day` = slp_11,
    `Self-rated mental health` = gendmhi,
    'Stress' = gen_025,
    `Sense of belonging` = gen_045
  )

weighted_data <- survey::svydesign(
  id = ~1,
  weights = ~wgt_full,
  data = cycles1to6_data 
)

gtsummary::tbl_svysummary(
  data = weighted_data, 
  by = clc_sex,
  include = -wgt_full,
  statistic = list(
    all_continuous() ~ "{median} ({p25}, {p75})",
    all_categorical() ~ "{n_unweighted} ({p}%)"
  ),
  missing = "ifany"
) %>%
  # Rename columns
  gtsummary::modify_header(label = "**Characteristic**") %>%
  gtsummary::modify_spanning_header(
    all_stat_cols() ~ "**Sex**"
  ) %>%
  gtsummary::modify_header(
    stat_1 = "**Male**",
    stat_2 = "**Female**"
  ) %>%
  # Rename variable names and category labels
  gtsummary::modify_table_body(
    ~ .x %>%
      dplyr::mutate(
        label = dplyr::case_when(
          variable == "Marital status" & label == "1" ~ "Married or common-law",
          variable == "Marital status" & label == "2" ~ "Widowed, separated, or divorced",
          variable == "Marital status" & label == "3" ~ "Single",
          variable == "Highest education level" & label == "1" ~ "No high school",
          variable == "Highest education level" & label == "2" ~ "Secondary",
          variable == "Highest education level" & label == "3" ~ "Post-secondary",
          variable == "Working status" & label == "1" ~ "Has a job",
          variable == "Working status" & label == "2" ~ "Does not have a job",
          variable == "Hypertension family history" & label == "1" ~ "Yes",
          variable == "Hypertension family history" & label == "2" ~ "No",
          variable == "Chronic kidney disease" & label == "1" ~ "Yes",
          variable == "Chronic kidney disease" & label == "2" ~ "No",
          variable == "Diabetes" & label == "1" ~ "Yes",
          variable == "Diabetes" & label == "2" ~ "No",
          variable == "Alcohol consumption level" & label == "1" ~ "Never drinker",
          variable == "Alcohol consumption level" & label == "2" ~ "Former drinker",
          variable == "Alcohol consumption level" & label == "3" ~ "Light drinker",
          variable == "Alcohol consumption level" & label == "4" ~ "Moderate to heavy drinker",
          variable == "Smoking status" & label == "1" ~ "Current smoker",
          variable == "Smoking status" & label == "2" ~ "Former smoker",
          variable == "Smoking status" & label == "3" ~ "Never smoker",
          variable == "Self-rated mental health" & label == "1" ~ "Poor",
          variable == "Self-rated mental health" & label == "2" ~ "Fair or good",
          variable == "Self-rated mental health" & label == "3" ~ "Very good or excellent",
          variable == "Stress" & label == "1" ~ "Not at all to a bit",
          variable == "Stress" & label == "2" ~ "Quite a bit or extremely",
          variable == "Sense of belonging" & label == "1" ~ "Strong",
          variable == "Sense of belonging" & label == "2" ~ "Weak",
          TRUE ~ label
        )
      )
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
  dplyr::mutate(mvpa_min = coalesce(mvpa_min.x, mvpa_min.y),
         minperweek = coalesce(minperweek.x, minperweek.y)) %>%
  dplyr::mutate(mvpa_min = ifelse(is.na(mvpa_min), haven::tagged_na("b"), mvpa_min),
         minperweek = ifelse(is.na(minperweek), haven::tagged_na("b"), minperweek)) %>%
  dplyr::select(c(wgt_full, clc_sex, recodeflow:::select_vars_by_role(c("imputation-predictor"), my_variables)), cardiov, anymed2, ccc_32)

# Impute data
set.seed(123)
imputed_cycles1to6_data <- impute_variables(dplyr::select(cycles1to6_data, -c(cardiov, anymed2, ccc_32)), outcomes = recodeflow:::select_vars_by_role(c("Predictor"), my_variables), predictors = recodeflow:::select_vars_by_role(c("imputation-predictor"), my_variables))

# Generate imputed Table 1
# imputed_table1_data <- get_descriptive_data(
#   imputed_cycles1to6_data,
#   my_variables,
#   my_variable_details,
#   # All the variables whose descriptive statistics we want
#   recodeflow:::select_vars_by_role(
#     c("Table 1"),
#     my_variables
#   ),
#   # Sets the stratifier
#   list("all" = list("clc_sex"))
# )
# 
# create_descriptive_table(
#   imputed_table1_data,
#   my_variables,
#   my_variable_details,
#   recodeflow:::select_vars_by_role(
#     c("Table 1"),
#     my_variables
#   ),
#   column_stratifier = c("clc_sex"),
#   subjects_order = c("Age", "Sex", "Marital status", "Education", "Occupation", "Family history", "Exercise", "Diet", "Weight", "Chronic disease", "Alcohol", "Smoking", "Sleep", "General")
# )

# Generate imputed and weighted Table 1
imputed_cycles1to6_data <- imputed_cycles1to6_data %>%
  dplyr::rename(
    'Age' = clc_age,
    `Marital status` = married,
    `Highest education level` = edudr04,
    `Working status` = working,
    `Hypertension family history` = fmh_15,
    `Minutes of exercise per week` = minperweek,
    `Daily fruit and vegetable consumption` = totalfv,
    `Body mass index` = hwmdbmi,
    `Waist-to-height ratio` = whr,
    `Chronic kidney disease` = ckd,
    'Diabetes' = diabx,
    `Alcohol consumption level` = low_drink_score1,
    `Smoking status` = smoke,
    `Hours of sleep per day` = slp_11,
    `Self-rated mental health` = gendmhi,
    'Stress' = gen_025,
    `Sense of belonging` = gen_045
  )

weighted_imputed_data <- survey::svydesign(
  id = ~1,
  weights = ~wgt_full,
  data = imputed_cycles1to6_data 
)

gtsummary::tbl_svysummary(
  data = weighted_imputed_data, 
  by = clc_sex,
  include = -wgt_full,
  statistic = list(
    all_continuous() ~ "{median} ({p25}, {p75})",
    all_categorical() ~ "{n_unweighted} ({p}%)"
  ),
  missing = "no"
) %>%
  # Rename columns
  gtsummary::modify_header(label = "**Characteristic**") %>%
  gtsummary::modify_spanning_header(
    all_stat_cols() ~ "**Sex**"
  ) %>%
  gtsummary::modify_header(
    stat_1 = "**Male**",
    stat_2 = "**Female**"
  ) %>%
  # Rename variable names and category labels
  gtsummary::modify_table_body(
    ~ .x %>%
      dplyr::mutate(
        label = dplyr::case_when(
          variable == "Marital status" & label == "1" ~ "Married or common-law",
          variable == "Marital status" & label == "2" ~ "Widowed, separated, or divorced",
          variable == "Marital status" & label == "3" ~ "Single",
          variable == "Highest education level" & label == "1" ~ "No high school",
          variable == "Highest education level" & label == "2" ~ "Secondary",
          variable == "Highest education level" & label == "3" ~ "Post-secondary",
          variable == "Working status" & label == "1" ~ "Has a job",
          variable == "Working status" & label == "2" ~ "Does not have a job",
          variable == "Hypertension family history" & label == "1" ~ "Yes",
          variable == "Hypertension family history" & label == "2" ~ "No",
          variable == "Chronic kidney disease" & label == "1" ~ "Yes",
          variable == "Chronic kidney disease" & label == "2" ~ "No",
          variable == "Diabetes" & label == "1" ~ "Yes",
          variable == "Diabetes" & label == "2" ~ "No",
          variable == "Alcohol consumption level" & label == "1" ~ "Never drinker",
          variable == "Alcohol consumption level" & label == "2" ~ "Former drinker",
          variable == "Alcohol consumption level" & label == "3" ~ "Light drinker",
          variable == "Alcohol consumption level" & label == "4" ~ "Moderate to heavy drinker",
          variable == "Smoking status" & label == "1" ~ "Current smoker",
          variable == "Smoking status" & label == "2" ~ "Former smoker",
          variable == "Smoking status" & label == "3" ~ "Never smoker",
          variable == "Self-rated mental health" & label == "1" ~ "Poor",
          variable == "Self-rated mental health" & label == "2" ~ "Fair or good",
          variable == "Self-rated mental health" & label == "3" ~ "Very good or excellent",
          variable == "Stress" & label == "1" ~ "Not at all to a bit",
          variable == "Stress" & label == "2" ~ "Quite a bit or extremely",
          variable == "Sense of belonging" & label == "1" ~ "Strong",
          variable == "Sense of belonging" & label == "2" ~ "Weak",
          TRUE ~ label
        )
      )
  )

# Add extra variables to imputed dataset for exploratory analysis
imputed_cycles1to6_data$cardiov <- cycles1to6_data$cardiov
imputed_cycles1to6_data$anymed2 <- cycles1to6_data$anymed2
imputed_cycles1to6_data$ccc_32 <- cycles1to6_data$ccc_32