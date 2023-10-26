library(recodeflow)
library(cchsflow)
library(dplyr)
library(readr)
library(haven)
library(cli)
library(pastecs)
library(tidyr)

my_variables <- read.csv("P:/10619/Dropbox/Sept25/worksheets/chmsflow-variables.csv")
my_variable_details <- read.csv("P:/10619/Dropbox/Sept25/worksheets/chmsflow-variable-details.csv")

cycle1 <- read_stata("data/cycle1/cycle1.dta")
cycle2 <- read_stata("data/cycle2/cycle2.dta")
cycle3 <- read_stata("data/cycle3/cycle3.dta")
cycle4 <- read_stata("data/cycle4/cycle4.dta")
cycle5 <- read_stata("data/cycle5/cycle5.dta")
cycle6 <- read_stata("data/cycle6/cycle6.dta")
names(cycle6) <- tolower(names(cycle6)) 

cycle1_table1_data <- recodeflow::rec_with_table(cycle1, recodeflow:::select_vars_by_role("Table 1", my_variables), variable_details = my_variable_details, log = TRUE)
cycle2_table1_data <- recodeflow::rec_with_table(cycle2, recodeflow:::select_vars_by_role("Table 1", my_variables), variable_details = my_variable_details, log = TRUE)
cycle3_table1_data <- recodeflow::rec_with_table(cycle3, recodeflow:::select_vars_by_role("Table 1", my_variables), variable_details = my_variable_details, log = TRUE)
cycle4_table1_data <- recodeflow::rec_with_table(cycle4, recodeflow:::select_vars_by_role("Table 1", my_variables), variable_details = my_variable_details, log = TRUE)
cycle5_table1_data <- recodeflow::rec_with_table(cycle5, recodeflow:::select_vars_by_role("Table 1", my_variables), variable_details = my_variable_details, log = TRUE)
cycle6_table1_data <- recodeflow::rec_with_table(cycle6, recodeflow:::select_vars_by_role("Table 1", my_variables), variable_details = my_variable_details, log = TRUE)

cycles1to6_table1_data <- dplyr::bind_rows(cycle1_table1_data, cycle2_table1_data, cycle3_table1_data, cycle4_table1_data, cycle5_table1_data, cycle6_table1_data)
cycles1to6_table1_data <- dplyr::filter(cycles1to6_table1_data, insample == 1)

sbp_data <- cycles1to6_table1_data %>% drop_na(bpmdpbps)
plot(density(sbp_data$bpmdpbps), main = "Systolic blood pressure distribution", xlab = "Systolic blood pressure (mmHg)")

dbp_data <- cycles1to6_table1_data %>% drop_na(bpmdpbpd)
plot(density(dbp_data$bpmdpbpd), main = "Diastolic blood pressure distribution", xlab = "Diastolic blood pressure (mmHg)")

male_data <- filter(cycles1to6_table1_data, clc_sex == 1)
female_data <- filter(cycles1to6_table1_data, clc_sex == 2)

plot(density(male_data$clc_age), main = "Age distribution", xlab = "Age", col = "blue")
lines(density(female_data$clc_age), col = "red")

male_exercise_data <- male_data %>% drop_na(mvpa_min)
female_exercise_data <- female_data %>% drop_na(mvpa_min)
plot(density(male_exercise_data$mvpa_min), main = "Physical activity distribution", xlab = "Average minutes of exercise per day", col = "blue", ylim = c(0, 0.050))
lines(density(female_exercise_data$mvpa_min), col = "red")

plot(density(male_data$totalfv), main = "Fruit and vegetable consumption distribution", xlab = "Times per day produce consumed", col = "blue")
lines(density(female_data$totalfv), col = "red")

male_bmi_data <- male_data %>% drop_na(hwmdbmi)
female_bmi_data <- female_data %>% drop_na(hwmdbmi)
plot(density(male_bmi_data$hwmdbmi), main = "BMI distribution", xlab = "BMI", col = "blue")
lines(density(female_bmi_data$hwmdbmi), col = "red")

male_diabetes_data <- male_data %>% drop_na(lab_hba1)
female_diabetes_data <- female_data %>% drop_na(lab_hba1)
plot(density(male_diabetes_data$lab_hba1), main = "HbA1C distribution", xlab = "HbA1C", col = "blue")
lines(density(female_diabetes_data$lab_hba1), col = "red")

male_gfr_data <- male_data %>% drop_na(gfr)
female_gfr_data <- female_data %>% drop_na(gfr)
plot(density(male_gfr_data$gfr), main = "GFR distribution", xlab = "GFR", col = "blue")
lines(density(female_gfr_data$gfr), col = "red")

male_chol_data <- male_data %>% drop_na(nonhdl)
female_chol_data <- female_data %>% drop_na(nonhdl)
plot(density(male_chol_data$nonhdl), main = "Non-HDL Cholesterol distribution", xlab = "Non-HDL Cholesterol (mmol/L)", col = "blue", ylim = c(0, 0.4))
lines(density(female_chol_data$nonhdl), col = "red")