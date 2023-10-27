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

male_income_data <- male_data %>% drop_na(adj_hh_inc)
female_income_data <- female_data %>% drop_na(adj_hh_inc)
plot(density(male_income_data$adj_hh_inc), main = "Household income distribution", xlab = "Household income ($)", col = "blue")
lines(density(female_income_data$adj_hh_inc), col = "red")

male_work_hours_data <- male_data %>% drop_na(lmh_016)
female_work_hours_data <- female_data %>% drop_na(lmh_016)
plot(density(male_work_hours_data$lmh_016), main = "Working hours distribution", xlab = "Hours worked per week", col = "blue")
lines(density(female_work_hours_data$lmh_016), col = "red")

male_smoking_data <- male_data %>% drop_na(pack_years_der)
female_smoking_data <- female_data %>% drop_na(pack_years_der)
plot(density(male_smoking_data$pack_years_der), main = "Smoking distribution", xlab = "Smoking pack-years", col = "blue")
lines(density(female_smoking_data$pack_years_der), col = "red")

male_exercise_data <- male_data %>% drop_na(mvpa_min)
female_exercise_data <- female_data %>% drop_na(mvpa_min)
plot(density(male_exercise_data$mvpa_min), main = "Physical activity distribution", xlab = "Average minutes of exercise per day", col = "blue", ylim = c(0, 0.050))
lines(density(female_exercise_data$mvpa_min), col = "red")

plot(density(male_data$totalfv), main = "Fruit and vegetable consumption distribution", xlab = "Times per day produce consumed", col = "blue")
lines(density(female_data$totalfv), col = "red")

male_milk_data <- male_data %>% drop_na(mdcd04y)
female_milk_data <- female_data %>% drop_na(mdcd04y)
plot(density(male_milk_data$mdcd04y), main = "Milk distribution", xlab = "Times per year milk consumed", col = "blue")
lines(density(female_milk_data$mdcd04y), col = "red")

male_bmi_data <- male_data %>% drop_na(hwmdbmi)
female_bmi_data <- female_data %>% drop_na(hwmdbmi)
plot(density(male_bmi_data$hwmdbmi), main = "BMI distribution", xlab = "BMI", col = "blue")
lines(density(female_bmi_data$hwmdbmi), col = "red")

male_waist_circum_data <- male_data %>% drop_na(hwm_14cx)
female_waist_circum_data <- female_data %>% drop_na(hwm_14cx)
plot(density(male_waist_circum_data$hwm_14cx), main = "Waist circumference distribution", xlab = "Waist circumference (cm)", col = "blue")
lines(density(female_waist_circum_data$hwm_14cx), col = "red")

male_diabetes_data <- male_data %>% drop_na(lab_hba1)
female_diabetes_data <- female_data %>% drop_na(lab_hba1)
plot(density(male_diabetes_data$lab_hba1), main = "HbA1C distribution", xlab = "HbA1C", col = "blue")
lines(density(female_diabetes_data$lab_hba1), col = "red")

male_gfr_data <- male_data %>% drop_na(gfr)
female_gfr_data <- female_data %>% drop_na(gfr)
plot(density(male_gfr_data$gfr), main = "GFR distribution", xlab = "GFR (mL/min)", col = "blue")
lines(density(female_gfr_data$gfr), col = "red")

male_chol_data <- male_data %>% drop_na(nonhdl)
female_chol_data <- female_data %>% drop_na(nonhdl)
plot(density(male_chol_data$nonhdl), main = "Non-HDL cholesterol distribution", xlab = "Non-HDL Cholesterol (mmol/L)", col = "blue", ylim = c(0, 0.4))
lines(density(female_chol_data$nonhdl), col = "red")

male_ggt_data <- male_data %>% drop_na(lab_ggt)
female_ggt_data <- female_data %>% drop_na(lab_ggt)
plot(density(male_ggt_data$lab_ggt), main = "Blood gamma-glutamyltransferase distribution", xlab = "Blood gamma-glutamyltransferase (U/L)", col = "blue")
lines(density(female_ggt_data$lab_ggt), col = "red")

male_lead_data <- male_data %>% drop_na(lab_bpb)
female_lead_data <- female_data %>% drop_na(lab_bpb)
plot(density(male_lead_data$lab_bpb), main = "Blood lead distribution", xlab = "Blood lead (Âµmol/L)", col = "blue")
lines(density(female_lead_data$lab_bpb), col = "red")