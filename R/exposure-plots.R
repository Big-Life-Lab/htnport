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

male_data <- filter(cycles1to6_data, clc_sex == 1)
female_data <- filter(cycles1to6_data, clc_sex == 2)

plot(density(male_data$clc_age), main = "Age distribution", xlab = "Age", col = "blue")
lines(density(female_data$clc_age), col = "red")
legend("topright", legend = c("Male", "Female"), col = c("blue", "red"), lty = 1:2, inset = 0.02, bg = 'white')

male_exercise_data <- male_data %>% drop_na(minperweek)
female_exercise_data <- female_data %>% drop_na(minperweek)
plot(density(male_exercise_data$minperweek), main = "Exercise distribution", xlab = "Average minutes of exercise per week", col = "blue", xlim = c(0, 150), ylim = c(0, 0.008))
lines(density(female_exercise_data$minperweek), col = "red")
legend("topright", legend = c("Male", "Female"), col = c("blue", "red"), lty = 1:2, inset = 0.02, bg = 'white')

plot(density(male_data$totalfv), main = "Fruit and vegetable consumption distribution", xlab = "Times per day produce consumed", col = "blue")
lines(density(female_data$totalfv), col = "red")
legend("topright", legend = c("Male", "Female"), col = c("blue", "red"), lty = 1:2, inset = 0.02, bg = 'white')

male_bmi_data <- male_data %>% drop_na(hwmdbmi)
female_bmi_data <- female_data %>% drop_na(hwmdbmi)
plot(density(male_bmi_data$hwmdbmi), main = "BMI distribution", xlab = "BMI", col = "blue")
lines(density(female_bmi_data$hwmdbmi), col = "red")
legend("topright", legend = c("Male", "Female"), col = c("blue", "red"), lty = 1:2, inset = 0.02, bg = 'white')

male_whr_data <- male_data %>% drop_na(whr)
female_whr_data <- female_data %>% drop_na(whr)
plot(density(male_whr_data$whr), main = "Waist-to-height ratio", xlab = "Waist-to-height ratio", col = "blue")
lines(density(female_whr_data$whr), col = "red")
legend("topright", legend = c("Male", "Female"), col = c("blue", "red"), lty = 1:2, inset = 0.02, bg = 'white')

male_sleep_data <- male_data %>% drop_na(slp_11)
female_sleep_data <- female_data %>% drop_na(slp_11)
plot(density(male_sleep_data$slp_11), main = "Sleep distribution", xlab = "Hours slept per night", col = "blue")
lines(density(female_sleep_data$slp_11), col = "red")
legend("topright", legend = c("Male", "Female"), col = c("blue", "red"), lty = 1:2, inset = 0.02, bg = 'white')