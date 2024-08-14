# Set working directory at RDC
setwd("P:/10619/Dropbox/chmsflow")

# Load required packages
library(dplyr)
library(caret)
library(Hmisc)

# Load this R file to obtain imputed dataset
source("R/table-1.R")

# Recode 2s as 0s in binary predictors and factorize all categorical predictors
imputed_cycles1to6_data <- imputed_cycles1to6_data %>%
  mutate(highbp14090_adj = ifelse(highbp14090_adj == 2, 0, highbp14090_adj),
         ckd = ifelse(ckd == 2, 0, ckd),
         diab_m = ifelse(diab_m == 2, 0, diab_m),
         fmh_15 = ifelse(fmh_15 == 2, 0, fmh_15),
         smoke = ifelse(smoke == 2, 0, smoke),
         edudr04 = case_when(
           edudr04 == 1 ~ 2,
           edudr04 == 2 ~ 1,
           edudr04 == 3 ~ 0
         ),
         gendmhi = case_when(
           gendmhi == 1 ~ 2,
           gendmhi == 2 ~ 1,
           gendmhi == 3 ~ 0
         ))

cat_variables <- c("ckd", "diab_m", "edudr04", "fmh_15", "gendmhi", 
                   "gen_025", "gen_045", "low_drink_score1", "married", 
                   "smoke", "working")
imputed_cycles1to6_data <- imputed_cycles1to6_data %>%
  mutate(across(all_of(cat_variables), as.factor))

# Separate male and female data
male_data <- filter(imputed_cycles1to6_data, clc_sex == 1)
female_data <- filter(imputed_cycles1to6_data, clc_sex == 2)

# Create train and test data for male and female
male_train_data <- filter(male_data, cycle != 6)
female_train_data <- filter(female_data, cycle != 6)

male_test_data <- filter(male_data, cycle == 6)
female_test_data <- filter(female_data, cycle == 6)

# Apply survey weights to male and female train data
weighted_male <- svydesign(
  id = ~1,
  weights = ~wgt_full,
  data = male_train_data
)

weighted_female <- svydesign(
  id = ~1,
  weights = ~wgt_full,
  data = female_train_data
)

# Fit male and female models
male_model <- svyglm(highbp14090_adj ~ rcs(clc_age, 4) + married + edudr04 + working + gendmhi + gen_025 + gen_045 + fmh_15 +
                       rcs(hwmdbmi, 3) + rcs(whr, 3) + low_drink_score1 + rcs(minperweek, 3) + smoke + slp_11 + totalfv + diab_m + 
                       ckd + rcs(clc_age, 4)*gen_045 + rcs(clc_age, 4)*rcs(hwmdbmi, 3) + rcs(clc_age, 4)*rcs(whr, 3) + rcs(clc_age, 4)*rcs(minperweek, 3) +
                       rcs(clc_age, 4)*smoke + rcs(clc_age, 4)*slp_11 + rcs(clc_age, 4)*diab_m + rcs(clc_age, 4)*ckd + rcs(hwmdbmi, 3)*rcs(whr, 3), 
                     design = weighted_male, family = quasibinomial())

female_model <- svyglm(highbp14090_adj ~ rcs(clc_age, 4) + married + edudr04 + working + gendmhi + gen_025 + gen_045 + fmh_15 +
                         rcs(hwmdbmi, 3) + rcs(whr, 3) + low_drink_score1 + rcs(minperweek, 3) + smoke + slp_11 + totalfv + diab_m + 
                         ckd + rcs(clc_age, 4)*gen_045 + rcs(clc_age, 4)*rcs(hwmdbmi, 3) + rcs(clc_age, 4)*rcs(whr, 3) + rcs(clc_age, 4)*rcs(minperweek, 3) +
                         rcs(clc_age, 4)*smoke + rcs(clc_age, 4)*slp_11 + rcs(clc_age, 4)*diab_m + rcs(clc_age, 4)*ckd + rcs(hwmdbmi, 3)*rcs(whr, 3), 
                       design = weighted_female, family = quasibinomial())