# Set working directory at RDC
setwd("P:/10619/Dropbox/chmsflow")

# Load required packages
library(survey)

# Load required file to obtain imputed dataset
source("R/table-1.R")

# Synthetic dataset for test use outside RDC
# imputed_cycles1to6_data <- data.frame(
#   highbp14090_adj = sample(1:2, 9627, replace = TRUE), # Binary outcome
#   ccc_51 = sample(1:2, 9627, replace = TRUE), # Binary
#   ckd = sample(1:2, 9627, replace = TRUE), # Binary
#   edudr04 = sample(1:3, 9627, replace = TRUE), # 3 categories
#   fmh_15 = sample(1:2, 9627, replace = TRUE), # Binary
#   gendmhi = sample(1:3, 9627, replace = TRUE), # 3 categories
#   gen_025 = sample(1:2, 9627, replace = TRUE), # 3 categories
#   gen_045 = sample(1:2, 9627, replace = TRUE), # Binary
#   low_drink_score1 = sample(1:4, 9627, replace = TRUE), # 4 categories
#   married = sample(1:3, 9627, replace = TRUE), # 3 categories
#   smoke = sample(1:2, 9627, replace = TRUE), # Binary
#   working = sample(1:2, 9627, replace = TRUE), # Binary
#   clc_sex = sample(1:2, 9627, replace = TRUE), # Binary
#   wgt_full = runif(9627, 0, 1), # Continuous weights
#   clc_age = runif(9627, 18, 90), # Continuous
#   hwmdbmi = runif(9627, 18, 40), # Continuous
#   minperweek = runif(9627, 0, 2000), # Continuous
#   totalfv = runif(9627, 0, 10), # Continuous
#   whr = runif(9627, 0.5, 1.5), # Continuous
#   slp_11 = runif(9627, 4, 12), # Continuous
#   diabx = sample(1:2, 9627, replace = TRUE), # Binary
#   cycle = sample(1:6, 9627, replace = TRUE) # Cycle variable ranging from 1 to 6
# )

# Separate male and female data
male_data <- filter(imputed_cycles1to6_data, clc_sex == 1)
female_data <- filter(imputed_cycles1to6_data, clc_sex == 2)

# Create survey design objects for male and female data
male_svy <- svydesign(ids = ~1, data = male_data, weights = ~wgt_full)
female_svy <- svydesign(ids = ~1, data = female_data, weights = ~wgt_full)

# Weighted density plot for age
plot(svysmooth(~clc_age, design = male_svy), col = "blue", main = "Age distribution", xlab = "Age")
lines(svysmooth(~clc_age, design = female_svy), col = "red")
legend("topright", legend = c("Male", "Female"), col = c("blue", "red"), lty = 1:2, inset = 0.02, bg = 'white')

# Weighted density plot for physical activity minutes
plot(svysmooth(~minperweek, design = male_svy), col = "blue", main = "Exercise distribution", xlab = "Average minutes of exercise per week", xlim = c(0, 150))
lines(svysmooth(~minperweek, design = female_svy), col = "red")
legend("topright", legend = c("Male", "Female"), col = c("blue", "red"), lty = 1:2, inset = 0.02, bg = 'white')

# Weighted density plot for total fruit and vegetable consumption
plot(svysmooth(~totalfv, design = male_svy), col = "blue", main = "Fruit and vegetable consumption distribution", xlab = "Times per day produce consumed")
lines(svysmooth(~totalfv, design = female_svy), col = "red")
legend("topright", legend = c("Male", "Female"), col = c("blue", "red"), lty = 1:2, inset = 0.02, bg = 'white')

# Weighted density plot for body mass index
plot(svysmooth(~hwmdbmi, design = male_svy), col = "blue", main = "BMI distribution", xlab = "BMI")
lines(svysmooth(~hwmdbmi, design = female_svy), col = "red")
legend("topright", legend = c("Male", "Female"), col = c("blue", "red"), lty = 1:2, inset = 0.02, bg = 'white')

# Weighted density plot for waist-to-height ratio
plot(svysmooth(~whr, design = male_svy), col = "blue", main = "Waist-to-height ratio", xlab = "Waist-to-height ratio")
lines(svysmooth(~whr, design = female_svy), col = "red")
legend("topright", legend = c("Male", "Female"), col = c("blue", "red"), lty = 1:2, inset = 0.02, bg = 'white')

# Weighted density plot for sleep duration
plot(svysmooth(~slp_11, design = male_svy), col = "blue", main = "Sleep distribution", xlab = "Hours slept per night")
lines(svysmooth(~slp_11, design = female_svy), col = "red")
legend("topright", legend = c("Male", "Female"), col = c("blue", "red"), lty = 1:2, inset = 0.02, bg = 'white')