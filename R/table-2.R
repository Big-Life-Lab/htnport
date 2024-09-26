# Set working directory at RDC
setwd("P:/10619/Dropbox/chmsflow")

# Load packages
library(dplyr)
library(e1071)
library(survey)

# Load this R file to obtain imputed dataset
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
#   cycle = sample(1:6, 9627, replace = TRUE), # Cycle variable ranging from 1 to 6
#   anymed2 = sample(0:1, 9627, replace = TRUE), # Binary
#   ccc_32 = sample(1:2, 9627, replace = TRUE), # Binary
#   cardiov = sample(1:2, 9627, replace = TRUE) # Binary
# )

# Generate Table 2a - outcome x sex distribution
table2a_data <- get_descriptive_data(
  imputed_cycles1to6_data,
  my_variables,
  my_variable_details,
  # All the variables whose descriptive statistics we want
  "highbp14090_adj",
  # Sets the stratifier
  list("all" = list("clc_sex"))
)

create_descriptive_table(
  table2a_data,
  my_variables,
  my_variable_details,
  "highbp14090_adj",
  column_stratifier = c("clc_sex")
)

# Generate Table 2b - predictor x outcome distribution
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

# Generate Table 2c - meds distribution
table2c_data <- get_descriptive_data(
  imputed_cycles1to6_data,
  my_variables,
  my_variable_details,
  # All the variables whose descriptive statistics we want
  "ccc_32",
  # Sets the stratifier
  list("all" = list("anymed2"))
)

create_descriptive_table(
  table2c_data,
  my_variables,
  my_variable_details,
  "ccc_32",
  column_stratifier = c("anymed2")
)

# Generate Table 2d - misclassified meds distribution for chronic conditions
imputed_cycles1to6_data$misclassified_meds <- ifelse(imputed_cycles1to6_data$anymed2 == 1 & imputed_cycles1to6_data$ccc_32 == 2, 1, 0)

table2d_data <- imputed_cycles1to6_data %>%
  group_by(misclassified_meds) %>%
  summarise(
    diabx_1_count = sum(diabx == 1, na.rm = TRUE),
    diabx_2_count = sum(diabx == 2, na.rm = TRUE),
    cardiov_1_count = sum(cardiov == 1, na.rm = TRUE),
    cardiov_2_count = sum(cardiov == 2, na.rm = TRUE),
    ckd_1_count = sum(ckd == 1, na.rm = TRUE),
    ckd_2_count = sum(ckd == 2, na.rm = TRUE)
  )

flextable::flextable(table2d_data)

# Truncate skewed continuous variables if necessary
truncate_skewed <- function(df, threshold = 0.995, skew_threshold = 1) {
  
  # Create a copy of the dataframe to avoid overwriting the original
  df_truncated <- df
  
  # Loop over all the numeric columns
  for (col in c("clc_age", "hwmdbmi", "minperweek", "totalfv", "whr", "slp_11")) {
    if (is.numeric(df[[col]])) {
      
      # Calculate skewness
      skewness_value <- skewness(df[[col]], na.rm = TRUE)
      
      # Check if the variable is skewed
      if (abs(skewness_value) > skew_threshold) {
        
        # Calculate the 99.5th percentile
        quantile_value <- quantile(df[[col]], threshold, na.rm = TRUE)
        
        # Truncate the variable
        df_truncated[[col]] <- ifelse(df[[col]] > quantile_value, quantile_value, df[[col]])
        
        # Print message indicating that the column was truncated
        message(paste("Truncated column:", col, "| Skewness:", round(skewness_value, 2)))
      }
    }
  }
  
  return(df_truncated)
}

imputed_cycles1to6_data <- truncate_skewed(imputed_cycles1to6_data)

# Recode 2s as 0s in binary predictors and factorize all categorical predictors
imputed_cycles1to6_data <- imputed_cycles1to6_data %>%
  mutate(highbp14090_adj = ifelse(highbp14090_adj == 2, 0, highbp14090_adj),
         ckd = ifelse(ckd == 2, 0, ckd),
         diabx = ifelse(diabx == 2, 0, diabx),
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
         ),
         snoke = case_when(
           smoke == 1 ~ 2,
           smoke == 2 ~ 1,
           smoke == 3 ~ 0
         ))

cat_variables <- c("ckd", "diabx", "edudr04", "fmh_15", "gendmhi", 
               "gen_025", "gen_045", "low_drink_score1", "married", 
               "smoke", "working")
imputed_cycles1to6_data <- imputed_cycles1to6_data %>%
  mutate(across(all_of(cat_variables), as.factor))

# Separate male and female data
male_data <- filter(imputed_cycles1to6_data, clc_sex == 1)
female_data <- filter(imputed_cycles1to6_data, clc_sex == 2)

# Apply survey weights to male and female data
weighted_male <- svydesign(
    id = ~1,
    weights = ~wgt_full,
    data = male_data
  )

weighted_female <- svydesign(
  id = ~1,
  weights = ~wgt_full,
  data = female_data
)

# List of predictors
predictors <- recodeflow:::select_vars_by_role(c("Predictor"), my_variables)

# Function to fit crude models and return ORs and CIs for all levels of a predictor
fit_crude_model <- function(predictor, design) {
  formula <- as.formula(paste("highbp14090_adj ~", predictor))
  model <- svyglm(formula, design = design, family = quasibinomial())
  
  # Extract coefficients and calculate ORs
  coef_est <- coef(model)
  or <- round(exp(coef_est), 2)
  
  # Calculate confidence intervals
  conf_int <- exp(confint(model))
  
  # Create a data frame with the results
  # We use [-1] to exclude the intercept
  results <- data.frame(
    Variable = predictor,
    Level = names(coef_est)[-1],
    OR = or[-1],  # Exclude the intercept
    CI_Lower = round(conf_int[-1, 1], 2),
    CI_Upper = round(conf_int[-1, 2], 2)
  )
  
  results <- results %>%
    mutate(Level = recode(Level,
                          "ckd1" = "Chronic kidney disease",
                          "clc_age" = "Age",
                          "diabx1" = "Diabetes",
                          "edudr041" = "High school graduate only",
                          "edudr042" = "Did not graduate high school",
                          "fmh_151" = "Family history for hypertension",
                          "gendmhi1" = "Fair or good mental health",
                          "gendmhi2" = "Poor mental health",
                          "gen_0252" = "Quite a bit or extremely stressed",
                          "gen_0452" = "Weak sense of belonging",
                          "hwmdbmi" = "Body mass index",
                          "low_drink_score12" = "Former drinker",
                          "low_drink_score13" = "Light drinker",
                          "low_drink_score14" = "Moderate to heavy drinker",
                          "married2" = "Widowed, separated, or divorced",
                          "married3" = "Single",
                          "minperweek" = "Minutes of exercise per week",
                          "slp_11" = "Sleep duration",
                          "smoke1" = "Former smoker",
                          "smoke2" = "Current smoker",
                          "totalfv" = "Daily fruit and vegetable consumption",
                          "whr" = "Waist-to-height ratio",
                          "working2" = "Does not have a job"))
  
  return(results)
}

# Fit crude models for males and extract ORs and CIs
crude_models_male <- lapply(predictors, fit_crude_model, design = weighted_male)
crude_models_male <- do.call(rbind, crude_models_male)

# Fit crude models for females and extract ORs and CIs
crude_models_female <- lapply(predictors, fit_crude_model, design = weighted_female)
crude_models_female <- do.call(rbind, crude_models_female)

# Combine the data frames for males and females
combined_crude_models <- bind_rows(
  crude_models_male %>% mutate(Sex = "Male"),
  crude_models_female %>% mutate(Sex = "Female")
)

# Define the custom order for the Level column
custom_order <- c("Age", "Widowed, separated, or divorced", "Single", "High school graduate only", "Did not graduate high school", "Does not have a job",
                  "Family history for hypertension", "Minutes of exercise per week", "Daily fruit and vegetable consumption", "Body mass index", "Waist-to-height ratio",
                  "Diabetes", "Chronic kidney disease", "Former drinker", "Light drinker", "Moderate to heavy drinker", "Current smoker", "Sleep duration", 
                  "Fair or good mental health", "Poor mental health", "Quite a bit or extremely stressed", "Weak sense of belonging"
                  )

# Combine custom order with the original order
original_order <- unique(combined_crude_models$Level)
final_order <- c(custom_order, setdiff(original_order, custom_order))

# Convert the Level column to a factor with the specified levels
combined_crude_models <- combined_crude_models %>%
  mutate(Level = factor(Level, levels = final_order)) %>%
  arrange(Level) %>%
  select(-Variable)

combined_crude_models <- combined_crude_models[c("Sex", "Level", "OR", "CI_Lower", "CI_Upper")]

# Print the combined data frame
flextable::flextable(combined_crude_models)