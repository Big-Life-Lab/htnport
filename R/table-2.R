setwd("P:/10619/Dropbox/chmsflow")

library(survey)

source("R/table-1.R")

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

imputed_cycles1to6_data <- imputed_cycles1to6_data %>%
  mutate(highbp14090_adj = ifelse(highbp14090_adj == 2, 0, highbp14090_adj),
         diab_m = ifelse(diab_m == 2, 0, diab_m),
         ckd = ifelse(ckd == 2, 0, ckd),
         fmh_15 = ifelse(fmh_15 == 2, 0, fmh_15),
         smoke = ifelse(smoke == 2, 0, smoke),
         working = ifelse(working == 2, 0, working))

cat_variables <- c("ccc_51", "ckd", "edudr04", "fmh_15", "gendmhi", 
               "gen_025", "gen_045", "low_drink_score1", "married", 
               "smoke", "working")
imputed_cycles1to6_data <- imputed_cycles1to6_data %>%
  mutate(across(all_of(cat_variables), as.factor))

# synth_oh <- synth_oh %>%
#   mutate(
#     p_decayed = case_when(
#       n_decayed > 0 ~ 1,
#       n_decayed == 0 ~ 0,
#       TRUE ~ NA
#     ) %>%
#       factor)
male_data <- filter(imputed_cycles1to6_data, clc_sex == 1)
female_data <- filter(imputed_cycles1to6_data, clc_sex == 2)

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
  or <- exp(coef_est)
  
  # Calculate confidence intervals
  conf_int <- exp(confint(model))
  
  # Create a data frame with the results
  # We use [-1] to exclude the intercept
  results <- data.frame(
    Variable = predictor,
    Level = names(coef_est)[-1],
    OR = or[-1],  # Exclude the intercept
    CI_Lower = conf_int[-1, 1],
    CI_Upper = conf_int[-1, 2]
  )
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

# Print the combined data frame
flextable::flextable(combined_crude_models)