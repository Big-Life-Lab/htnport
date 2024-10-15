# Set working directory at RDC
setwd("P:/10619/Dropbox/chmsflow")

# Load this R file to obtain imputed dataset
source("R/table-1.R")

# Load new packages
library(e1071)
library(survey)
library(rms)

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
#   smoke = sample(1:3, 9627, replace = TRUE), # Binary
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
         smoke = case_when(
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

# Apply survey weights to male and female train data
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

# Fit male and female models
male_model <- svyglm(highbp14090_adj ~ rcs(clc_age, 4) + married + edudr04 + working + gendmhi + gen_025 + gen_045 + fmh_15 +
                       rcs(hwmdbmi, 3) + rcs(whr, 3) + low_drink_score1 + rcs(minperweek, 3) + smoke + slp_11 + totalfv + diabx + 
                       ckd + rcs(clc_age, 4)*gen_045 + rcs(clc_age, 4)*rcs(hwmdbmi, 3) + rcs(clc_age, 4)*rcs(whr, 3) + rcs(clc_age, 4)*rcs(minperweek, 3) +
                       rcs(clc_age, 4)*smoke + rcs(clc_age, 4)*slp_11 + rcs(clc_age, 4)*diabx + rcs(clc_age, 4)*ckd + rcs(hwmdbmi, 3)*rcs(whr, 3), 
                     design = weighted_male, family = quasibinomial())

female_model <- svyglm(highbp14090_adj ~ rcs(clc_age, 4) + married + edudr04 + working + gendmhi + gen_025 + gen_045 + fmh_15 +
                         rcs(hwmdbmi, 3) + rcs(whr, 3) + low_drink_score1 + rcs(minperweek, 3) + smoke + slp_11 + totalfv + diabx + 
                         ckd + rcs(clc_age, 4)*gen_045 + rcs(clc_age, 4)*rcs(hwmdbmi, 3) + rcs(clc_age, 4)*rcs(whr, 3) + rcs(clc_age, 4)*rcs(minperweek, 3) +
                         rcs(clc_age, 4)*smoke + rcs(clc_age, 4)*slp_11 + rcs(clc_age, 4)*diabx + rcs(clc_age, 4)*ckd + rcs(hwmdbmi, 3)*rcs(whr, 3), 
                       design = weighted_female, family = quasibinomial())

# Multicollinearity assessment
calculate_simplified_vif <- function(design) {
  # Define the simplified formula without interaction and spline terms
  simplified_formula <- highbp14090_adj ~ clc_age + married + edudr04 + working + gendmhi + gen_025 + gen_045 + fmh_15 +
  hwmdbmi + whr + low_drink_score1 + minperweek + smoke + slp_11 + totalfv + diabx + ckd
  
  # Fit the simplified model
  simplified_model <- svyglm(simplified_formula, design = design, family = quasibinomial())
  
  # Calculate VIF for the simplified model
  vif_values <- vif(simplified_model)
  
  # Return the VIF values
  return(vif_values)
}

calculate_simplified_vif(design = weighted_male)
calculate_simplified_vif(design = weighted_female)

# Linearity assessment for slp_11 and totalfv
plot(male_data$slp_11, male_model$fitted.values) 
plot(male_data$totalfv, male_model$fitted.values) 

plot(female_data$slp_11, female_model$fitted.values) 
plot(female_data$totalfv, female_model$fitted.values) 

# Refit male and female models without whr if high collinearity detected
# male_model <- svyglm(highbp14090_adj ~ rcs(clc_age, 4) + married + edudr04 + working + gendmhi + gen_025 + gen_045 + fmh_15 +
#                        rcs(hwmdbmi, 3) + low_drink_score1 + rcs(minperweek, 3) + smoke + slp_11 + totalfv + diabx + 
#                        ckd + rcs(clc_age, 4)*gen_045 + rcs(clc_age, 4)*rcs(hwmdbmi, 3) + rcs(clc_age, 4)*rcs(minperweek, 3) +
#                        rcs(clc_age, 4)*smoke + rcs(clc_age, 4)*slp_11 + rcs(clc_age, 4)*diabx + rcs(clc_age, 4)*ckd, 
#                      design = weighted_male, family = quasibinomial())
# 
# female_model <- svyglm(highbp14090_adj ~ rcs(clc_age, 4) + married + edudr04 + working + gendmhi + gen_025 + gen_045 + fmh_15 +
#                        rcs(hwmdbmi, 3) + low_drink_score1 + rcs(minperweek, 3) + smoke + slp_11 + totalfv + diabx + 
#                        ckd + rcs(clc_age, 4)*gen_045 + rcs(clc_age, 4)*rcs(hwmdbmi, 3) + rcs(clc_age, 4)*rcs(minperweek, 3) +
#                        rcs(clc_age, 4)*smoke + rcs(clc_age, 4)*slp_11 + rcs(clc_age, 4)*diabx + rcs(clc_age, 4)*ckd, 
#                      design = weighted_female, family = quasibinomial())

# Function to calculate Nagelkerke's R²
calculate_nagelkerke_r2 <- function(model, data) {
  # Get the number of observations
  n <- nrow(data)
  
  # Get the log-likelihood of the fitted model
  fitted_model_fit <- glm(formula = model$formula, data = data, family = binomial())
  log_likelihood_fitted <- logLik(fitted_model_fit)[1]
  
  # Get the log-likelihood of the null model
  null_model_fit <- glm(highbp14090_adj ~ 1, data = data, family = binomial())
  log_likelihood_null <- logLik(null_model_fit)[1]
  
  # Calculate the likelihood ratio statistic (LR)
  LR <- 2 * (log_likelihood_fitted - log_likelihood_null)
  
  # Calculate Nagelkerke's R² using the formula
  nagelkerke_r2 <- (1 - exp(-LR / n)) / (1 - exp(-(-2 * log_likelihood_null) / n))
  
  return(nagelkerke_r2)
}

# Stepdown procedure by Harrell and Ambler
stepdown <- function(full_model, data, threshold = 0.95) {
  initial_r2 <- calculate_nagelkerke_r2(full_model, data)
  current_model <- full_model
  current_r2 <- initial_r2
  removed_terms <- c()
  
  # Get the terms in the model
  terms_in_model <- attr(terms(full_model), "term.labels")
  
  # Iteratively remove terms and check R²
  while (length(terms_in_model) > 0) {
    r2_drop <- sapply(terms_in_model, function(term) {
      reduced_model <- try(update(current_model, as.formula(paste(". ~ . -", term))), silent = TRUE)
      if (inherits(reduced_model, "try-error")) return(Inf)
      calculate_nagelkerke_r2(reduced_model, data)
    })
    
    # Identify the term with the least impact on R²
    term_to_remove <- terms_in_model[which.max(r2_drop)]
    max_r2_drop <- r2_drop[which.max(r2_drop)]
    
    # Check if the current R² after removing the term is still above the threshold
    if (max_r2_drop < threshold * initial_r2) {
      break
    } else {
      current_model <- update(current_model, as.formula(paste(". ~ . -", term_to_remove)))
      terms_in_model <- setdiff(terms_in_model, term_to_remove)
      removed_terms <- c(removed_terms, term_to_remove)
      current_r2 <- max_r2_drop
    }
  }
  
  return(current_model)
}

# Apply stepdown function to male and female models
male_reduced_model <- stepdown(male_model, male_data)
female_reduced_model <- stepdown(female_model, female_data)

# Function to perform Likelihood Ratio Test (LRT) for svyglm models
lrt_svyglm <- function(full_model, reduced_model) {
  # Calculate the log-likelihoods
  logLik_full <- logLik(full_model)
  logLik_reduced <- logLik(reduced_model)
  
  # Compute the test statistic
  test_statistic <- -2 * (logLik_reduced - logLik_full)
  
  # Degrees of freedom difference
  df_diff <- df.residual(reduced_model) - df.residual(full_model)
  
  # Calculate p-value
  p_value <- pchisq(test_statistic, df = df_diff, lower.tail = FALSE)
  
  # Return the test statistic and p-value as a list
  return(list(statistic = test_statistic, p_value = p_value))
}

# Perform likelihood ratio tests to confirm reduced models as better fit (ie. p > 0.05)
lrt_svyglm(male_model, male_reduced_model)
lrt_svyglm(female_model, female_reduced_model)

# Perform likelihood ratio tests to determine overall significance of reduced models (ie. p < 0.05)
male_null_model <- svyglm(highbp14090_adj ~ 1, design = weighted_male, family = quasibinomial())
female_null_model <- svyglm(highbp14090_adj ~ 1, design = weighted_female, family = quasibinomial())

lrt_svyglm(male_reduced_model, male_null_model)
lrt_svyglm(female_reduced_model, female_null_model)