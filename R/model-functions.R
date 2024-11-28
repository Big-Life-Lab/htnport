# Function for multicollinearity assessment
calculate_simplified_vif <- function(design) {
  # Define the simplified formula without interaction and spline terms
  simplified_formula <- highbp14090_adj ~ clc_age + married + edudr04 + working + gendmhi + gen_025 + gen_045 + fmh_15 +
    hwmdbmi + whr + low_drink_score1 + minperweek + smoke + slp_11 + totalfv + diabx + ckd
  
  # Fit the simplified model
  simplified_model <- survey::svyglm(simplified_formula, design = design, family = quasibinomial())
  
  # Calculate VIF for the simplified model
  vif_values <- rms::vif(simplified_model)
  
  # Return the VIF values
  return(vif_values)
}

# Function for stepdown procedure by Harrell and Ambler
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

# Metric-generating functions
# Nagelkerke's R²
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

# Brier score
calculate_brier_score <- function(model, data) {
  # Generate predicted_probabilities
  predicted_probabilities <- predict(model, newdata = data, type = "response")
  
  # Calculate Brier score
  brier_score <- mean((predicted_probabilities - data$highbp14090_adj)^2)
  
  return(brier_score)
}

# c-statistic via ROC
calculate_auc <- function(model, data) {
  # Generate predicted_probabilities
  predicted_probabilities <- predict(model, newdata = data, type = "response")
  
  # Calculate ROC curve and AUC
  auc_value <- pROC::auc(pROC::roc(data$highbp14090_adj, predicted_probabilities))
  
  return(auc_value)
}

# Calibration - comparing observed and predicted probabilities, both in-the-whole and across percentiles
compare_probs <- function(data, predicted_probs) {
  # Add predicted probabilities to data
  data$predicted <- predicted_probs
  
  # Calculate overall observed and predicted probabilities
  overall_observed <- mean(data$highbp14090_adj)
  overall_predicted <- mean(data$predicted)
  
  # Relative difference overall
  relative_difference_overall <- (overall_observed - overall_predicted) / overall_predicted
  
  # Calculate 90th and 10th percentiles of predicted probabilities
  percentile_90 <- quantile(data$predicted, 0.9)
  percentile_10 <- quantile(data$predicted, 0.1)
  
  # Observed and predicted probabilities for 90th and 10th percentiles
  observed_90 <- mean(data$highbp14090_adj[data$predicted >= percentile_90])
  predicted_90 <- mean(data$predicted[data$predicted >= percentile_90])
  observed_10 <- mean(data$highbp14090_adj[data$predicted <= percentile_10])
  predicted_10 <- mean(data$predicted[data$predicted <= percentile_10])
  
  # Relative differences for 90th and 10th percentiles
  relative_diff_90 <- (observed_90 - predicted_90) / predicted_90
  relative_diff_10 <- (observed_10 - predicted_10) / predicted_10
  
  # Ratio of relative differences for 90th vs 10th percentile
  ratio_90_10 <- relative_diff_90 / relative_diff_10
  
  # Calculate 95th and 5th percentiles of predicted probabilities
  percentile_95 <- quantile(data$predicted, 0.95)
  percentile_5 <- quantile(data$predicted, 0.05)
  
  # Observed and predicted probabilities for 95th and 5th percentiles
  observed_95 <- mean(data$highbp14090_adj[data$predicted >= percentile_95])
  predicted_95 <- mean(data$predicted[data$predicted >= percentile_95])
  observed_5 <- mean(data$highbp14090_adj[data$predicted <= percentile_5])
  predicted_5 <- mean(data$predicted[data$predicted <= percentile_5])
  
  # Relative differences for 95th and 5th percentiles
  relative_diff_95 <- (observed_95 - predicted_95) / predicted_95
  relative_diff_5 <- (observed_5 - predicted_5) / predicted_5
  
  # Ratio of relative differences for 95th vs 5th percentile
  ratio_95_5 <- relative_diff_95 / relative_diff_5
  
  # Return results
  return(list(
    relative_difference_overall = relative_difference_overall,
    ratio_90_10 = ratio_90_10,
    ratio_95_5 = ratio_95_5
  ))
}

# Calibration slopes
calibration_slope <- function(data, predicted_probs) {
  data$predicted <- predicted_probs
  slope_model <- lm(highbp14090_adj ~ predicted, data = data)
  return(summary(slope_model))
}

# Predicted probabilities for models
generate_predicted_probabilities <- function(model, data) {
  predicted_probabilities <- predict(model, newdata = data, type = "response")
  return(predicted_probabilities)
}

# Bootstrap function which returns metrics per bootstrap sample
bootstrap_function <- function(data, indices, model) {
  # Create the bootstrap sample using indices
  boot_data <- data[indices, ]
  
  # Recreate the survey design with the bootstrap sample
  boot_design <- survey::svydesign(ids = ~1, weights = ~wgt_full, data = boot_data)
  
  # Refit the provided svyglm model on the bootstrap sample
  boot_model <- update(model, data = boot_data, design = boot_design)
  
  # Generate predicted probabilities on the original (out-of-bag) data
  predicted_probs <- predict(boot_model, newdata = data, type = "response")
  
  # Calculate Nagelkerke's R²
  nagelkerke_r2 <- calculate_nagelkerke_r2(boot_model, data)
  
  # Calculate Brier Score
  brier_score <- calculate_brier_score(boot_model, data)
  
  # Calculate AUC
  auc_value <- calculate_auc(boot_model, data)
  
  # Calibration Comparison (Relative Differences and Ratios)
  calibration_comparison <- compare_probs(data, predicted_probs)
  
  # Calculate Calibration Slope
  slope_model <- calibration_slope(data, predicted_probs)
  calibration_slope_value <- slope_model$coefficients[2]  # Extract slope
  
  # Return all performance metrics as a list
  return(list(
    nagelkerke_r2 = nagelkerke_r2,
    brier_score = brier_score,
    auc_value = auc_value,
    relative_difference_overall = calibration_comparison$relative_difference_overall,
    ratio_90_10 = calibration_comparison$ratio_90_10,
    ratio_95_5 = calibration_comparison$ratio_95_5,
    calibration_slope = calibration_slope_value
  ))
}