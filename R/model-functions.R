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

# Function to calculate ORs and CIs from a model
get_or_table <- function(model) {
  # Extract coefficients and standard errors
  coef_table <- coef(summary(model))
  estimates <- coef_table[, "Estimate"]
  std_errors <- coef_table[, "Std. Error"]
  
  # Calculate OR and confidence intervals
  OR <- exp(estimates)
  lower_CI <- exp(estimates - 1.96 * std_errors)
  upper_CI <- exp(estimates + 1.96 * std_errors)
  
  # Create a data frame
  or_table <- data.frame(
    Variable = rownames(coef_table),
    OR = OR,
    Lower_CI = lower_CI,
    Upper_CI = upper_CI,
    stringsAsFactors = TRUE
  )
  
  return(or_table)
}

# Function for stepdown procedure by Harrell and Ambler
stepdown <- function(full_model, data, threshold = 0.95) {
  # Predict full model's predicted values
  full_model_predictions <- predict(full_model, type = "response")
  
  # Calculate the initial R² using ordinary R²
  initial_r2 <- calculate_r2(full_model_predictions, data$highbp14090_adj)
  current_model <- full_model
  current_r2 <- initial_r2
  removed_terms <- c()
  
  # Get the terms in the model
  terms_in_model <- attr(terms(full_model), "term.labels")
  
  # Iteratively remove terms and check R²
  while (length(terms_in_model) > 0) {
    # Debugging: Display current terms in the model
    print(paste("Current Terms:", paste(terms_in_model, collapse = ", ")))
    
    # Calculate the impact of removing each term on R²
    r2_drop <- sapply(terms_in_model, function(term) {
      reduced_model <- update(current_model, as.formula(paste(". ~ . -", term)))
      reduced_predictions <- predict(reduced_model, type = "response")
      calculate_r2(reduced_predictions, full_model_predictions)
    })
    
    # Debugging: Display R² drops
    print(paste("R² Drops:", paste(round(r2_drop, 3), collapse = ", ")))
    
    # Identify the term with the least impact on R² (highest R² after removal)
    term_to_remove <- terms_in_model[which.max(r2_drop)]
    max_r2_drop <- r2_drop[which.max(r2_drop)]
    
    # Debugging: Display the term to be removed and the current R²
    print(paste("Term to Remove:", term_to_remove))
    print(paste("Max R² Drop:", round(max_r2_drop, 3)))
    
    # Handle cases where max_r2_drop is NA, NaN, or infinite
    if (is.na(max_r2_drop) || is.nan(max_r2_drop) || is.infinite(max_r2_drop)) {
      print("Encountered invalid R² value (NA, NaN, or Inf). Stopping.")
      break
    }
    
    # Check if removing the term keeps the R² above the threshold
    if (max_r2_drop < threshold * initial_r2) {
      print("Stopping: Removing additional terms would lower R² below the threshold.")
      break
    } else {
      # Update the current model and terms
      current_model <- update(current_model, as.formula(paste(". ~ . -", term_to_remove)))
      terms_in_model <- setdiff(terms_in_model, term_to_remove)
      removed_terms <- c(removed_terms, term_to_remove)
      current_r2 <- max_r2_drop
    }
  }
  
  print("Removed Terms:")
  print(removed_terms)
  
  return(current_model)
}

# Function to perform Likelihood Ratio Test (LRT) for svyglm models
lrt_svyglm <- function(full_model, reduced_model, design) {
  
  # Approximate log-likelihood for a survey-weighted model
  calculate_loglik <- function(model, design) {
    # Extract model predictions and response
    predicted_probs <- predict(model, type = "response")
    observed <- model$y
    
    # Compute the log-likelihood contributions
    loglik_contrib <- observed * log(predicted_probs) + (1 - observed) * log(1 - predicted_probs)
    
    # Adjust for survey weights
    loglik_weighted <- sum(loglik_contrib * weights(design, type = "sampling"))
    return(loglik_weighted)
  }
  
  # Calculate log-likelihoods for both models
  logLik_full <- calculate_loglik(full_model, design)
  logLik_reduced <- calculate_loglik(reduced_model, design)
  
  # Compute the test statistic
  test_statistic <- -2 * (logLik_reduced - logLik_full)
  
  # Degrees of freedom difference
  df_diff <- df.residual(reduced_model) - df.residual(full_model)
  
  # Calculate p-value
  p_value <- pchisq(test_statistic, df = df_diff, lower.tail = FALSE)
  
  # Return the test statistic and p-value
  return(list(statistic = test_statistic, p_value = p_value))
}

# Metric-generating functions
# Ordinary R²
calculate_r2 <- function(predicted, actual) {
  ss_total <- sum((actual - mean(actual))^2)
  ss_residual <- sum((actual - predicted)^2)
  return(1 - (ss_residual / ss_total))
}

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
  predicted_probabilities <- predict(model, type = "response")
  
  # Calculate Brier score
  brier_score <- mean((predicted_probabilities - data$highbp14090_adj)^2)
  
  return(brier_score)
}

# c-statistic via ROC
calculate_auc <- function(model, data) {
  # Generate predicted probabilities
  predicted_probabilities <- predict(model, type = "response")
  
  # Calculate ROC curve
  roc_curve <- pROC::roc(data$highbp14090_adj, predicted_probabilities)
  
  # Calculate AUC
  auc_value <- pROC::auc(roc_curve)
  
  # Calculate 95% confidence interval for AUC
  ci_auc <- pROC::ci.auc(roc_curve)
  
  return(list(
    AUC = auc_value,
    CI = ci_auc
  ))
}

# Calibration - comparing observed and predicted probabilities, both in-the-whole and across percentiles
compare_probs <- function(data, predicted_probs) {
  # Add predicted probabilities to data
  data$predicted <- predicted_probs
  
  # Calculate overall observed and predicted probabilities
  overall_observed <- mean(data$highbp14090_adj)
  overall_predicted <- mean(data$predicted)
  
  # Relative difference overall
  relative_difference_overall <- (overall_observed - overall_predicted) / overall_observed
  
  # Calculate 90th and 10th percentiles of predicted probabilities
  percentile_90 <- quantile(data$predicted, 0.9)
  percentile_10 <- quantile(data$predicted, 0.1)
  
  # Observed and predicted probabilities for 90th and 10th percentiles
  observed_90 <- mean(data$highbp14090_adj[data$predicted >= percentile_90])
  predicted_90 <- mean(data$predicted[data$predicted >= percentile_90])
  observed_10 <- mean(data$highbp14090_adj[data$predicted <= percentile_10])
  predicted_10 <- mean(data$predicted[data$predicted <= percentile_10])
  
  # Relative differences for 90th and 10th percentiles
  relative_diff_90 <- (observed_90 - predicted_90) / observed_90
  relative_diff_10 <- (observed_10 - predicted_10) / observed_10
  
  # Ratio of relative differences for 90th vs 10th percentile
  ratio_90_10 <- abs(relative_diff_90 / relative_diff_10)
  
  # Calculate 95th and 5th percentiles of predicted probabilities
  percentile_95 <- quantile(data$predicted, 0.95)
  percentile_5 <- quantile(data$predicted, 0.05)
  
  # Observed and predicted probabilities for 95th and 5th percentiles
  observed_95 <- mean(data$highbp14090_adj[data$predicted >= percentile_95])
  predicted_95 <- mean(data$predicted[data$predicted >= percentile_95])
  observed_5 <- mean(data$highbp14090_adj[data$predicted <= percentile_5])
  predicted_5 <- mean(data$predicted[data$predicted <= percentile_5])
  
  # Relative differences for 95th and 5th percentiles
  relative_diff_95 <- (observed_95 - predicted_95) / observed_95
  relative_diff_5 <- (observed_5 - predicted_5) / observed_5
  
  # Ratio of relative differences for 95th vs 5th percentile
  ratio_95_5 <- abs(relative_diff_95 / relative_diff_5)
  
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
generate_predicted_probabilities <- function(model) {
  predicted_probabilities <- predict(model, type = "response")
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
  predicted_probs <- predict(boot_model, type = "response")
  
  # Calculate Nagelkerke's R²
  nagelkerke_r2 <- calculate_nagelkerke_r2(boot_model, data)
  
  # Calculate Brier Score
  brier_score <- calculate_brier_score(boot_model, data)
  
  # Calculate AUC and its CI using custom function
  auc_result <- calculate_auc(boot_model, data)
  auc_value <- auc_result$AUC
  auc_ci <- auc_result$CI
  
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
    auc_ci_lower = auc_ci[1],  # Lower bound of AUC CI
    auc_ci_upper = auc_ci[3],  # Upper bound of AUC CI
    relative_difference_overall = calibration_comparison$relative_difference_overall,
    ratio_90_10 = calibration_comparison$ratio_90_10,
    ratio_95_5 = calibration_comparison$ratio_95_5,
    calibration_slope = calibration_slope_value
  ))
}