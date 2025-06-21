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
      calculate_r2(reduced_predictions, data$highbp14090_adj)
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
  predicted_probabilities <- predict(model, newdata = data, type = "response")
  
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
  
  # Ratio of 90th vs 10th percentile
  ratio_90_10 <- percentile_90 / percentile_10
  
  # Calculate 95th and 5th percentiles of predicted probabilities
  percentile_95 <- quantile(data$predicted, 0.95)
  percentile_5 <- quantile(data$predicted, 0.05)
  
  # Ratio of 95th vs 5th percentile
  ratio_95_5 <- percentile_95 / percentile_5
  
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
  nagelkerke_r2 <- calculate_nagelkerke_r2(boot_model, boot_data)
  
  # Calculate Brier Score
  brier_score <- calculate_brier_score(boot_model, boot_data)
  
  # Calculate c-statistic and its CI using custom function on bootstrap sample
  auc_value <- calculate_auc(boot_model, boot_data)$AUC
  auc_ci <- calculate_auc(boot_model, boot_data)$CI
  
  # Calculate c-statistic optimism
  auc_original <- calculate_auc(boot_model, data)$AUC
  auc_optimism <- auc_value - auc_original
  
  # Calibration Comparison (Relative Differences and Ratios)
  calibration_comparison <- compare_probs(boot_data, predicted_probs)
  
  # Calculate Calibration Slope
  slope_model <- calibration_slope(boot_data, predicted_probs)
  calibration_slope_value <- slope_model$coefficients[2]  # Extract slope
  
  # Return all performance metrics as a list
  return(list(
    nagelkerke_r2 = nagelkerke_r2,
    brier_score = brier_score,
    auc_value = auc_value,
    auc_ci_lower = auc_ci[1],  # Lower bound of AUC CI
    auc_ci_upper = auc_ci[3],  # Upper bound of AUC CI
    auc_optimism = auc_optimism,  # <<< Added this output
    relative_difference_overall = calibration_comparison$relative_difference_overall,
    ratio_90_10 = calibration_comparison$ratio_90_10,
    ratio_95_5 = calibration_comparison$ratio_95_5,
    calibration_slope = calibration_slope_value
  ))
}

# Function to obtain beta coefficients from a model
extract_beta_coefs <- function(model) {
  coef_summary <- summary(model)$coefficients  # Extract coefficients and SEs
  
  data.frame(
    Variable = rownames(coef_summary),  # Extract coefficient names
    Beta = coef_summary[, "Estimate"],  # Extract beta coefficients
    SE = coef_summary[, "Std. Error"],  # Extract standard errors
    row.names = NULL  # Remove row names
  )
}

# Function to calculate Shap-adjusted ORs and CIs
calculate_shap_or_ci <- function(shap_values_df, predictor) {
  # Define custom cutoffs for continuous variables
  cutoffs <- list(
    clc_age = c(-Inf, 40, 60, 70, Inf),     # 20-39, 40-59, 60-69, 70-79
    hwmdbmi = c(-Inf, 25, 30, Inf),         # <25, 25-<30, ≥30
    whr = c(-Inf, 50, 60, Inf),             # <50, 50-<60, ≥60
    minperweek = c(-Inf, 150, Inf),         # <150, ≥150
    totalfv = c(-Inf, 5, Inf),              # <5, ≥5
    slp_11 = c(-Inf, 7, Inf)                # <7, ≥7
  )
  
  # Filter SHAP values for the predictor
  shap_filtered <- shap_values_df %>% filter(feature == predictor)
  
  if (nrow(shap_filtered) == 0) {
    return(list(S_OR = NaN, CI = c(NaN, NaN)))
  }
  
  unique_values <- sort(unique(shap_filtered$feature.value))
  
  # Case 1: Continuous variable with predefined cutoffs
  if (predictor %in% names(cutoffs)) {
    shap_filtered <- shap_filtered %>%
      mutate(
        shap_category = cut(as.numeric(as.character(sub(".*=", "", feature.value))),
                            breaks = cutoffs[[predictor]],
                            labels = FALSE, include.lowest = TRUE, right = FALSE)
      )
    
    # Set reference category
    ref_category <- if (predictor %in% c("minperweek", "totalfv", "slp_11")) 2 else min(shap_filtered$shap_category)
    
    ref_shap <- shap_filtered %>% filter(shap_category == ref_category)
    ref_mean <- mean(ref_shap$phi)
    
    # Standard error: includes both between-person variance and mean MC variance
    ref_se <- sqrt(var(ref_shap$phi) + mean(ref_shap$phi.var)) / sqrt(nrow(ref_shap))
    
    results <- list()
    
    for (cat in sort(unique(shap_filtered$shap_category)[unique(shap_filtered$shap_category) != ref_category])) {
      cat_shap <- shap_filtered %>% filter(shap_category == cat)
      cat_mean <- mean(cat_shap$phi)
      
      cat_se <- sqrt(var(cat_shap$phi) + mean(cat_shap$phi.var)) / sqrt(nrow(cat_shap))
      
      mean_diff <- cat_mean - ref_mean
      se_diff <- sqrt(ref_se^2 + cat_se^2)
      
      S_OR <- exp(mean_diff)
      CI <- exp(c(mean_diff - 1.96 * se_diff, mean_diff + 1.96 * se_diff))
      
      results[[paste0(predictor, "=", cat)]] <- list(S_OR = S_OR, CI = CI)
    }
    
    return(results)
    
    # Case 2: Categorical variable with <= 4 unique values
  } else if (length(unique_values) > 1 && length(unique_values) <= 4) {
    ref_category <- min(unique_values)
    ref_shap <- shap_filtered %>% filter(feature.value == ref_category)
    ref_mean <- mean(ref_shap$phi)
    ref_se <- sqrt(var(ref_shap$phi) + mean(ref_shap$phi.var)) / sqrt(nrow(ref_shap))
    
    results <- list()
    
    for (cat in unique_values[unique_values != ref_category]) {
      cat_shap <- shap_filtered %>% filter(feature.value == cat)
      cat_mean <- mean(cat_shap$phi)
      cat_se <- sqrt(var(cat_shap$phi) + mean(cat_shap$phi.var)) / sqrt(nrow(cat_shap))
      
      mean_diff <- cat_mean - ref_mean
      se_diff <- sqrt(ref_se^2 + cat_se^2)
      
      S_OR <- exp(mean_diff)
      CI <- exp(c(mean_diff - 1.96 * se_diff, mean_diff + 1.96 * se_diff))
      
      results[[as.character(cat)]] <- list(S_OR = S_OR, CI = CI)
    }
    
    return(results)
    
    # Case 3: Continuous variable without specified cutoffs
  } else {
    mean_shap <- mean(shap_filtered$phi)
    se_shap <- sqrt(var(shap_filtered$phi) + mean(shap_filtered$phi.var)) / sqrt(nrow(shap_filtered))
    
    S_OR <- exp(mean_shap)
    CI <- exp(c(mean_shap - 1.96 * se_shap, mean_shap + 1.96 * se_shap))
    
    result <- list()
    result[[predictor]] <- list(S_OR = S_OR, CI = CI)
    return(result)
  }
}

# Function to plot ORs for age interactions
plot_age_int_or <- function(model, var, var_type = c("categorical", "continuous"),
                            design = NULL, sex_label = "Group",
                            title_labels = NULL, legend_labels = NULL,
                            level_labels = NULL, qval_round = 2) {
  
  var_type <- match.arg(var_type)
  
  title_label <- if (!is.null(title_labels) && var %in% names(title_labels)) title_labels[[var]] else var
  legend_label <- if (!is.null(legend_labels) && var %in% names(legend_labels)) legend_labels[[var]] else var
  
  if (var_type == "categorical") {
    
    # preds <- ggpredict(model, terms = c("clc_age", var)) %>%
    #   group_by(x) %>%
    #   arrange(group) %>%
    #   mutate(
    #     predicted_ref = first(predicted),
    #     odds = predicted / (1 - predicted),
    #     odds_ref = predicted_ref / (1 - predicted_ref),
    #     or = odds / odds_ref,
    #     or_low = (conf.low / (1 - conf.low)) / odds_ref,
    #     or_high = (conf.high / (1 - conf.high)) / odds_ref
    #   ) %>%
    #   ungroup() %>%
    #   filter(group != first(group))  # Remove reference group
    
    preds <- ggpredict(model, terms = c("clc_age", var)) %>%
      group_by(x) %>%
      arrange(group) %>%
      mutate(
        log_odds = log(predicted / (1 - predicted)),
        log_odds_ref = first(log_odds),
        se_ref = first(std.error),
        log_or = log_odds - log_odds_ref,
        se_log_or = sqrt(std.error^2 + se_ref^2),
        or = exp(log_or),
        or_low = exp(log_or - 1.96 * se_log_or),
        or_high = exp(log_or + 1.96 * se_log_or)
      ) %>%
      ungroup() %>%
      filter(group != first(group))  # Remove reference group
    
    p <- ggplot(preds, aes(x = x, y = or, color = group, fill = group)) +
      geom_line(size = 1.2) +
      geom_ribbon(aes(ymin = or_low, ymax = or_high), alpha = 0.2, color = NA) +
      geom_hline(yintercept = 1, linetype = "dotted") +
      scale_y_log10() +
      labs(
        title = paste0("Odds ratio by age for ", title_label, " in ", sex_label),
        x = "Age (years)",
        y = "Adjusted odds ratio",
        color = legend_label,
        fill = legend_label
      ) +
      scale_color_discrete(name = legend_label, labels = if (!is.null(level_labels)) level_labels[[var]] else NULL) +
      scale_fill_discrete(name = legend_label, labels = if (!is.null(level_labels)) level_labels[[var]] else NULL) +
      theme_minimal()
    
  } else if (var_type == "continuous") {
    
    ref_vals <- list(
      "hwmdbmi" = 18.5,
      "minperweek" = 0,
      "slp_11" = 2
    )
    comp_vals <- list(
      "hwmdbmi" = c(25, 30),
      "minperweek" = c(75, 150),
      "slp_11" = c(4.5, 7)
    )
    
    ref_val <- ref_vals[[var]]
    comps <- comp_vals[[var]]
    terms_string <- paste0(var, " [", paste(c(ref_val, comps), collapse = ", "), "]")
    
    # preds <- ggpredict(model, terms = c("clc_age", terms_string)) %>%
    #   group_by(x) %>%
    #   arrange(group) %>%
    #   mutate(
    #     predicted_ref = first(predicted),
    #     odds = predicted / (1 - predicted),
    #     odds_ref = predicted_ref / (1 - predicted_ref),
    #     or = odds / odds_ref,
    #     or_low = (conf.low / (1 - conf.low)) / odds_ref,
    #     or_high = (conf.high / (1 - conf.high)) / odds_ref
    #   ) %>%
    #   ungroup() %>%
    #   filter(group != first(group))  # Remove reference group
    
    preds <- ggpredict(model, terms = c("clc_age", terms_string)) %>%
      group_by(x) %>%
      arrange(group) %>%
      mutate(
        log_odds = log(predicted / (1 - predicted)),
        log_odds_ref = first(log_odds),
        se_ref = first(std.error),
        log_or = log_odds - log_odds_ref,
        se_log_or = sqrt(std.error^2 + se_ref^2),
        or = exp(log_or),
        or_low = exp(log_or - 1.96 * se_log_or),
        or_high = exp(log_or + 1.96 * se_log_or)
      ) %>%
      ungroup() %>%
      filter(group != first(group))  # Remove reference group
    
    p <- ggplot(preds, aes(x = x, y = or, color = group, fill = group, group = group)) +
      geom_line(size = 1.2) +
      geom_ribbon(aes(ymin = or_low, ymax = or_high), alpha = 0.2, color = NA) +
      geom_hline(yintercept = 1, linetype = "dotted") +
      scale_y_log10() +
      labs(
        title = paste0("Odds ratio by age for ", title_label, " in ", sex_label),
        x = "Age (years)",
        y = "Adjusted odds ratio",
        color = legend_label,
        fill = legend_label
      ) +
      theme_minimal()
  }
  
  return(p)
}