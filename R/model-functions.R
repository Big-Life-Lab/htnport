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

# Add Wilson CIs for observed proportions and SE-of-mean CIs for predicted means
# to a calibration data frame. Expects columns: n, Obs, Pavg.
add_calibration_cis <- function(cal_df, outcome, predicted, group_var, alpha = 0.05) {
  z <- qnorm(1 - alpha / 2)

  # Wilson CI for observed proportion
  x <- round(cal_df$Obs * cal_df$n)
  n <- cal_df$n
  denom <- n + z^2
  p_tilde <- (x + z^2 / 2) / denom
  margin <- (z / denom) * sqrt(x * (n - x) / n + z^2 / 4)
  cal_df$Obs_lower <- p_tilde - margin
  cal_df$Obs_upper <- p_tilde + margin

  # SE of mean for predicted risks per subgroup
  cal_df$Pavg_lower <- NA
  cal_df$Pavg_upper <- NA
  for (i in seq_len(nrow(cal_df))) {
    lvl <- as.character(cal_df$Subgroup[i])
    idx <- as.character(group_var) == lvl
    if (sum(idx) > 1) {
      p_sub <- predicted[idx]
      se_mean <- sd(p_sub) / sqrt(length(p_sub))
      t_crit <- qt(1 - alpha / 2, length(p_sub) - 1)
      cal_df$Pavg_lower[i] <- cal_df$Pavg[i] - t_crit * se_mean
      cal_df$Pavg_upper[i] <- cal_df$Pavg[i] + t_crit * se_mean
    }
  }
  cal_df
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

# Function to obtain bootstrap beta coefficients for a model
bootstrap_coefs <- function(data, indices, model) {
  # Create bootstrap sample
  boot_data <- data[indices, ]

  # Recreate survey design
  boot_design <- survey::svydesign(ids = ~1, weights = ~wgt_full, data = boot_data)

  # Refit model
  boot_model <- update(model, data = boot_data, design = boot_design)

  # Return coefficients
  return(coef(boot_model))
}

# Population estimate function for use with survey::withReplicates().
# Refits the model with replicate weights, computes weighted mean predicted risk
# overall and by subgroup (age 1yr, age 5yr, family history, BMI, diabetes),
# weighted percentiles, weighted O-E, and replicate coefficients.
pop_est <- function(w, data, formula, outcome = "highbp14090_adj",
                    age_var = "clc_age", age5yr_var = "age_group_5yr",
                    fmh_var = "fmh_15", bmi_var = "bmigroup",
                    diab_var = "diabx") {

  # Refit model with replicate weights
  data$.w <- w
  fit <- glm(formula, data = data, weights = .w, family = quasibinomial())
  p <- predict(fit, newdata = data, type = "response")
  obs <- data[[outcome]]

  # Weighted mean helper
  wmean <- function(x, wt) sum(wt * x) / sum(wt)

  # Overall
  overall_mean <- wmean(p, w)
  overall_obs <- wmean(obs, w)
  o_minus_e <- overall_obs - overall_mean

  # Mean predicted risk by 1-year age
  ages_1yr <- sort(unique(data[[age_var]]))
  mean_by_age1yr <- sapply(ages_1yr, function(a) {
    idx <- data[[age_var]] == a
    wmean(p[idx], w[idx])
  })
  names(mean_by_age1yr) <- paste0("age1yr_", ages_1yr)

  # Mean predicted risk by 5-year age group
  age5yr_levels <- levels(data[[age5yr_var]])
  mean_by_age5yr <- sapply(age5yr_levels, function(g) {
    idx <- data[[age5yr_var]] == g
    wmean(p[idx], w[idx])
  })
  names(mean_by_age5yr) <- paste0("age5yr_", age5yr_levels)

  # Mean predicted risk by family history
  fmh_levels <- sort(unique(data[[fmh_var]]))
  mean_by_fmh <- sapply(fmh_levels, function(lvl) {
    idx <- data[[fmh_var]] == lvl
    wmean(p[idx], w[idx])
  })
  names(mean_by_fmh) <- paste0("fmh_", fmh_levels)

  # Mean predicted risk by BMI group
  bmi_levels <- sort(unique(data[[bmi_var]]))
  mean_by_bmi <- sapply(bmi_levels, function(lvl) {
    idx <- data[[bmi_var]] == lvl
    wmean(p[idx], w[idx])
  })
  names(mean_by_bmi) <- paste0("bmi_", bmi_levels)

  # Mean predicted risk by diabetes status
  diab_levels <- sort(unique(data[[diab_var]]))
  mean_by_diab <- sapply(diab_levels, function(lvl) {
    idx <- data[[diab_var]] == lvl
    wmean(p[idx], w[idx])
  })
  names(mean_by_diab) <- paste0("diab_", diab_levels)

  # Weighted percentiles
  pct_probs <- c(0.05, 0.10, 0.20, 0.25, 0.30, 0.40, 0.50, 0.60, 0.75, 0.80, 0.90, 0.95)
  ord <- order(p)
  p_sorted <- p[ord]
  w_sorted <- w[ord]
  cum_w <- cumsum(w_sorted) / sum(w_sorted)
  w_pctiles <- sapply(pct_probs, function(q) p_sorted[which(cum_w >= q)[1]])
  names(w_pctiles) <- paste0("pct_", pct_probs * 100)

  # Replicate coefficients
  betas <- coef(fit)
  names(betas) <- paste0("beta_", names(betas))

  c(overall_mean = overall_mean, overall_obs = overall_obs, o_minus_e = o_minus_e,
    mean_by_age1yr, mean_by_age5yr, mean_by_fmh, mean_by_bmi, mean_by_diab,
    w_pctiles, betas)
}

# Extract estimates with CIs from a withReplicates result object.
# Greps for named elements matching `pattern` and returns a data frame
# with point estimate, SE, and 95% CI using t(degf).
extract_ci <- function(res, pattern, degf = 68) {
  est <- coef(res)
  se <- SE(res)
  idx <- grep(pattern, names(est))
  t_crit <- qt(0.975, degf)
  data.frame(
    name = names(est)[idx],
    estimate = est[idx],
    se = se[idx],
    ci_lower = est[idx] - t_crit * se[idx],
    ci_upper = est[idx] + t_crit * se[idx],
    row.names = NULL
  )
}

# Extract replicate-weight coefficient sets from a withReplicates result.
# Returns a matrix with one row per replicate, columns = model coefficients.
extract_replicate_betas <- function(res) {
  reps <- res$replicates
  beta_cols <- grep("^beta_", colnames(reps))
  betas <- reps[, beta_cols]
  colnames(betas) <- gsub("^beta_", "", colnames(betas))
  rownames(betas) <- paste0("replicate_", seq_len(nrow(betas)))
  betas
}

# Build 1-year age summary with CIs from withReplicates results (full + reduced).
# Returns a data frame with one row per age, columns for full and reduced estimates + CIs.
build_age1yr_summary <- function(res_full, res_reduced) {
  full_ci <- extract_ci(res_full, "^age1yr_")
  reduced_ci <- extract_ci(res_reduced, "^age1yr_")
  full_ci$clc_age <- as.numeric(gsub("age1yr_", "", full_ci$name))
  reduced_ci$clc_age <- as.numeric(gsub("age1yr_", "", reduced_ci$name))
  merge(
    full_ci[, c("clc_age", "estimate", "ci_lower", "ci_upper")],
    reduced_ci[, c("clc_age", "estimate", "ci_lower", "ci_upper")],
    by = "clc_age", suffixes = c("_full", "_reduced")
  )
}

# Build 5-year age group summary with CIs from withReplicates results (full + reduced).
# Returns a data frame with one row per age group, columns for full and reduced estimates + CIs.
build_age5yr_summary <- function(res_full, res_reduced) {
  full_ci <- extract_ci(res_full, "^age5yr_")
  reduced_ci <- extract_ci(res_reduced, "^age5yr_")
  full_ci$age_group_5yr <- gsub("age5yr_", "", full_ci$name)
  reduced_ci$age_group_5yr <- gsub("age5yr_", "", reduced_ci$name)

  summary_df <- merge(
    full_ci[, c("age_group_5yr", "estimate", "ci_lower", "ci_upper")],
    reduced_ci[, c("age_group_5yr", "estimate", "ci_lower", "ci_upper")],
    by = "age_group_5yr", suffixes = c("_full", "_reduced")
  )
  summary_df
}

# Build predicted percentile summary with CIs from a withReplicates result.
build_percentile_summary <- function(res) {
  extract_ci(res, "^pct_")
}

# Build population mean predicted risks with CIs across all 4 models (full/reduced × male/female).
# Returns a data frame with one row per subgroup (overall, age groups, family history, BMI, diabetes).
build_pop_mean_risks <- function(res_male_full, res_female_full, res_male_reduced, res_female_reduced) {
  patterns <- c("^overall_mean$", "^age5yr_", "^fmh_", "^bmi_", "^diab_")
  results <- list()
  for (pat in patterns) {
    mf <- extract_ci(res_male_full, pat)
    ff <- extract_ci(res_female_full, pat)
    mr <- extract_ci(res_male_reduced, pat)
    fr <- extract_ci(res_female_reduced, pat)
    combined <- data.frame(
      group = mf$name,
      male_full = mf$estimate, male_full_lower = mf$ci_lower, male_full_upper = mf$ci_upper,
      female_full = ff$estimate, female_full_lower = ff$ci_lower, female_full_upper = ff$ci_upper,
      male_reduced = mr$estimate, male_reduced_lower = mr$ci_lower, male_reduced_upper = mr$ci_upper,
      female_reduced = fr$estimate, female_reduced_lower = fr$ci_lower, female_reduced_upper = fr$ci_upper
    )
    results <- c(results, list(combined))
  }
  do.call(rbind, results)
}