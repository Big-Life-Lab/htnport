# Set working directory at RDC
setwd("P:/10619/Dropbox/htnport")

# Load this R file to obtain dataset and reduced models, as well as calibration plot function
source("R/develop-models.R")
source("R/calibration.R")

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

# Model assessment for original sample
# Generate predicted probabilities for models
generate_predicted_probabilities <- function(model, data) {
  predicted_probabilities <- predict(model, newdata = data, type = "response")
  return(predicted_probabilities)
}
male_predicted_probabilities <- generate_predicted_probabilities(male_reduced_model, male_data)
female_predicted_probabilities <- generate_predicted_probabilities(female_reduced_model, female_data)

# Calibration plots with performance metrics
calibration(male_predicted_probabilities, male_data$highbp14090_adj)
calibration(female_predicted_probabilities, female_data$highbp14090_adj)

# Calibration in-the-whole and across percentiles
compare_probs(male_data, male_predicted_probabilities)
compare_probs(female_data, female_predicted_probabilities)

# Model assessment for bootstrap samples - BOOTSTRAP VALIDATION
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

# Perform bootstrapping for both male and female models
set.seed(123)
n_bootstrap <- 10  # Number of bootstrap resamples

# For male model
bootstrap_results_male <- replicate(n_bootstrap, {
  boot_indices <- sample(1:nrow(male_data), replace = TRUE)
  bootstrap_function(male_data, boot_indices, male_reduced_model)
}, simplify = FALSE)

# For female model
bootstrap_results_female <- replicate(n_bootstrap, {
  boot_indices <- sample(1:nrow(female_data), replace = TRUE)
  bootstrap_function(female_data, boot_indices, female_reduced_model)
}, simplify = FALSE)

# Aggregate and summarize results for male
nagelkerke_r2_male <- sapply(bootstrap_results_male, function(x) x$nagelkerke_r2)
brier_score_male <- sapply(bootstrap_results_male, function(x) x$brier_score)
auc_male <- sapply(bootstrap_results_male, function(x) x$auc_value)
relative_difference_overall_male <- sapply(bootstrap_results_male, function(x) x$relative_difference_overall)
ratio_90_10_male <- sapply(bootstrap_results_male, function(x) x$ratio_90_10)
ratio_95_5_male <- sapply(bootstrap_results_male, function(x) x$ratio_95_5)
calibration_slope_male <- sapply(bootstrap_results_male, function(x) x$calibration_slope)

mean_nagelkerke_r2_male <- mean(nagelkerke_r2_male)
mean_brier_score_male <- mean(brier_score_male)
mean_auc_male <- mean(auc_male)
mean_relative_difference_overall_male <- mean(relative_difference_overall_male)
mean_ratio_90_10_male <- mean(ratio_90_10_male)
mean_ratio_95_5_male <- mean(ratio_95_5_male)
mean_calibration_slope_male <- mean(calibration_slope_male)

cat("Male Model:\n")
cat("Mean Nagelkerke R²:", mean_nagelkerke_r2_male, "\n")
cat("Mean Brier Score:", mean_brier_score_male, "\n")
cat("Mean AUC:", mean_auc_male, "\n")
cat("Mean Observed v. Predicted:", mean_relative_difference_overall_male, "\n")
cat("Mean 90:10 Observed v. Predicted:", mean_ratio_90_10_male, "\n")
cat("Mean 95:5 Observed v. Predicted:", mean_ratio_95_5_male, "\n")
cat("Mean Calibration Slope:", mean_calibration_slope_male, "\n")

# Aggregate and summarize results for female
nagelkerke_r2_female <- sapply(bootstrap_results_female, function(x) x$nagelkerke_r2)
brier_score_female <- sapply(bootstrap_results_female, function(x) x$brier_score)
auc_female <- sapply(bootstrap_results_female, function(x) x$auc_value)
relative_difference_overall_female <- sapply(bootstrap_results_female, function(x) x$relative_difference_overall)
ratio_90_10_female <- sapply(bootstrap_results_female, function(x) x$ratio_90_10)
ratio_95_5_female <- sapply(bootstrap_results_female, function(x) x$ratio_95_5)
calibration_slope_female <- sapply(bootstrap_results_female, function(x) x$calibration_slope)

mean_nagelkerke_r2_female <- mean(nagelkerke_r2_female)
mean_brier_score_female <- mean(brier_score_female)
mean_auc_female <- mean(auc_female)
mean_relative_difference_overall_female <- mean(relative_difference_overall_female)
mean_ratio_90_10_female <- mean(ratio_90_10_female)
mean_ratio_95_5_female <- mean(ratio_95_5_female)
mean_calibration_slope_female <- mean(calibration_slope_female)

cat("Female Model:\n")
cat("Mean Nagelkerke R²:", mean_nagelkerke_r2_female, "\n")
cat("Mean Brier Score:", mean_brier_score_female, "\n")
cat("Mean AUC:", mean_auc_female, "\n")
cat("Mean Observed v. Predicted:", mean_relative_difference_overall_female, "\n")
cat("Mean 90:10 Observed v. Predicted:", mean_ratio_90_10_female, "\n")
cat("Mean 95:5 Observed v. Predicted:", mean_ratio_95_5_female, "\n")
cat("Mean Calibration Slope:", mean_calibration_slope_female, "\n")