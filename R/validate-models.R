# Set working directory at RDC
setwd("P:/10619/Dropbox/htnport")

# Load this R file to obtain dataset and reduced models
source("R/develop-models.R")
source("R/calibration.R")

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

# Calibration - comparing observed and predicted probabilities
compare_probs <- function(data, predicted_probs) {
  # Add predicted probabilities to data
  data$predicted <- predicted_probs
  
  # Compare observed and predicted probabilities as a whole
  comparison_whole <- data %>%
    dplyr::select(Observed = highbp14090_adj, Predicted = predicted)
  
  # Bin predicted probabilities into 90:10 and 95:5 deciles
  data$decile_90_10 <- cut(data$predicted, breaks = quantile(data$predicted, probs = seq(0, 1, 0.1)), include.lowest = TRUE)
  data$decile_95_5 <- cut(data$predicted, breaks = quantile(data$predicted, probs = seq(0, 1, 0.05)), include.lowest = TRUE)
  
  # Compare observed and predicted probabilities by 90:10 decile
  comparison_90_10 <- data %>%
    dplyr::group_by(decile_90_10) %>%
    dplyr::summarize(Observed = mean(highbp14090_adj), Predicted = mean(predicted), .groups = 'drop')
  
  # Compare observed and predicted probabilities by 95:5 decile
  comparison_95_5 <- data %>%
    dplyr::group_by(decile_95_5) %>%
    dplyr::summarize(Observed = mean(highbp14090_adj), Predicted = mean(predicted), .groups = 'drop')
  
  return(list(comparison_whole = comparison_whole, comparison_90_10 = comparison_90_10, comparison_95_5 = comparison_95_5))
}

# Calibration slopes
calibration_slope <- function(data, predicted_probs) {
  data$predicted <- predicted_probs
  slope_model <- lm(highbp14090_adj ~ predicted, data = data)
  return(summary(slope_model))
}

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
  
  # Calibration Comparison (Whole and Deciles)
  calibration_comparison <- compare_probs(data, predicted_probs)
  
  # Calculate Calibration Slope
  slope_model <- calibration_slope(data, predicted_probs)
  calibration_slope_value <- slope_model$coefficients[2]  # Extract slope
  
  # Return all performance metrics as a list
  return(list(
    nagelkerke_r2 = nagelkerke_r2,
    brier_score = brier_score,
    auc_value = auc_value,
    calibration_comparison_whole_obs = mean(calibration_comparison$comparison_whole$Observed),
    calibration_comparison_whole_pred = mean(calibration_comparison$comparison_whole$Predicted),
    calibration_comparison_90_10_obs = mean(calibration_comparison$comparison_90_10$Observed),
    calibration_comparison_90_10_pred = mean(calibration_comparison$comparison_90_10$Predicted),
    calibration_comparison_95_5_obs = mean(calibration_comparison$comparison_95_5$Observed),
    calibration_comparison_95_5_pred = mean(calibration_comparison$comparison_95_5$Predicted),
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
calibration_comparison_whole_obs_male <- sapply(bootstrap_results_male, function(x) x$calibration_comparison_whole_obs)
calibration_comparison_whole_pred_male <- sapply(bootstrap_results_male, function(x) x$calibration_comparison_whole_pred)
calibration_comparison_90_10_obs_male <- sapply(bootstrap_results_male, function(x) x$calibration_comparison_90_10_obs)
calibration_comparison_90_10_pred_male <- sapply(bootstrap_results_male, function(x) x$calibration_comparison_90_10_pred)
calibration_comparison_95_5_obs_male <- sapply(bootstrap_results_male, function(x) x$calibration_comparison_95_5_obs)
calibration_comparison_95_5_pred_male <- sapply(bootstrap_results_male, function(x) x$calibration_comparison_95_5_pred)
calibration_slope_male <- sapply(bootstrap_results_male, function(x) x$calibration_slope)

mean_nagelkerke_r2_male <- mean(nagelkerke_r2_male)
mean_brier_score_male <- mean(brier_score_male)
mean_auc_male <- mean(auc_male)
mean_comparison_whole_obs_male <- mean(calibration_comparison_whole_obs_male)
mean_comparison_whole_pred_male <- mean(calibration_comparison_whole_pred_male)
mean_comparison_90_10_obs_male <- mean(calibration_comparison_90_10_obs_male)
mean_comparison_90_10_pred_male <- mean(calibration_comparison_90_10_pred_male)
mean_comparison_95_5_obs_male <- mean(calibration_comparison_95_5_obs_male)
mean_comparison_95_5_pred_male <- mean(calibration_comparison_95_5_pred_male)
mean_calibration_slope_male <- mean(calibration_slope_male)

cat("Male Model:\n")
cat("Mean Nagelkerke R²:", mean_nagelkerke_r2_male, "\n")
cat("Mean Brier Score:", mean_brier_score_male, "\n")
cat("Mean AUC:", mean_auc_male, "\n")
cat("Mean Whole Observation:", mean_comparison_whole_obs_male, "\n")
cat("Mean Whole Prediction:", mean_comparison_whole_pred_male, "\n")
cat("Mean 90-10 Observation:", mean_comparison_90_10_obs_male, "\n")
cat("Mean 90-10 Prediction:", mean_comparison_90_10_pred_male, "\n")
cat("Mean 95-5 Observation:", mean_comparison_95_5_obs_male, "\n")
cat("Mean 95-5 Prediction:", mean_comparison_95_5_pred_male, "\n")
cat("Mean Calibration Slope:", mean_calibration_slope_male, "\n")

# Aggregate and summarize results for female
nagelkerke_r2_female <- sapply(bootstrap_results_female, function(x) x$nagelkerke_r2)
brier_score_female <- sapply(bootstrap_results_female, function(x) x$brier_score)
auc_female <- sapply(bootstrap_results_female, function(x) x$auc_value)
calibration_comparison_whole_obs_female <- sapply(bootstrap_results_female, function(x) x$calibration_comparison_whole_obs)
calibration_comparison_whole_pred_female <- sapply(bootstrap_results_female, function(x) x$calibration_comparison_whole_pred)
calibration_comparison_90_10_obs_female <- sapply(bootstrap_results_female, function(x) x$calibration_comparison_90_10_obs)
calibration_comparison_90_10_pred_female <- sapply(bootstrap_results_female, function(x) x$calibration_comparison_90_10_pred)
calibration_comparison_95_5_obs_female <- sapply(bootstrap_results_female, function(x) x$calibration_comparison_95_5_obs)
calibration_comparison_95_5_pred_female <- sapply(bootstrap_results_female, function(x) x$calibration_comparison_95_5_pred)
calibration_slope_female <- sapply(bootstrap_results_female, function(x) x$calibration_slope)

mean_nagelkerke_r2_female <- mean(nagelkerke_r2_female)
mean_brier_score_female <- mean(brier_score_female)
mean_auc_female <- mean(auc_female)
mean_comparison_whole_obs_female <- mean(calibration_comparison_whole_obs_female)
mean_comparison_whole_pred_female <- mean(calibration_comparison_whole_pred_female)
mean_comparison_90_10_obs_female <- mean(calibration_comparison_90_10_obs_female)
mean_comparison_90_10_pred_female <- mean(calibration_comparison_90_10_pred_female)
mean_comparison_95_5_obs_female <- mean(calibration_comparison_95_5_obs_female)
mean_comparison_95_5_pred_female <- mean(calibration_comparison_95_5_pred_female)
mean_calibration_slope_female <- mean(calibration_slope_female)

cat("Female Model:\n")
cat("Mean Nagelkerke R²:", mean_nagelkerke_r2_female, "\n")
cat("Mean Brier Score:", mean_brier_score_female, "\n")
cat("Mean AUC:", mean_auc_female, "\n")
cat("Mean Whole Observation:", mean_comparison_whole_obs_female, "\n")
cat("Mean Whole Prediction:", mean_comparison_whole_pred_female, "\n")
cat("Mean 90-10 Observation:", mean_comparison_90_10_obs_female, "\n")
cat("Mean 90-10 Prediction:", mean_comparison_90_10_pred_female, "\n")
cat("Mean 95-5 Observation:", mean_comparison_95_5_obs_female, "\n")
cat("Mean 95-5 Prediction:", mean_comparison_95_5_pred_female, "\n")
cat("Mean Calibration Slope:", mean_calibration_slope_female)

# Generate predicted_probabilities for models
generate_predicted_probabilities <- function(model, data) {
  predicted_probabilities <- predict(model, newdata = data, type = "response")
  return(predicted_probabilities)
}

male_predicted_probabilities <- generate_predicted_probabilities(male_reduced_model, male_data)
female_predicted_probabilities <- generate_predicted_probabilities(female_reduced_model, female_data)

# Calibration plots with LOESS, and model assessment across original sample
calibration(male_predicted_probabilities, male_data$highbp14090_adj)
calibration(female_predicted_probabilities, female_data$highbp14090_adj)