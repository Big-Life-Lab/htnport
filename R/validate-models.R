# Set working directory at RDC
setwd("P:/10619/Dropbox/chmsflow")

# Load this R file to obtain dataset and reduced models
source("R/develop-models.R")

# Load new packages
library(pROC)
library(ggplot2)

# Generate predicted_probabilities for male and female models
generate_predicted_probabilities <- function(model, data) {
  predicted_probabilities <- predict(model, newdata = data, type = "response")
  return(predicted_probabilities)
}

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
  auc_value <- auc(roc(data$highbp14090_adj, predicted_probabilities))
  
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

# Function to perform bootstrapping and calculate performance metrics
bootstrap_function <- function(data, indices) {
  # Create the bootstrap sample using indices
  boot_data <- data[indices, ]
  
  # Recreate the survey design with the bootstrap sample
  boot_design <- svydesign(ids = ~1, weights = ~weight_variable, data = boot_data)
  
  # Refit the svyglm model on the bootstrap sample
  boot_model <- svyglm(highbp14090_adj ~ rcs(clc_age, 4) + married + working +
                         rcs(hwmdbmi, 3) * whr + slp_11, 
                       design = boot_design, family = quasibinomial())
  
  # Generate predicted probabilities on the original (out-of-bag) data
  predicted_probs <- predict(boot_model, newdata = mydata, type = "response")
  
  # Calculate Nagelkerke's R²
  nagelkerke_r2 <- calculate_nagelkerke_r2(boot_model, mydata)
  
  # Calculate Brier Score
  brier_score <- calculate_brier_score(boot_model, mydata)
  
  # Calculate AUC
  auc_value <- calculate_auc(boot_model, mydata)
  
  # Calculate Calibration Slope
  slope_model <- calibration_slope(mydata, predicted_probs)
  calibration_slope_value <- slope_model$coefficients[2]  # Extract slope
  
  # Calibration Comparison (Deciles)
  calibration_comparison <- compare_probs(mydata, predicted_probs)
  
  # Return all performance metrics as a list
  return(list(
    nagelkerke_r2 = nagelkerke_r2,
    brier_score = brier_score,
    auc_value = auc_value,
    calibration_slope = calibration_slope_value,
    calibration_comparison_90_10 = calibration_comparison$comparison_90_10,
    calibration_comparison_95_5 = calibration_comparison$comparison_95_5
  ))
}

# Perform bootstrapping with 1000 replications
set.seed(123)
n_bootstrap <- 1000  # Number of bootstrap resamples
bootstrap_results <- replicate(n_bootstrap, {
  # Sample with replacement (same size as original data)
  boot_indices <- sample(1:nrow(mydata), replace = TRUE)
  bootstrap_function(mydata, boot_indices)
})

# Example: Aggregate and summarize results
nagelkerke_r2_values <- sapply(bootstrap_results, function(x) x$nagelkerke_r2)
brier_score_values <- sapply(bootstrap_results, function(x) x$brier_score)
auc_values <- sapply(bootstrap_results, function(x) x$auc_value)
calibration_slope_values <- sapply(bootstrap_results, function(x) x$calibration_slope)

# Print summarized results (means, CIs, etc.)
mean_nagelkerke_r2 <- mean(nagelkerke_r2_values)
mean_brier_score <- mean(brier_score_values)
mean_auc <- mean(auc_values)
mean_calibration_slope <- mean(calibration_slope_values)

cat("Mean Nagelkerke R²:", mean_nagelkerke_r2, "\n")
cat("Mean Brier Score:", mean_brier_score, "\n")
cat("Mean AUC:", mean_auc, "\n")
cat("Mean Calibration Slope:", mean_calibration_slope, "\n")

# Calibration plots with LOESS
plot_calibration <- function(data, predicted_probs, title) {
  data$predicted <- predicted_probs
  ggplot(data, aes(x = predicted, y = highbp14090_adj)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "loess", color = "blue") +
    labs(title = title, x = "Predicted Probability", y = "Observed Probability") +
    theme_minimal()
}

male_predicted_probabilities <- generate_predicted_probabilities(male_reduced_model, male_data)
female_predicted_probabilities <- generate_predicted_probabilities(female_reduced_model, female_data)

plot_calibration(male_data, male_predicted_probabilities, "Calibration Plot for Male Data")
plot_calibration(female_data, female_predicted_probabilities, "Calibration Plot for Female Data")