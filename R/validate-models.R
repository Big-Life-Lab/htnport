# Set working directory at RDC
setwd("P:/10619/Dropbox/chmsflow")

# Load required packages
library(dplyr)
library(rms)
library(pROC)
library(ggplot2)

# Load this R file to obtain all datasets and reduced models
source("R/develop-models.R")

# Generate predictions for male and female models
generate_predictions <- function(model, data) {
  predictions <- predict(model, newdata = data, type = "response")
  return(predictions)
}

male_train_predictions <- generate_predictions(male_reduced_model, male_train_data)
male_test_predictions <- generate_predictions(male_reduced_model, male_test_data)
male_combined_predictions <- generate_predictions(male_reduced_model, male_data)

female_train_predictions <- generate_predictions(female_reduced_model, female_train_data)
female_test_predictions <- generate_predictions(female_reduced_model, female_test_data)
female_combined_predictions <- generate_predictions(female_reduced_model, female_data)

calculate_nagelkerke_r2 <- function(model, data) {
  # Get the number of observations
  n <- nrow(data)
  
  # Get the log-likelihood of the fitted model
  log_likelihood_fitted <- logLik(model)[1]
  
  # Get the log-likelihood of the null model
  null_model_fit <- glm(highbp14090_adj ~ 1, data = data, family = binomial())
  log_likelihood_null <- logLik(null_model_fit)[1]
  
  # Calculate the likelihood ratio statistic (LR)
  LR <- 2 * (log_likelihood_fitted - log_likelihood_null)
  
  # Calculate Nagelkerke's R2 using the formula
  nagelkerke_r2 <- (1 - exp(-LR / n)) / (1 - exp(-(-2 * log_likelihood_null) / n))
  
  return(nagelkerke_r2)
}

male_train_nagelkerke_r2 <- calculate_nagelkerke_r2(male_reduced_model, male_train_data)
male_test_nagelkerke_r2 <- calculate_nagelkerke_r2(male_reduced_model, male_test_data)
male_combined_nagelkerke_r2 <- calculate_nagelkerke_r2(male_reduced_model, male_data)

female_train_nagelkerke_r2 <- calculate_nagelkerke_r2(female_reduced_model, female_train_data)
female_test_nagelkerke_r2 <- calculate_nagelkerke_r2(female_reduced_model, female_test_data)
female_combined_nagelkerke_r2 <- calculate_nagelkerke_r2(female_reduced_model, female_data)

male_train_nagelkerke_r2
male_test_nagelkerke_r2
male_combined_nagelkerke_r2

female_train_nagelkerke_r2
female_test_nagelkerke_r2
female_combined_nagelkerke_r2

# Brier score
calculate_brier_score <- function(model, data) {
  # Generate predictions
  predictions <- predict(model, newdata = data, type = "response")
  
  # Calculate Brier score
  brier_score <- mean((predictions - data$highbp14090_adj)^2)
  
  return(brier_score)
}

male_train_brier_score <- calculate_brier_score(male_reduced_model, male_train_data)
male_test_brier_score <- calculate_brier_score(male_reduced_model, male_test_data)
male_combined_brier_score <- calculate_brier_score(male_reduced_model, male_data)

female_train_brier_score <- calculate_brier_score(female_reduced_model, female_train_data)
female_test_brier_score <- calculate_brier_score(female_reduced_model, female_test_data)
female_combined_brier_score <- calculate_brier_score(female_reduced_model, female_data)

male_train_brier_score
male_test_brier_score
male_combined_brier_score

female_train_brier_score
female_test_brier_score
female_combined_brier_score

# c-statistic via ROC
calculate_auc <- function(model, data) {
  # Generate predictions
  predictions <- predict(model, newdata = data, type = "response")
  
  # Calculate ROC curve and AUC
  auc_value <- auc(roc(data$highbp14090_adj, predictions))
  
  return(auc_value)
}

male_train_auc <- calculate_auc(male_reduced_model, male_train_data)
male_test_auc <- calculate_auc(male_reduced_model, male_test_data)
male_combined_auc <- calculate_auc(male_reduced_model, male_data)

female_train_auc <- calculate_auc(female_reduced_model, female_train_data)
female_test_auc <- calculate_auc(female_reduced_model, female_test_data)
female_combined_auc <- calculate_auc(female_reduced_model, female_data)

male_train_auc
male_test_auc
male_combined_auc

female_train_auc
female_test_auc
female_combined_auc

# Calibration - comparing observed and predicted probabilities
compare_probs <- function(data, predicted_probs) {
  # Add predicted probabilities to data
  data$predicted <- predicted_probs
  
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
  
  return(list(comparison_90_10 = comparison_90_10, comparison_95_5 = comparison_95_5))
}

obs_pred_comparison_male_train <- compare_probs(male_train_data, male_train_predictions)
obs_pred_comparison_male_test <- compare_probs(male_test_data, male_test_predictions)
obs_pred_comparison_male_combined <- compare_probs(male_data, male_combined_predictions)

obs_pred_comparison_female_train <- compare_probs(female_train_data, female_train_predictions)
obs_pred_comparison_female_test <- compare_probs(female_test_data, female_test_predictions)
obs_pred_comparison_female_combined <- compare_probs(female_data, female_combined_predictions)

print(obs_pred_comparison_male_train)
print(obs_pred_comparison_male_test)
print(obs_pred_comparison_male_combined)

print(obs_pred_comparison_female_train)
print(obs_pred_comparison_female_test)
print(obs_pred_comparison_female_combined)

# Calibration slopes
calibration_slope <- function(data, predicted_probs) {
  data$predicted <- predicted_probs
  slope_model <- lm(highbp14090_adj ~ predicted, data = data)
  return(summary(slope_model))
}

calibration_slope_male_train <- calibration_slope(male_train_data, male_train_predictions)
calibration_slope_male_test <- calibration_slope(male_test_data, male_test_predictions)
calibration_slope_male_combined <- calibration_slope(male_data, male_combined_predictions)

calibration_slope_female_train <- calibration_slope(female_train_data, female_train_predictions)
calibration_slope_female_test <- calibration_slope(female_test_data, female_test_predictions)
calibration_slope_female_combined <- calibration_slope(female_data, female_combined_predictions)

print(calibration_slope_male_train)
print(calibration_slope_male_test)
print(calibration_slope_male_combined)

print(calibration_slope_female_train)
print(calibration_slope_female_test)
print(calibration_slope_female_combined)

# Calibration plots with LOESS
plot_calibration <- function(data, predicted_probs, title) {
  data$predicted <- predicted_probs
  ggplot(data, aes(x = predicted, y = highbp14090_adj)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "loess", color = "blue") +
    labs(title = title, x = "Predicted Probability", y = "Observed Probability") +
    theme_minimal()
}

plot_calibration(male_train_data, male_train_predictions, "Calibration Plot for Male Train Data")
plot_calibration(male_test_data, male_test_predictions, "Calibration Plot for Male Test Data")
plot_calibration(male_data, male_combined_predictions, "Calibration Plot for Male Combined Data")

plot_calibration(female_train_data, female_train_predictions, "Calibration Plot for Female Train Data")
plot_calibration(female_test_data, female_test_predictions, "Calibration Plot for Female Test Data")
plot_calibration(female_data, female_combined_predictions, "Calibration Plot for Female Combined Data")