# Set working directory at RDC
setwd("P:/10619/Dropbox/chmsflow")

# Load this R file to obtain all datasets and reduced models, as well as Nagelkerke's R2 function
source("R/develop-models.R")

# Load new packages
library(pROC)
library(ggplot2)
library(FSelectorRcpp)

# Generate predicted_probabilities for male and female models
generate_predicted_probabilities <- function(model, data) {
  predicted_probabilities <- predict(model, newdata = data, type = "response")
  return(predicted_probabilities)
}

male_train_predicted_probabilities <- generate_predicted_probabilities(male_reduced_model, male_train_data)
male_test_predicted_probabilities <- generate_predicted_probabilities(male_reduced_model, male_test_data)
male_combined_predicted_probabilities <- generate_predicted_probabilities(male_reduced_model, male_data)

female_train_predicted_probabilities <- generate_predicted_probabilities(female_reduced_model, female_train_data)
female_test_predicted_probabilities <- generate_predicted_probabilities(female_reduced_model, female_test_data)
female_combined_predicted_probabilities <- generate_predicted_probabilities(female_reduced_model, female_data)

# Nagelkerke's R2
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
  # Generate predicted_probabilities
  predicted_probabilities <- predict(model, newdata = data, type = "response")
  
  # Calculate Brier score
  brier_score <- mean((predicted_probabilities - data$highbp14090_adj)^2)
  
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
  # Generate predicted_probabilities
  predicted_probabilities <- predict(model, newdata = data, type = "response")
  
  # Calculate ROC curve and AUC
  auc_value <- auc(roc(data$highbp14090_adj, predicted_probabilities))
  
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

obs_pred_comparison_male_train <- compare_probs(male_train_data, male_train_predicted_probabilities)
obs_pred_comparison_male_test <- compare_probs(male_test_data, male_test_predicted_probabilities)
obs_pred_comparison_male_combined <- compare_probs(male_data, male_combined_predicted_probabilities)

obs_pred_comparison_female_train <- compare_probs(female_train_data, female_train_predicted_probabilities)
obs_pred_comparison_female_test <- compare_probs(female_test_data, female_test_predicted_probabilities)
obs_pred_comparison_female_combined <- compare_probs(female_data, female_combined_predicted_probabilities)

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

calibration_slope_male_train <- calibration_slope(male_train_data, male_train_predicted_probabilities)
calibration_slope_male_test <- calibration_slope(male_test_data, male_test_predicted_probabilities)
calibration_slope_male_combined <- calibration_slope(male_data, male_combined_predicted_probabilities)

calibration_slope_female_train <- calibration_slope(female_train_data, female_train_predicted_probabilities)
calibration_slope_female_test <- calibration_slope(female_test_data, female_test_predicted_probabilities)
calibration_slope_female_combined <- calibration_slope(female_data, female_combined_predicted_probabilities)

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

plot_calibration(male_train_data, male_train_predicted_probabilities, "Calibration Plot for Male Train Data")
plot_calibration(male_test_data, male_test_predicted_probabilities, "Calibration Plot for Male Test Data")
plot_calibration(male_data, male_combined_predicted_probabilities, "Calibration Plot for Male Combined Data")

plot_calibration(female_train_data, female_train_predicted_probabilities, "Calibration Plot for Female Train Data")
plot_calibration(female_test_data, female_test_predicted_probabilities, "Calibration Plot for Female Test Data")
plot_calibration(female_data, female_combined_predicted_probabilities, "Calibration Plot for Female Combined Data")

# Information gain
# Define the function to extract variable names from rcs terms
extract_variable_from_rcs <- function(term) {
  # Use regular expression to extract the variable name from rcs
  variable_name <- gsub("rcs\\(([^,]+),\\s*\\d+\\)", "\\1", term)
  return(variable_name)
}

# Define the function to exclude interaction terms and simplify rcs terms
exclude_interactions_and_simplify_rcs <- function(model) {
  # Extract the formula from the model
  formula <- formula(model)
  
  # Get terms object from the formula
  terms_obj <- terms(formula)
  
  # Extract term labels
  term_labels <- attr(terms_obj, "term.labels")
  
  # Identify interaction terms (terms containing ":")
  interaction_terms <- grep(":", term_labels, value = TRUE)
  
  # Exclude interaction terms to get only main predictors
  main_predictors <- setdiff(term_labels, interaction_terms)
  
  # Replace rcs terms with just the variable names
  simplified_predictors <- sapply(main_predictors, extract_variable_from_rcs)
  
  # Remove duplicates, which might arise if multiple rcs terms refer to the same variable
  simplified_predictors <- unique(simplified_predictors)
  
  # Construct new formula with only simplified predictors
  new_formula <- as.formula(paste("highbp14090_adj", "~", paste(simplified_predictors, collapse = " + ")))
  
  # Return the new formula
  return(new_formula)
}

information_gain(exclude_interactions_and_simplify_rcs(male_reduced_model), male_data)
information_gain(exclude_interactions_and_simplify_rcs(female_reduced_model), female_data)