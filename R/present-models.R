# Set working directory at RDC
setwd("P:/10619/Dropbox/chmsflow")

# Load this R file to obtain all datasets and reduced models
source("R/develop-models.R")

# Load new packages
library(iml)
library(ggplot2)

# Ensure the outcome column is removed from the male data
male_data <- male_data[, !(names(male_data) %in% highbp14090_adj)]

# Create the Predictor object with 'response' to get probabilities
male_explainer <- Predictor$new(
  model = male_reduced_model,                                    # Pass the model
  data = male_data,                                              # Pass the dataset without the outcome
  y = data[[" highbp14090_adj"]],                                # Specify the response variable dynamically
  type = "response"                                              # Return predicted probabilities
)

# Initialize an empty list to store SHAP values for each observation
male_shap_values_list <- list()

# Loop through each row in the male data
for (i in 1:nrow(male_data)) {
  observation <- as.data.frame(mydata[i, ])            # Extract each observation as a data frame
  
  # Compute SHAP values for the observation
  male_shap_values <- Shapley$new(explainer, x.interest = observation)
  
  # Extract the computed SHAP values and store them in the list
  male_shap_values_list[[i]] <- male_shap_values$results
}

# Convert the list to a data frame for easier manipulation
male_shap_values_all <- do.call(rbind, male_shap_values_list)

# Repeat all for female model and data
female_data <- female_data[, !(names(female_data) %in% highbp14090_adj)]

female_explainer <- Predictor$new(
  model = female_reduced_model,                                   
  data = female_data,                                             
  y = data[[" highbp14090_adj"]],                                
  type = "response"                                             
)

female_shap_values_list <- list()

for (i in 1:nrow(female_data)) {
  observation <- as.data.frame(mydata[i, ])            
  
  female_shap_values <- Shapley$new(explainer, x.interest = observation)
  
  female_shap_values_list[[i]] <- female_shap_values$results
}

female_shap_values_all <- do.call(rbind, female_shap_values_list)

# # Filter for ckd == 1 and ckd == 0 separately
# shap_ckd_1 <- shap_values_all %>%
#   filter(feature.value == "ckd=1")
# 
# shap_ckd_0 <- shap_values_all %>%
#   filter(feature.value == "ckd=0")
# 
# # Calculate S-OR for ckd
# ckd_sor <- exp(mean(shap_ckd_1$phi) - mean(shap_ckd_0$phi))
# 
# # Calculate the standard error for the difference in means
# se_diff <- sqrt(mean(shap_ckd_1$phi.var) / nrow(shap_ckd_1) + mean(shap_ckd_0$phi.var) / nrow(shap_ckd_0))
# 
# # Calculate the difference in means (log-odds difference)
# log_odds_diff <- mean(shap_ckd_1$phi) - mean(shap_ckd_0$phi)
# 
# # Compute the 95% confidence interval for ckd S-OR
# lower_bound_or <- exp(log_odds_diff - 1.96 * se_diff)
# upper_bound_or <- exp(log_odds_diff + 1.96 * se_diff)