# Set working directory at RDC
setwd("P:/10619/Dropbox/chmsflow")

# Load this R file to obtain all datasets and reduced models
source("R/develop-models.R")

# Load new packages
library(iml)
library(ggplot2)

# Define function to calculate and plot SHAP values for a given individual
generate_shap_values <- function(model, data, observation) {
  
  # Create the Predictor object with 'response' to get probabilities
  explainer <- Predictor$new(
    model = model,          # Pass the model
    data = data,            # Pass the dataset
    y = data$highbp14090_adj, # Specify the response variable
    type = "response"       # Return predicted probabilities
  )
  
  # Compute SHAP values for passed in observation
  shap_values <- Shapley$new(explainer, x.interest = observation)
  
  # Plot the SHAP values for the instance
  plot <- shap_values$plot()
  
  return(plot)
}

# Example: plot SHAP values for first observation in datasets
generate_shap_values(male_reduced_model, male_data, male_data[1, ])
generate_shap_values(female_reduced_model, female_data, female_data[1, ])