# HTNPoRT Model Outputs

This folder contains all the outputs needed for the implementation of the final HTNPoRT models, including model parameters, validation data, and other model-related files.

# Structure

Each folder in this repository contains a specific type of output from the HTNPoRT model.

*   [**`bootstrap-betas`**](./bootstrap-betas/): Contains the coefficients from 1000 bootstrap iterations of the HTNPoRT models.
*   [**`correlation-matrix`**](./correlation-matrix/): Contains the correlation matrices for the predictor variables used in the HTNPoRT models.
*   [**`logistic-model-export`**](./logistic-model-export/): Contains the exported logistic regression model parameters.
*   [**`model-object`**](./model-object/): Contains the saved model objects for the HTNPoRT models.
*   [**`predicted-percentiles`**](./predicted-percentiles/): Contains the percentiles of predicted hypertension risk across the entire study population.
*   [**`R`**](./R/): Contains custom R functions used to create the derived variables in the model.
*   [**`validation-data`**](./validation-data/): Contains the validation datasets for the HTNPoRT models.
*   [**`vascular-age`**](./vascular-age/): Contains summaries of the predicted probability of hypertension for individuals at different ages.