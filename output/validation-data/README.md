# HTNPoRT Model Validation Data

This folder contains the validation datasets for the male and female HTNPoRT models. These datasets include the original predictor variables, transformed variables (e.g., restricted cubic splines), and the model's predicted probabilities of hypertension.

## File Descriptions

*   `HTNPoRT-male-validation-data.csv`: Validation data and predictions for the male model.
*   `HTNPoRT-female-validation-data.csv`: Validation data and predictions for the female model.

## Data Dictionary

| Variable            | Description                                                                 |
| ------------------- | --------------------------------------------------------------------------- |
| id                  | Unique identifier for each individual in the validation set.                |
| clc_age             | Age of the respondent.                                                      |
| ...                 | Columns for original and transformed predictor variables.                   |
| reduced_predicted   | The predicted probability of hypertension from the corresponding model.      |

## Usage

 These files can be used for testing whether model objects generate same predictions outside secure environments. The code that generated these files can be found in `models.Rmd` between lines 1765 and 1860.