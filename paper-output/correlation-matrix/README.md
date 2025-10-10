# Correlation Matrices for HTNPoRT Model Predictors

This folder contains CSV files with the correlation matrices for the predictor variables used in the HTNPoRT models, separated by sex. These matrices are used to assess multicollinearity among predictors.

## File Descriptions

*   `male-correlation-matrix.csv`: Correlation matrix for the male model predictors.
*   `female-correlation-matrix.csv`: Correlation matrix for the female model predictors.

## Data Dictionary

The CSV files contain a matrix where both rows and columns represent the following predictor variables. The values are the Pearson correlation coefficients.

| Variable  | Description                               |
| --------- | ----------------------------------------- |
| clc_age   | Age of the respondent                     |
| diabx     | Presence of diabetes                      |
| fmh_15    | Family history of hypertension            |
| hwmdbmi   | Body Mass Index (BMI)                     |

## Code Reference

The code that generated these files can be found in `models.Rmd` between lines 449 and 457.