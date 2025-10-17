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

The code that generated these files can be found in `models.Rmd` between lines 449 and 454.

### Example Code

```r
# Obtain raw sex-specific data without transformations and centering
male_data_simple <- dplyr::filter(imputed_cycles1to6_data, clc_sex == 1)
female_data_simple <- dplyr::filter(imputed_cycles1to6_data, clc_sex == 2)

# Generate correlation matrix for predictors only
male_data_simple <- dplyr::select(male_data_simple, recodeflow:::select_vars_by_role(c("Predictor"), my_variables))
female_data_simple <- dplyr::select(female_data_simple, recodeflow:::select_vars_by_role(c("Predictor"), my_variables))

male_cor_mat <- cor(male_data_simple)
female_cor_mat <- cor(female_data_simple)
```
