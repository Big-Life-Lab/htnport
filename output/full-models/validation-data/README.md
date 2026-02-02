# HTNPoRT Model Validation Data

This folder contains the validation datasets for the male and female HTNPoRT models. These datasets include the original predictor variables, transformed variables (e.g., restricted cubic splines), and the model's predicted probabilities of hypertension.

## File Descriptions

*   `HTNPoRT-full-male-validation-data.csv`: Validation data and predictions for the male full model.
*   `HTNPoRT-full-female-validation-data.csv`: Validation data and predictions for the female full model.

## Data Dictionary

| Variable            | Description                                                                 |
| ------------------- | --------------------------------------------------------------------------- |
| id                  | Unique identifier for each individual in the validation set.                |
| clc_age             | Age of the respondent.                                                      |
| ...                 | Columns for original and transformed predictor variables.                   |
| predicted_risk      | The predicted probability of hypertension from the model.      |

## Usage

 These files can be used for testing whether model objects generate same predictions outside secure environments. The code that generated these files can be found in `models.Rmd` between lines 1787 and 1882.

### Example Code

```r
set.seed(123)
male_validation_data <- data.frame(
  clc_age = sample(20:79, 10000, replace = TRUE),
  fmh_15 = factor(sample(0:1, 10000, replace = TRUE)),
  hwmdbmi = runif(10000, 13.8, 49),
  diabx = factor(sample(0:1, 10000, replace = TRUE)),
)

# -------- dummy variables --------
cat_variables <- c("diabx", "fmh_15")
male_validation_data <- step_dummy(male_validation_data, cat_variables)

# -------- rcs --------
# age
# age
knots_age <- c(24, 40, 57, 74) # male
# BMI
knots_bmi <- c(22.35, 27.24, 34.178) # male
# unpack rcs
vars_rcs <- c("clc_age", "hwmdbmi", "minperweek")
knots_list <- list(knots_age, knots_bmi, knots_exercise)
knots_list <- setNames(knots_list, vars_rcs)
male_validation_data <- step_rcs(male_validation_data, vars_rcs, knots_list)

# -------- interaction terms --------
interaction_list <- list(
  c("clc_age", "hwmdbmi"),
  c("clc_age", "diabx")
)
male_validation_data <- step_interaction(male_validation_data, interaction_list)

# Get weighted mean values from the dataset
vars_mean <- c("clc_age_rcs_1", "clc_age_rcs_2", "clc_age_rcs_3", "fmh_15_1", "hwmdbmi_rcs_1", "hwmdbmi_rcs_2", "diabx_1", "clc_age_rcs_1_by_hwmdbmi_rcs_1", "clc_age_rcs_2_by_hwmdbmi_rcs_1", "clc_age_rcs_3_by_hwmdbmi_rcs_1", "clc_age_rcs_1_by_hwmdbmi_rcs_2", "clc_age_rcs_2_by_hwmdbmi_rcs_2", "clc_age_rcs_3_by_hwmdbmi_rcs_2", "clc_age_rcs_1_by_diabx_1", "clc_age_rcs_2_by_diabx_1", "clc_age_rcs_3_by_diabx_1")

means <- c(
  46.14509835,
  10.41999141,
  2.012829724,
  0.524803077,
  27.66777298,
  2.529042313,
  0.104918358,
  1289.586153,
  294.9641651,
  56.95945766,
  121.6744825,
  28.59310879,
  5.458573727,
  6.145555691,
  2.19318601,
  0.472921201
)

# centering vars in the dataset
names(means) <- vars_mean
vars_center <- names(means)
male_validation_data <- step_center(male_validation_data, vars_center, means)

# Add centered model predictions to dataset
male_validation_data$predicted_risk <- predict(male_full_model, newdata = male_validation_data, type = "response")
```
