# HTNPoRT Model Objects

This folder contains the survey-weighted generalized linear (`svyglm`) model for predicting hypertension, saved as an R object (`.rds` file). The model been processed to remove all individual-level data, ensuring privacy while retaining the necessary components for prediction and recalibration.

## Model Object Components:

The `male_model.rds` and `female_model.rds` objects are `safe_svyglm` lists with the following components:

*   **`coefficients`**: A numeric vector of the model's parameter estimates. These are the weights for each predictor in the model.

*   **`vcov`**: The variance-covariance matrix for the model coefficients. This is essential for calculating the standard errors of predictions.

*   **`formula`**: The model formula, which specifies the relationship between the outcome and the predictors. The formula for this model is:
    ```
    highbp14090_adj ~ clc_age_rcs_1_C + clc_age_rcs_2_C + clc_age_rcs_3_C + fmh_15_1_C + hwmdbmi_rcs_1_C + hwmdbmi_rcs_2_C + diabx_1_C + clc_age_rcs_1_by_hwmdbmi_rcs_1_C + clc_age_rcs_2_by_hwmdbmi_rcs_1_C + clc_age_rcs_3_by_hwmdbmi_rcs_1_C +  clc_age_rcs_1_by_hwmdbmi_rcs_2_C + clc_age_rcs_2_by_hwmdbmi_rcs_2_C + clc_age_rcs_3_by_hwmdbmi_rcs_2_C + clc_age_rcs_1_by_diabx_1_C + clc_age_rcs_2_by_diabx_1_C + clc_age_rcs_3_by_diabx_1_C
    ```
    
*   **`family`**: The error distribution and link function used in the model. For this model, it is `quasibinomial`, which is suitable for binary outcomes (like the presence or absence of hypertension) that may have overdispersion.

*   **`df.residual`**: The residual degrees of freedom for the model.

*   **`dispersion`**: The dispersion parameter of the model, estimated from the data.

*   **`survey_info`**: A list summarizing the survey design. It includes information like the number of observations, strata, and clusters, but no individual-level data.

*   **`centering`**: A list containing the mean values used to center the main predictors in the model. This is crucial for applying the model to new data.

*   **`spline_info`**: A list containing the knot locations for the restricted cubic splines (RCS) used to model non-linear relationships for some continuous predictors.

*   **`factor_info`**: A list containing information about factor levels and contrasts for categorical predictors.

*   **`original_prevalence`**: The prevalence of the outcome (hypertension) in the original dataset. This is useful for model recalibration in new datasets.

## Usage

The `male_model.rds` and `female_model.rds` objects can be loaded into R using `readRDS()` and used with a custom `predict.safe_svyglm` function to make predictions on new data. See the `model-export.qmd` file for an example of how to create and use the objects.
