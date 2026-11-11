# Bootstrap Betas for HTNPoRT Models

This folder contains the model coefficients (betas) re-estimated with each of the 500 Canadian Health Measures Survey (CHMS) bootstrap replicate weights, for the full HTNPoRT models in males and females.

## File Descriptions

*   `HTNPoRT-full-male-500-betas.csv`: Replicate-weight coefficients for the male full model.
*   `HTNPoRT-full-female-500-betas.csv`: Replicate-weight coefficients for the female full model.

## Data Dictionary

Each row represents the model refitted with one CHMS bootstrap replicate weight.

| Variable                 | Description                                                         |
| ------------------------ | ------------------------------------------------------------------- |
| (first column, unnamed)  | Replicate identifier (e.g., "replicate_1")                          |
| (Intercept)              | The model intercept for that replicate.                             |
| ... (model coefficients) | One column per centred predictor term (e.g., `clc_age_rcs_1_C`).   |

## Code Reference

These coefficients can be used to generate uncertainty around model predictions, consistent with the CHMS bootstrap variance method (scale = 1/500, 68 degrees of freedom).

The replicate refits are run in `papers/models.Rmd` (lines 1041–1057) using `survey::withReplicates()` with `pop_est()` (`R/model-functions.R`, line 304). Coefficients are extracted with `extract_replicate_betas()` (`R/model-functions.R`, line 404) and exported in `papers/models.Rmd` (lines 1069–1077).

### Example Code

```r
res_male_full <- survey::withReplicates(weighted_male_c, function(w, data) {
  pop_est(w, data, formula = formula(male_model))
}, return.replicates = TRUE)

betas_male_full <- extract_replicate_betas(res_male_full)
```
