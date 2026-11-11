# HTNPoRT - Vascular Age

The files in this folder provide the survey-weighted mean predicted probability of hypertension by age for the reduced HTNPoRT models, with 95% confidence intervals. This is referred to as "vascular age": the average hypertension risk for people of a given age.

The summaries are provided for males and females, by 1-year age and by 5-year age group.

## File Contents

### 1-Year Risk Summaries

Mean predicted risk for each single year of age from 20 to 79.

-   `HTNPoRT-reduced-male-risk-by-age-1yr.csv`
-   `HTNPoRT-reduced-female-risk-by-age-1yr.csv`

### 5-Year Risk Summaries

Mean predicted risk for each 5-year age group from 20 to 79.

-   `HTNPoRT-reduced-male-risk-by-age-5yr.csv`
-   `HTNPoRT-reduced-female-risk-by-age-5yr.csv`

The columns in these files are:

*   **`clc_age`** (1-year files) or **`age_group_5yr`** (5-year files): The single year of age, or the 5-year age group (e.g., `[20,25)`).
*   **`estimate`**: The survey-weighted mean predicted hypertension risk.
*   **`ci_lower`**, **`ci_upper`**: The 95% confidence interval, from the CHMS bootstrap replicate weights, computed on the logit scale with a t distribution (68 degrees of freedom) and back-transformed.

## Usage and Generation

A user can find their age (or age group) to see the average predicted hypertension risk for people of that age, and compare it with their own predicted risk.

Weighted means are computed for each replicate in `pop_est()` (`R/model-functions.R`, line 304) and summarized with `build_age1yr_summary()` and `build_age5yr_summary()` (`R/model-functions.R`, lines 415 and 429). These functions return full and reduced model results side by side; the exported files from `papers/models.Rmd` (lines 1910–1925) were split into separate full and reduced files for this folder.

### Example Code

```r
# Within pop_est(): weighted mean predicted risk by 1-year age
mean_by_age1yr <- sapply(ages_1yr, function(a) {
  idx <- data[[age_var]] == a
  sum(w[idx] * p[idx]) / sum(w[idx])
})

# Estimates and 95% CIs across replicates (full and reduced side by side)
male_summary_1yr <- build_age1yr_summary(res_male_full, res_male_reduced)
male_summary_5yr <- build_age5yr_summary(res_male_full, res_male_reduced)
```
