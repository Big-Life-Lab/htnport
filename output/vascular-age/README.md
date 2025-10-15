# HTNPoRT - Vascular Age

The files in this folder provide summaries of the predicted probability of hypertension for individuals at different ages. This is referred to as "vascular age" - the baseline cardiovascular risk for a given age.

The summaries are provided for both males and females and are stratified into 1-year and 5-year age groups.

## File Contents

The data is generated from the centered derivation data (`male_data_c` and `female_data_c`), as well as the predicted values from the model.

### 1-Year Risk Summaries

These files provide risk summaries for each individual year of age from 20 to 79.

-   `HTNPoRT-male-risk-by-age-1yr.csv`
-   `HTNPoRT-female-risk-by-age-1yr.csv`

The columns in these files are:

*   **`clc_age`**: The single year of age.
*   **`mean_pred_reduced`**: The mean predicted hypertension risk for that age.
*   **`sd_pred_reduced`**: The standard deviation of the predicted risk for that age.
*   **`median_pred_reduced`**: The median (50th percentile) predicted hypertension risk for that age.
*   **`iqr_pred_reduced`**: The Interquartile Range (IQR) of the predicted risk for that age.

### 5-Year Risk Summaries

These files provide more detailed risk summaries for 5-year age groups.

-   `HTNPoRT-male-risk-by-age-5yr.csv`
-   `HTNPoRT-female-risk-by-age-5yr.csv`

The columns in these files are:

*   **`age_group_5yr`**: The 5-year age group (e.g., `[20,25)`).
*   **`mean_pred_reduced`**: The mean predicted hypertension risk for that age group.
*   **`sd_pred_reduced`**: The standard deviation of the predicted risk.
*   **`min_pred_reduced`**: The minimum predicted risk in that age group.
*   **`max_pred_reduced`**: The maximum predicted risk in that age group.
*   **`p05_reduced` ... `p95_reduced`**: The 5th, 10th, 20th, 25th, 30th, 40th, 50th, 60th, 75th, 80th, 90th, and 95th percentiles of predicted risk for that age group.

## Usage and Generation

These files can be used to look up the baseline hypertension risk for a "healthy" individual, which can be conceptualized as their "vascular age". For example, a user can find their age in the `1yr` file to see the median and spread of risk for a healthy person of their age.

The code that generates these summary files can be found in `C:\Users\User\Documents\R\htnport\papers\models.Rmd` between lines 1860-1979. The core logic for creating the summaries is as follows:

**1-Year Summary Generation:**
```r
male_summary_1yr <- male_data_c %>%
  group_by(clc_age) %>%
  summarise(
    mean_pred_reduced = mean(reduced_predicted),
    sd_pred_reduced = sd(reduced_predicted),
    median_pred_reduced = median(reduced_predicted),
    iqr_pred_reduced = IQR(reduced_predicted)
  )
```

**5-Year Summary Generation:**
```r
male_data_c$age_group_5yr <- cut(male_data_c$clc_age, breaks = seq(20, 80, by = 5), right = FALSE)

percentiles <- c(0.05, 0.10, 0.20, 0.25, 0.30, 0.40, 0.50, 0.60, 0.75, 0.80, 0.90, 0.95)

male_summary_5yr <- male_data_c %>%
  group_by(age_group_5yr) %>%
  summarise(
    mean_pred_reduced = mean(reduced_predicted),
    sd_pred_reduced = sd(reduced_predicted),
    min_pred_reduced = min(reduced_predicted),
    max_pred_reduced = max(reduced_predicted),
    p05_reduced = quantile(reduced_predicted, 0.05),
    # ... and so on for other percentiles
  )
```
