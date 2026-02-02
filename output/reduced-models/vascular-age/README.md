# HTNPoRT - Vascular Age

The files in this folder provide summaries of the predicted probability of hypertension for individuals at different ages. This is referred to as "vascular age" - the baseline cardiovascular risk for a given age.

The summaries are provided for both males and females and are stratified into 1-year and 5-year age groups.

## File Contents

The data is generated from the centered derivation data (`male_data_c` and `female_data_c`), as well as the predicted values from the model.

### 1-Year Risk Summaries

These files provide risk summaries for each individual year of age from 20 to 79.

-   `HTNPoRT-reduced-male-risk-by-age-1yr.csv`
-   `HTNPoRT-reduced-female-risk-by-age-1yr.csv`

The columns in these files are:

*   **`clc_age`**: The single year of age.
*   **`mean_pred`**: The mean predicted hypertension risk for that age.
*   **`sd_pred`**: The standard deviation of the predicted risk for that age.
*   **`median_pred`**: The median (50th percentile) predicted hypertension risk for that age.
*   **`iqr_pred`**: The Interquartile Range (IQR) of the predicted risk for that age.

### 5-Year Risk Summaries

These files provide more detailed risk summaries for 5-year age groups.

-   `HTNPoRT-reduced-male-risk-by-age-5yr.csv`
-   `HTNPoRT-reduced-female-risk-by-age-5yr.csv`

The columns in these files are:

*   **`age_group_5yr`**: The 5-year age group (e.g., `[20,25)`).
*   **`mean_pred`**: The mean predicted hypertension risk for that age group.
*   **`sd_pred`**: The standard deviation of the predicted risk.
*   **`min_pred`**: The minimum predicted risk in that age group.
*   **`max_pred`**: The maximum predicted risk in that age group.
*   **`p05_pred` ... `p95_pred`**: The 5th, 10th, 20th, 25th, 30th, 40th, 50th, 60th, 75th, 80th, 90th, and 95th percentiles of predicted risk for that age group.

## Usage and Generation

These files can be used to look up the baseline hypertension risk for a "healthy" individual, which can be conceptualized as their "vascular age". For example, a user can find their age in the `1yr` file to see the median and spread of risk for a healthy person of their age.

The code that generates these summary files can be found in `models.Rmd` between lines 1888-2005. The core logic for creating the summaries is as follows:

**1-Year Summary Generation:**
```r
male_summary_1yr <- male_data_c %>%
  group_by(clc_age) %>%
  summarise(
    mean_pred = mean(predicted_risk),
    sd_pred = sd(predicted_risk),
    median_pred = median(predicted_risk),
    iqr_pred = IQR(predicted_risk)
  )
```

**5-Year Summary Generation:**
```r
male_data_c$age_group_5yr <- cut(male_data_c$clc_age, breaks = seq(20, 80, by = 5), right = FALSE)

percentiles <- c(0.05, 0.10, 0.20, 0.25, 0.30, 0.40, 0.50, 0.60, 0.75, 0.80, 0.90, 0.95)

male_summary_5yr <- male_data_c %>%
  group_by(age_group_5yr) %>%
  summarise(
    mean_pred = mean(predicted_risk),
    sd_pred = sd(predicted_risk),
    min_pred = min(predicted_risk),
    max_pred = max(predicted_risk),
    p05_pred = quantile(predicted_risk, 0.05),
    # ... and so on for other percentiles
  )
```
