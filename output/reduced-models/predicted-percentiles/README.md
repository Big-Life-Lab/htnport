# HTNPoRT - Predicted Risk Percentiles

These files contain survey-weighted percentiles of predicted hypertension risk across the study population for the reduced HTNPoRT models, separated by sex, with 95% confidence intervals. They show where an individual's predicted risk falls within the population distribution for their sex.

## File Contents

-   `HTNPoRT-reduced-male-risk-percentiles.csv`
-   `HTNPoRT-reduced-female-risk-percentiles.csv`

Each file has 12 rows, for the 5th, 10th, 20th, 25th, 30th, 40th, 50th, 60th, 75th, 80th, 90th, and 95th percentiles.

The columns in these files are:

*   **`name`**: The percentile (e.g., `pct_50` is the median).
*   **`estimate`**: The survey-weighted predicted hypertension risk at that percentile.
*   **`se`**: The standard error, from the CHMS bootstrap replicate weights.
*   **`ci_lower`**, **`ci_upper`**: The 95% confidence interval, computed on the logit scale with a t distribution (68 degrees of freedom) and back-transformed.

## Usage and Generation

To place an individual, compute their predicted risk with the HTNPoRT model and compare it with these percentile values.

Weighted percentiles are computed for each replicate in `pop_est()` (`R/model-functions.R`, line 304), summarized with `build_percentile_summary()` and `extract_ci()` (`R/model-functions.R`, lines 385 and 444), and exported in `papers/models.Rmd` (lines 1927–1935).

### Example Code

```r
# Within pop_est(): weighted percentiles of predicted risk
pct_probs <- c(0.05, 0.10, 0.20, 0.25, 0.30, 0.40, 0.50, 0.60, 0.75, 0.80, 0.90, 0.95)
ord <- order(p)
cum_w <- cumsum(w[ord]) / sum(w[ord])
w_pctiles <- sapply(pct_probs, function(q) p[ord][which(cum_w >= q)[1]])

# Estimates and 95% CIs across replicates
build_percentile_summary(res_male_reduced)
```
