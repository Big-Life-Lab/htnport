# HTNPoRT - Predicted Risk Percentiles

These files contain the percentiles of predicted hypertension risk across the entire study population, separated by sex. This allows for understanding where an individual's predicted risk falls within the overall distribution for their sex.

## File Contents

The files map each percentile (from 1 to 100) to a specific predicted hypertension risk value. This provides a lookup table to see what risk value corresponds to a given percentile, and vice-versa.

-   `HTNPoRT-full-male-risk-percentiles.csv`
-   `HTNPoRT-full-female-risk-percentiles.csv`

The columns in these files are:

*   **`percentile`**: The percentile rank, from 1 to 100.
*   **`predicted_risk`**: The predicted hypertension risk value that corresponds to that percentile for the specified sex. For example, the value in the female `predicted_risk` column for the `percentile` of 50 is the median predicted risk for the entire female study population.

## Usage and Generation

These files can be used to determine the percentile rank of an individual's predicted risk. By calculating a person's predicted risk using the HTNPoRT model, you can use these tables to find where that risk falls within the context of the broader population.

The code that generates these percentile lookups can be found in `models.Rmd` between lines 529-536. The logic is as follows:

### Example Code

```r
percentiles <- 1:100

predicted_percentiles <- data.frame(
  percentile = percentiles,
  `predicted risk (male)` = quantile(male_predicted_probabilities, probs = percentiles / 100, na.rm = TRUE),
  `predicted risk (female)` = quantile(female_predicted_probabilities, probs = percentiles / 100, na.rm = TRUE)
)
```
