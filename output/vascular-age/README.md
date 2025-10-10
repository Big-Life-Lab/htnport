# HTNPoRT - Vascular Age

## Purpose

The vascular age refers to the predicted probability/risk of hypertension for each age while other variables are held to their references/means. In other words, the vascular age is the predicted hypertension probability/risk for a healthy person of a given age.

## How to calculate

1.  Create a data frame with a row for each and every integer age from 20 to 79 years.

2.  Set categorical variables to their respective reference category for every age row.

-   **Family history:** 0 (family history absent)
-   **Diabetes:** 0 (diabetes absent)

3.  Set body mass index to its respective weighted mean for every age row.

-   **Male** 27.66777 kg/m2
-   **Female**: 27.09638 kg/m2

4.  Unpack all restricted cubic spline and interaction terms, and center all raw and resulting variables on their weighted means.

5.  Predict hypertension probability/risk for each age row.

6.  Repeat process for other sex.

## Files

-   **HTNPoRT-male-risk-by-age.csv:** Predicted hypertension probability/risk for healthy males of each age 20-79
-   **HTNPoRT-female-risk-by-age.csv:** Predicted hypertension probability/risk for healthy females of each age 20-79
