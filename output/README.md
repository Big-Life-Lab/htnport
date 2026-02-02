# HTNPoRT Model Outputs

This folder contains all the outputs needed for the implementation of the HTNPoRT models, organized by model type.

## Structure

*   [**`full-models`**](./full-models/): Contains outputs for the **full HTNPoRT models** with all predictors (age, marital status, education, working status, mental health, stress, community belonging, hypertension family history, BMI, alcohol consumption, smoking, physical activity, fruit and vegetable consumption, sleep duration, diabetes, and CKD).

*   [**`reduced-models`**](./reduced-models/): Contains outputs for the **final reduced HTNPoRT models** with core predictors only (age, hypertension family history, BMI, and diabetes).

## Subfolders Within Each Model Type

Each model folder (`full-models/` and `reduced-models/`) contains:

*   **`bootstrap-betas/`**: Coefficients from 1000 bootstrap iterations of the HTNPoRT models.
*   **`correlation-matrix/`**: Correlation matrices for the predictor variables.
*   **`logistic-model-export/`**: Exported logistic regression model parameters (CSV files for deployment).
*   **`model-object/`**: Saved R model objects (`.rds` files).
*   **`predicted-percentiles/`**: Percentiles of predicted hypertension risk across the study population.
*   **`validation-data/`**: Validation datasets for testing model predictions.
*   **`vascular-age/`**: Summaries of predicted hypertension probability by age group.

## File Naming Convention

Files follow the pattern: `HTNPoRT-{model}-{sex}-{description}.csv`

*   **Full models**: `HTNPoRT-full-male-*.csv`, `HTNPoRT-full-female-*.csv`
*   **Reduced models**: `HTNPoRT-reduced-male-*.csv`, `HTNPoRT-reduced-female-*.csv`
