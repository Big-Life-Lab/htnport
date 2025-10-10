# Bootstrap Betas for HTNPoRT Models

This folder contains CSV files with the coefficients (betas) from 1000 bootstrap iterations of the HTNPoRT models for males and females.

## File Descriptions

*   `HTNPoRT-male-1000-betas.csv`: Contains the bootstrap coefficients for the male model.
*   `HTNPoRT-female-1000-betas.csv`: Contains the bootstrap coefficients for the female model.

## Data Dictionary

Each row in the CSV files represents a single bootstrap iteration.

| Variable                          | Description                                                 |
| --------------------------------- | ----------------------------------------------------------- |
| Model iteration                   | Identifier for the bootstrap sample (e.g., "bootstrap_1")   |
| Intercept                         | The intercept term for the model in that iteration.         |
| ... (model coefficients)          | Columns for each of the model's predictor variables' coefficients. |

## Code Reference

 These files can be used for generating uncertainty around model predictions. The code that generated these files can be found in `models.Rmd` between lines 991 and 1034.