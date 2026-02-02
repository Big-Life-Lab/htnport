# Bootstrap Betas for HTNPoRT Models

This folder contains CSV files with the coefficients (betas) from 1000 bootstrap iterations of the HTNPoRT models for males and females.

## File Descriptions

*   `HTNPoRT-reduced-male-1000-betas.csv`: Contains the bootstrap coefficients for the male reduced model.
*   `HTNPoRT-reduced-female-1000-betas.csv`: Contains the bootstrap coefficients for the female reduced model.

## Data Dictionary

Each row in the CSV files represents a single bootstrap iteration.

| Variable                          | Description                                                 |
| --------------------------------- | ----------------------------------------------------------- |
| Model iteration                   | Identifier for the bootstrap sample (e.g., "bootstrap_1")   |
| Intercept                         | The intercept term for the model in that iteration.         |
| ... (model coefficients)          | Columns for each of the model's predictor variables' coefficients. |

## Code Reference

These files can be used for generating uncertainty around model predictions. The code that generated these files can be found in `models.Rmd` between lines 1038 and 1056 and in `model-functions.R` between lines 250 and 262.

### Example Code

```r
# Function to obtain bootstrap beta coefficients for a model
bootstrap_coefs <- function(data, indices, model) {
  # Create bootstrap sample
  boot_data <- data[indices, ]
  
  # Recreate survey design
  boot_design <- survey::svydesign(ids = ~1, weights = ~wgt_full, data = boot_data)
  
  # Refit model
  boot_model <- update(model, data = boot_data, design = boot_design)
  
  # Return coefficients
  return(coef(boot_model))
}

# Get bootstrap beta coefficients and their standard errors
bootstrap_coefs_male <- replicate(n_bootstrap, {
  boot_indices <- sample(1:nrow(male_data_c), replace = TRUE)
  bootstrap_coefs(male_data_c, boot_indices, male_final_model)
}, simplify = FALSE)

coef_matrix_male <- do.call(rbind, bootstrap_coefs_male)
rownames(coef_matrix_male) <- paste0("bootstrap_", 1:n_bootstrap)

bootstrap_coefs_female <- replicate(n_bootstrap, {
  boot_indices <- sample(1:nrow(female_data_c), replace = TRUE)
  bootstrap_coefs(female_data_c, boot_indices, female_final_model)
}, simplify = FALSE)

coef_matrix_female <- do.call(rbind, bootstrap_coefs_female)
rownames(coef_matrix_female) <- paste0("bootstrap_", 1:n_bootstrap)
```
