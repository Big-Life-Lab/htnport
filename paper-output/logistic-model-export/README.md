# HTNPoRT - Model Export Files

This folder contains the model export files for the **Hypertension Population Risk Tool (HTNPoRT)**, a diagnostic model designed to predict hypertension probability in the community setting. HTNPoRT was developed using population-based health survey data from Canada (Canadian Health Measures Survey - CHMS).

## Purpose

HTNPoRT is intended to support decision-making regarding hypertension prevention at both individual and population levels. The model predicts hypertension probability for community-dwelling adults based on age, family history, body mass index, and diabetes, while also providing SHAP-derived risk profiles which showcase the most influential predictors towards the probability value.

This folder provides the files required to implement and run the HTNPoRT algorithm, including both raw coefficient files and a PMML file for deployment.

## Folder Structure

Click [here](https://big-life-lab.github.io/model-parameters/2-model-parameter-files.html) for details on csv file aspects.

### Core Files

-   **model-export.csv:** An index file listing all other files required by the algorithm for deployment and execution.
-   **model-steps.csv:** Specifies the sequence of calculations to estimate the hypertension risk, from data preprocessing to final risk calculation.

### Variable Configuration Files

-   **variables.csv:** Descriptions of all variables used in the final model, including their data types, transformations, and roles.
-   **variable-details.csv:** Metadata and descriptive details for each variable, such as units and potential ranges.

### Transformation and Encoding Files

-   **center.csv:** Lists variables that were centered during model development.
-   **dummy.csv:** Defines categorical variables that were transformed into dummy variables.
-   **rcs.csv:** Specifies variables transformed using restricted cubic splines (RCS), including knot values.

### Model Coefficients and Predictions

-   **logistic.csv:** Sex-specific coefficients from the logistic regression model.
-   **interactions.csv:** Details the interaction terms included in the model.

### Deployment File

-   **HTNPoRT.pmml:** The Predictive Model Markup Language (PMML) file for direct integration with compatible software.

## Model Card: HTNPoRT

### Model Details

-   **Developed by:** Rafidul Islam, Douglas G. Manuel, and collaborators.\
-   **Model Type:** Logistic regression model.\
-   **Version:** HTNPoRT
-   **Input:** Age, family history, body mass index, and diabetes.\
-   **Output:** Hypertension probability/risk (diagnosed or undiagnosed).\
-   **Model Format:** PMML (with CSV support).\
-   **License:** Apache License 2.0.
-   **Excel Calculator**: <https://osf.io/mjd7n/>

### Intended Use

-   **Primary Purpose:** Estimating baseline hypertension risk for public health planning and clinical decision support.\
-   **Intended Users:** Public health professionals, epidemiologists, clinicians, and community-dwelling adults.\
-   **Geographic Scope:** Canada.

### Limitations

-   **Data Source:** Derived from community health surveys; may underrepresent certain populations.\
-   **Self-reported Data:** Subject to recall bias.\
-   **Interpretation:** Predictive associations, not causal.

### Ethical Considerations

-   **Equity:** Comprehensive sociodemographic data included, but underrepresentation of certain populations remains possible.\
-   **Transparency:** Openly shared model and coefficients for independent validation.

## License

This project is licensed under the Apache License 2.0. You are free to use, modify, and distribute the code under the terms of this license. See the LICENSE file for more details.
