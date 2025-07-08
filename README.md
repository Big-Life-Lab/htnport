# HTNPoRT

## Hypertension Population Risk Tool (HTNPoRT)

## Abstract

**Background**: Hypertension is preventable, and models predicting hypertension lack use for individual and population health planning.

**Methods**: The Hypertension Population Risk Tool (HTNPoRT) was derived from 19,643 respondents aged 20-79 in the Canadian Health Measures Survey. Sex-specific logistic regression models were developed using 16 predictors, including 4 sociodemographics, 3 psychosocial measures, 2 health status indicators, 5 health behaviours, and 2 chronic conditions.

**Results**: The final HTNPoRT models were discriminating (men c-statistic: 0.86, women c-statistic: 0.88), and well-calibrated in the overall population (men observed v. predicted: 1.02%, women observed v. predicted: 1.41%) and nearly all equity-relevant subgroups (179 out of 181). Age, diabetes, and body mass index were the most influential predictors of hypertension on SHAP-derived risk profiles, while predictability of adiposity measures differed across sex.

**Conclusions**: The public and health policymakers can use the models and risk profiles of HTNPoRT to support planning and decision-making on addressing the hypertension burden.

## Setup

This project can only be run at the uOttawa Research Data Centre (RDC) managed by Statistics Canada.

1.  Ensure all dependencies are located within a folder in your P drive at the RDC.
2.  Place the directory to the above folder in .libPaths() and use library() to load dependencies afterwards.
3.  Load functions and worksheets using source() and read.csv(), respectively.
4.  Create data folder in htnport and load CHMS data from there.
    1.  Ensure all required components of each CHMS cycle (minus medications) are in one Stata file called cyclex.dta. Combined bootstrap weights for all six cycles are located in cycles1to6_bsw.dta.
    2.  Keep medications for each CHMS cycle a separate Stata file called cyclex-meds.dta, though those of cycles 1-2 will be SAS files (cyclex-meds.sas7bdat).
    3.  Put names() of cycle 6 and medications of cycles 1, 4, and 6 as lower case to allow proper recoding with rec_with_table().
    4.  Load data using read_stata() and read_sas().
5. Follow workflow of one of the files in the papers folder to run specific code and/or reproduce results.

## Folders

- **data**: Study data (only available at RDC) and test data for validating svyglm model export.
- **R**: R functions necessary for running HTNPoRT descriptives, derivation, validation, and presentation.
- **models**: Exported svyglm and glm model objects of HTNPoRT.
- **papers**: Papers written for this project which include reproducible results.
- **worksheets**: variables.csv and variable-details.csv files detailing which variables are transformed across CHMS for HTNPoRT analyses and how they are recoded, respectively.
