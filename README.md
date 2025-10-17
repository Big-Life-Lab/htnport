# HTNPoRT

## Hypertension Population Risk Tool (HTNPoRT)

## Abstract

**Background**: Current models predicting hypertension have limited utility for patient-oriented decision making and population health planning.

**Methods**: The Hypertension Population Risk Tool (HTNPoRT) was derived from respondents aged 20-79 in the Canadian Health Measures Survey (survey years 2007 to 2019). Sex-specific logistic regression models were developed using 16 predictors, including 4 sociodemographics, 3 psychosocial measures, 2 health status indicators, 5 health behaviours, and 2 chronic conditions. The primary outcome was hypertension presence ascertained from blood pressure measurements and antihypertensive medication use.

**Results**: 5,152 of the 19,643 respondents in the study had hypertension (26.2%). The final HTNPoRT models each had 4 predictors and 2 age interactions,  were discriminating  (c-statistic, men: 0.86; women: 0.88), and were well-calibrated in the overall population (ratio of observed v. predicted events, men: 1.02%; women: 1.41%) and nearly all equity-relevant subgroups (179 out of 181). SHAP-derived risk profiles show the contribution of predictors towards the predicted hypertension outcome, while predictability of adiposity measures differed across sex.

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

- **data**: Study data (only available at RDC).
- **R**: R functions necessary for running HTNPoRT descriptives, derivation, validation, and presentation.
- **output**: Select parameters, objects, and paper output needed for final HTNPoRT model implementation.
- **papers**: Papers written for this project which include reproducible results.
- **worksheets**: `variables.csv` and `variable-details.csv` files detailing which variables are transformed across CHMS for HTNPoRT analyses and how they are recoded, respectively.
