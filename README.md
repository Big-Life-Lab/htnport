# HTNPoRT

## Hypertension Population Risk Tool (HTNPoRT)

## Abstract

**Objectives**: Effective, equitable hypertension prevention requires an understanding of which populations are at risk. We aimed to develop and validate the Hypertension Population Risk Tool (HTNPoRT) - a diagnostic model derived with only readily available data, suitable for individual screening and population health planning.

**Methods**: We analyzed data from the Canadian Health Measures Survey (cycles 1–6, 2007–2019). The study included community-dwelling respondents aged 20–79 years. The primary outcome was hypertension, defined as measured systolic/diastolic blood pressure of 140/90 mm Hg or current antihypertensive medication use. Sex-specific logistic regression models were developed using 16 predictors, including 4 sociodemographic, 3 psychosocial, 2 health status, 5 health behavioural, and 2 chronic condition variables. The model was fully prespecified, including the stepdown procedure to derive parsimonious models.

**Results**: Of 19,643 participants, 5,152 (26.2%) had hypertension. The final models included age, body mass index, diabetes, and family history of hypertension. Optimism-corrected c-statistics were 0.86 (95% CI: 0.85–0.87) for men and 0.88 (95% CI: 0.87–0.88) for women. Calibration showed relative differences between observed and predicted risk of 1.02% (men) and 1.41% (women), and consistent performance across 179 of 181 policy-relevant subgroups. Predicted hypertension risk in Canada varied but rose markedly with older age, diabetes, and obesity.

**Conclusions**: HTNPoRT is a well-performing predictive algorithm that relies only on minimal non-invasive, self-reported data. It is suitable for both individual risk screening and population-level surveillance to inform hypertension prevention strategies targeting both the general population and high-risk groups.

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
