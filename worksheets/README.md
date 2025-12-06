# Roles

## recode

Variables needed for any analyses (including for deriving variables) and recoded using recodeflow:::select_vars_by_role(). All variables in variables.csv have this role, except:

| Variable Name | Variable Description                    |
|---------------|-----------------------------------------|
| mucatc        | ATC code for medication (cycles 3-6)    |
| npi_25b       | Time medication last taken (cycles 3-6) |

## fam

Variables present in only the first four cycles of the CHMS.

| Variable Name | Variable Description        |
|---------------|-----------------------------|
| fmh_15        | Hypertension family history |
| slp_11        | Hours of sleep per night    |

## drugs

Variables present in only medication files of CHMS cycles 1-2 + derived medication variables

| Variable Name | Variable Description                                       |
|------------------------------------|------------------------------------|
| anymed        | Anti-hypertensive medication                               |
| atc101-15a    | ATC codes for fifteen prescription medications             |
| atc131-35a    | ATC codes for five new prescription medications            |
| atc201-15a    | ATC codes for fifteen over-the-counter medications         |
| atc231-35a    | ATC codes for five new over-the-counter medications        |
| diab_drug     | Diabetes medication                                        |
| mhr101-15b    | Time when fifteen prescription medications last taken      |
| mhr131-35b    | Time when five new prescription medications last taken     |
| mhr201-15b    | Time when fifteen over-the-counter medications last taken  |
| mhr231-35b    | Time when five new over-the-counter medications last taken |

## predictor

Variables included as candidate predictors for HTNPoRT models.

| Variable Name    | Variable Description                  |
|------------------|---------------------------------------|
| ckd              | Chronic kidney disease                |
| clc_age          | Age                                   |
| diabx            | Diabetes                              |
| edudr04          | Highest education level               |
| fmh_15           | Hypertension family history           |
| gendmhi          | Self-rated mental health              |
| gen_025          | Self-perceived stress                 |
| gen_045          | Sense of belonging to local community |
| hwmdbmi          | Body mass index                       |
| low_drink_score1 | Alcohol consumption level             |
| married          | Marital status                        |
| minperweek       | Minutes of exercise per week          |
| slp_11           | Hours of sleep per night              |
| smoke            | Smoking status                        |
| totalfv          | Daily fruit and vegetable consumption |
| whr              | Waist-to-height ratio                 |
| working          | Working status                        |

## imputation-predictor

Variables included as predictors for the imputation model.

| Variable Name    | Variable Description                  |
|------------------|---------------------------------------|
| ckd              | Chronic kidney disease                |
| clc_age          | Age                                   |
| clc_sex          | Sex                                   |
| cycle            | CHMS cycle                            |
| diabx            | Diabetes                              |
| edudr04          | Highest education level               |
| fmh_15           | Hypertension family history           |
| gendmhi          | Self-rated mental health              |
| gen_025          | Self-perceived stress                 |
| gen_045          | Sense of belonging to local community |
| highbp14090_adj  | Hypertension (adjusted) 140/90        |
| hwmdbmi          | Body mass index                       |
| low_drink_score1 | Alcohol consumption level             |
| married          | Marital status                        |
| minperweek       | Minutes of exercise per week          |
| slp_11           | Hours of sleep per night              |
| smoke            | Smoking status                        |
| totalfv          | Daily fruit and vegetable consumption |
| whr              | Waist-to-height ratio                 |
| working          | Working status                        |

## cali-subgroup

Variables, other than those assigned "Predictor" (except clc_age), included as subgroups for model calibration.

| Variable Name | Variable Description       |
|---------------|----------------------------|
| adj_hh_inc    | Adjusted household income  |
| agegroup4     | Categorical age            |
| alcdwky       | Weekly alcohol consumption |
| gendhdi       | Self-rated health          |
| gfr           | Glomerular filtration rate |
| gooddiet      | Healthy diet indicator     |
| hwm_13kg      | Weight                     |
| hwm_14cx      | Waist circumference        |
| img_03        | Immigrant                  |
| lab_bpb       | Blood lead                 |
| mvpa150wk     | Healthy exercise indicator |
| nonhdltodd    | High non-HDL cholesterol   |
| pgdcgt        | Ethnicity                  |

# Database Start

| Database Name | Database Description                        |
|---------------|---------------------------------------------|
| cycle1        | Cycle 1 of the CHMS                         |
| cycle1_meds   | Medication Component of Cycle 1 of the CHMS |
| cycle2        | Cycle 2 of the CHMS                         |
| cycle2_meds   | Medication Component of Cycle 2 of the CHMS |
| cycle3        | Cycle 3 of the CHMS                         |
| cycle3_meds   | Medication Component of Cycle 3 of the CHMS |
| cycle4        | Cycle 4 of the CHMS                         |
| cycle4_meds   | Medication Component of Cycle 4 of the CHMS |
| cycle5        | Cycle 5 of the CHMS                         |
| cycle5_meds   | Medication Component of Cycle 5 of the CHMS |
| cycle6        | Cycle 6 of the CHMS                         |
| cycle6_meds   | Medication Component of Cycle 6 of the CHMS |
