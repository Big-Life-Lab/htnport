setwd("P:/10619/Dropbox/Sept25")

library(recodeflow)
library(cchsflow)
library(dplyr)
library(readr)
library(haven)
library(cli)
library(pastecs)

source("R/alcohol.R")
source("R/blood pressure.R")
source("R/diabetes.R")
source("R/diet.R")
source("R/exercise.R")
source("R/family history.R")
source("R/income.R")
source("R/kidney.R")
source("R/sample.R")
source("R/smoking.R")

source("R/get-descriptive-data.R")
source("R/create-descriptive-table.R")

my_variables <- read.csv("P:/10619/Dropbox/Sept25/worksheets/variables.csv")
my_variable_details <- read.csv("P:/10619/Dropbox/Sept25/worksheets/variable-details.csv")

cycle1 <- read_stata("data/cycle1/cycle1.dta")
cycle2 <- read_stata("data/cycle2/cycle2.dta")
cycle3 <- read_stata("data/cycle3/cycle3.dta")
cycle4 <- read_stata("data/cycle4/cycle4.dta")
cycle5 <- read_stata("data/cycle5/cycle5.dta")
cycle6 <- read_stata("data/cycle6/cycle6.dta")
names(cycle6) <- tolower(names(cycle6)) 

cycle1_table1_data <- recodeflow::rec_with_table(cycle1, recodeflow:::select_vars_by_role(c("Table 1", "Fam"), my_variables), variable_details = my_variable_details, log = TRUE)
cycle2_table1_data <- recodeflow::rec_with_table(cycle2, recodeflow:::select_vars_by_role(c("Table 1", "Fam"), my_variables), variable_details = my_variable_details, log = TRUE)
cycle3_table1_data <- recodeflow::rec_with_table(cycle3, recodeflow:::select_vars_by_role(c("Table 1", "Fam"), my_variables), variable_details = my_variable_details, log = TRUE)
cycle4_table1_data <- recodeflow::rec_with_table(cycle4, recodeflow:::select_vars_by_role(c("Table 1", "Fam"), my_variables), variable_details = my_variable_details, log = TRUE)
cycle5_table1_data <- recodeflow::rec_with_table(cycle5, recodeflow:::select_vars_by_role(c("Table 1", "Sodium"), my_variables), variable_details = my_variable_details, log = TRUE)
cycle6_table1_data <- recodeflow::rec_with_table(cycle6, recodeflow:::select_vars_by_role(c("Table 1", "Sodium"), my_variables), variable_details = my_variable_details, log = TRUE)

cycles1to6_table1_data <- dplyr::bind_rows(cycle1_table1_data, cycle2_table1_data, cycle3_table1_data, cycle4_table1_data, cycle5_table1_data, cycle6_table1_data)
cycles1to6_table1_data <- dplyr::filter(cycles1to6_table1_data, insample == 1)

huiport_table1_data <- get_descriptive_data(
  cycles1to6_table1_data,
  my_variables,
  my_variable_details,
  # All the variables whose descriptive statistics we want
  recodeflow:::select_vars_by_role(
    c("Predictor"),
    my_variables
  ),
  # Sets the stratifier
  list("all" = list())
)
write.csv(huiport_table1_data, "P:/10619/Dropbox/Sept25/vignettes/Table 1/huiport_table1_data.csv")

sex_stratified_huiport_table1_data <- get_descriptive_data(
  cycles1to6_table1_data,
  my_variables,
  my_variable_details,
  # All the variables whose descriptive statistics we want
  recodeflow:::select_vars_by_role(
    c("Predictor"),
    my_variables
  ),
  # Sets the stratifier
  list("all" = list("clc_sex"))
)

create_descriptive_table(
  sex_stratified_huiport_table1_data,
  my_variables,
  my_variable_details,
  recodeflow:::select_vars_by_role(
    c("Predictor"),
    my_variables
  ),
  column_stratifier = c("clc_sex"),
  subjects_order = c("Age", "Sex", "Marital status", "Education", "Income", "Occupation", "Ethnicity", "Immigration", "Family history", "Exercise", "Diet", "Weight", "Height", "Chronic disease", "Alcohol", "Smoking", "Sleep", "General")
)

# cycles1to6_table1_data_summary <- as.data.frame(pastecs::stat.desc(cycles1to6_table1_data))
# write.csv(cycles1to6_table1_data_summary, "P:/10619/Dropbox/Sept25/vignettes/Table 1/table1_data_summary.csv")
# 
# male_data <- filter(cycles1to6_table1_data, clc_sex == 1)
# female_data <- filter(cycles1to6_table1_data, clc_sex == 2)
# 
# male__data_summary <- as.data.frame(pastecs::stat.desc(male_data))
# female_data_summary <- as.data.frame(pastecs::stat.desc(female_data))
# sex_stratified_data_summary <- dplyr::bind_rows(male__data_summary, female_data_summary)
# write.csv(sex_stratified_data_summary, "P:/10619/Dropbox/Sept25/vignettes/Table 1/sex_stratified_table1_data_summary.csv")
# 
# selected_stratified_cycles1to6_table1_data <- cycles1to6_table1_data %>%
#   group_by(clc_sex) %>%
#   select(highbp14090, agegroup4, married, nohsgrad, incq1, pgdcgt, img_03, gen_055, smkdsty, cardiov, fambp, famcvd60, mvpa150wk, poordiet, hwmdbmi, diabx, ckd, nonhdltodd, low_drink_score1, spa_020, gen_040, gendmhi)
# 
# table1 <- dplyr::bind_rows(count(selected_stratified_cycles1to6_table1_data, highbp14090 == 1),
#                            count(selected_stratified_cycles1to6_table1_data, agegroup4),
#                            count(selected_stratified_cycles1to6_table1_data, married == 1),
#                            count(selected_stratified_cycles1to6_table1_data, nohsgrad == 1),
#                            count(selected_stratified_cycles1to6_table1_data, incq1 == 1),
#                            count(selected_stratified_cycles1to6_table1_data, pgdcgt == 1),
#                            count(selected_stratified_cycles1to6_table1_data, img_03 == 1),
#                            count(selected_stratified_cycles1to6_table1_data, gen_055 == 1),
#                            count(selected_stratified_cycles1to6_table1_data, smkdsty %in% c(1, 2, 3)),
#                            count(selected_stratified_cycles1to6_table1_data, cardiov == 1),
#                            count(selected_stratified_cycles1to6_table1_data, fambp == 1),
#                            count(selected_stratified_cycles1to6_table1_data, famcvd60 == 1),
#                            count(selected_stratified_cycles1to6_table1_data, mvpa150wk == 2),
#                            count(selected_stratified_cycles1to6_table1_data, poordiet == 1),
#                            count(selected_stratified_cycles1to6_table1_data, hwmdbmi >= 25),
#                            count(selected_stratified_cycles1to6_table1_data, diabx == 1),
#                            count(selected_stratified_cycles1to6_table1_data, ckd == 1),
#                            count(selected_stratified_cycles1to6_table1_data, nonhdltodd == 1),
#                            count(selected_stratified_cycles1to6_table1_data, low_drink_score1 %in% c(3, 4, 5)),
#                            count(selected_stratified_cycles1to6_table1_data, spa_020 %in% c(3, 4, 5)),
#                            count(selected_stratified_cycles1to6_table1_data, gen_040 %in% c(3, 4, 5)),
#                            count(selected_stratified_cycles1to6_table1_data, gendmhi == 0))
# 
# sample_size <- sum(count(selected_stratified_cycles1to6_table1_data)$n)
# 
# table1 <- table1 %>%
#   mutate(pct = (n/sample_size) * 100)
# table1 <- recodeflow::set_data_labels(table1, variable_details = my_variable_details, variables_sheet = my_variables)
# write.csv(table1, "P:/10619/Dropbox/Sept25/vignettes/Table 1/table1.csv")