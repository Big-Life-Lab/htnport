setwd("P:/10619/Dropbox/chmsflow")

source("R/table-1.R")

table2_data <- get_descriptive_data(
  imputed_cycles1to6_data,
  my_variables,
  my_variable_details,
  # All the variables whose descriptive statistics we want
  "highbp14090_adj",
  # Sets the stratifier
  list("all" = list("clc_sex"))
)

create_descriptive_table(
  table2_data,
  my_variables,
  my_variable_details,
  "highbp14090_adj",
  column_stratifier = c("clc_sex")
)

table2b_data <- get_descriptive_data(
  imputed_cycles1to6_data,
  my_variables,
  my_variable_details,
  # All the variables whose descriptive statistics we want
  recodeflow:::select_vars_by_role(
    c("Predictor"),
    my_variables
  ),
  # Sets the stratifier
  list("all" = list("highbp14090_adj"))
)

create_descriptive_table(
  table2b_data,
  my_variables,
  my_variable_details,
  recodeflow:::select_vars_by_role(
    c("Predictor"),
    my_variables
  ),
  column_stratifier = c("highbp14090_adj"),
  subjects_order = c("Age", "Marital status", "Education", "Occupation", "Family history", "Exercise", "Diet", "Weight", "Chronic disease", "Alcohol", "Smoking", "Sleep", "General")
)

imputed_cycles1to6_data <- imputed_cycles1to6_data %>%
  mutate(highbp14090_adj = ifelse(highbp14090_adj == 2, 0, highbp14090_adj),
         ccc_51 = ifelse(ccc_51 == 2, 0, ccc_51),
         ckd = ifelse(ckd == 2, 0, ckd),
         fmh_15 = ifelse(fmh_15 == 2, 0, fmh_15),
         smoke = ifelse(smoke == 2, 0, smoke),
         working = ifelse(working == 2, 0, working))

calculate_odds_ratios <- function(predictor) {
  model <- glm(highbp14090_adj ~ get(predictor), data = imputed_cycles1to6_data, family = binomial)
  odds_ratios <- exp(coef(model))
  conf_int <- exp(confint(model))
  return(list(odds_ratios = odds_ratios, conf_int = conf_int))
}

predictors <- recodeflow:::select_vars_by_role("Predictor", my_variables)

male_data <- filter(imputed_cycles1to6_data, clc_sex == 1)
female_data <- filter(imputed_cycles1to6_data, clc_sex == 2)

male_ORs <- lapply(predictors, calculate_odds_ratios, data = male_data)
female_ORs <- lapply(predictors, calculate_odds_ratios, data = female_data)

# Iterate over each predictor variable
for (i in seq_along(predictors)) {
  # Get number of levels in the predictor variable
  num_levels <- length(male_ORs[[i]]$odds_ratios)
  
  # Iterate over each level (excluding the reference level)
  for (level in 2:num_levels) {
    # Add Male ORs and CIs for this level
    male_row <- data.frame(
      Sex = "Male",
      Variable = paste0(predictors[i], "_", level),  # Adjust Variable name for the level
      Odds_Ratio = male_ORs[[i]]$odds_ratios[level],
      CI_Lower = male_ORs[[i]]$conf_int[level, 1],
      CI_Upper = male_ORs[[i]]$conf_int[level, 2]
    )
    male_OR_df <- rbind(male_OR_df, male_row)
    
    # Add Female ORs and CIs for this level
    female_row <- data.frame(
      Sex = "Female",
      Variable = paste0(predictors[i], "_", level),  # Adjust Variable name for the level
      Odds_Ratio = female_ORs[[i]]$odds_ratios[level],
      CI_Lower = female_ORs[[i]]$conf_int[level, 1],
      CI_Upper = female_ORs[[i]]$conf_int[level, 2]
    )
    female_OR_df <- rbind(female_OR_df, female_row)
  }
}

OR_df <- bind_rows(male_OR_df, female_OR_df)
OR_df$OR_CI <- paste(df$Odds_Ratio, " (", df$CI_Lower, " - ", df$CI_Upper, ")", sep = "")

OR_df <- OR_df %>%
  mutate(Variable = case_when(
    Variable == "ccc_51_2" ~ "Diabetes",
    Variable == "ckd_2" ~ "Chronic kidney disease",
    Variable == "clc_age_2" ~ "Age",
    Variable == "edudr04_2" ~ "Highest education level- Secondary",
    Variable == "edudr04_3" ~ "Highest education level - Post-secondary",
    Variable == "fmh_15_2" ~ "Hypertension family history",
    Variable == "gendmhi_2" ~ "Self-related mental health - Good",
    Variable == "gendmhi_3" ~ "Self-related mental health - Very good or excellent",
    Variable == "gen_025_2" ~ "Stress - A bit",
    Variable == "gen_025_3" ~ "Stress - Quite a bit or extremely",
    Variable == "gen_045_2" ~ "Sense of belonging - Weak",
    Variable == "hwmdbmi_2" ~ "Body mass index",
    Variable == "low_drink_score1_2" ~ "Alcohol consumption level - Former drinker",
    Variable == "low_drink_score1_3" ~ "Alcohol consumption level - Light drinker",
    Variable == "low_drink_score1_4" ~ "Alcohol consumption level - Moderate to heavy drinker",
    Variable == "married_2" ~ "Marital status - widowed, separated, or divorced",
    Variable == "married_3" ~ "Marital status - Single",
    Variable == "minperweek_2" ~ "Minutes of exercise per week",
    Variable == "slp_11_2" ~ "Hours of sleep per day",
    Variable == "smoke_2" ~ "Current smoker",
    Variable == "totalfv_2" ~ "Daily fruit and vegetable consumption",
    Variable == "whr_2" ~ "Waist-to-height ratio",
    Variable == "working_2" ~ "Working status - Has a job",
    TRUE ~ Variable  # Keep other values unchanged
  ))

OR_df <- select(OR_df, -c(Odds_Ratio, CI_Lower, CI_Upper))

table2c <- flextable(OR_df)
table2c <- table2c %>%
  set_table_properties(width = .8, layout = "autofit") %>%
  set_header_labels(
    OR_CI = "OR (95% CI)"
  ) %>%
  set_caption("Sex-statified bivariate association between hypertension and its risk factors")