# Function to flatten cycles 3-6 long medication data and merge such data to overall cycle data
process_long_medication_data <- function(cycle_data, cycle_medication_data) {
  
  # Step 1: Group and summarize the medication data
  cycle_medication_data <- cycle_medication_data %>%
    dplyr::group_by(clinicid) %>%
    dplyr::summarize(
      meucatc = paste(unique(meucatc), collapse = ", "),   # Concatenate unique values of meucatc
      npi_25b = paste(unique(npi_25b), collapse = ", "),   # Concatenate unique values of npi_25b
      anymed = max(as.numeric(as.character(anymed))),      # Find the maximum of anymed
      diab_drug = max(as.numeric(as.character(diab_drug))) # Find the maximum of diab_drug
    )
  
  # Step 2: Merge the summarized data back to the original cycle_data
  cycle_data <- dplyr::left_join(cycle_data, cycle_medication_data, by = c("clinicid"))
  
  # Step 3: Create 'anymed2' and 'diab_drug2', and clean up columns
  cycle_data <- cycle_data %>%
    dplyr::mutate(anymed2 = anymed) %>%
    dplyr::mutate(diab_drug2 = diab_drug) %>%
    dplyr::select(-c(anymed, diab_drug))
  
  return(cycle_data)
}

# Function to ensure derived categorical variables present in all six cycles can be properly tabulated
recode_na_b <- function(column) {
  # Convert the column to character if it's a factor
  if (is.factor(column)) {
    column <- as.character(column)
  }
  # Replace "NA(c)" with "NA(b)"
  column[column == "NA(c)"] <- "NA(b)"
  return(column)
}

# Function to truncate continious variables
truncate_skewed <- function(df, threshold = 0.995, skew_threshold = 1) {
  
  # Create a copy of the dataframe to avoid overwriting the original
  df_truncated <- df
  
  # Loop over all the numeric columns
  for (col in c("clc_age", "hwmdbmi", "minperweek", "totalfv", "whr", "slp_11")) {
      
      # Calculate skewness
      skewness_value <- e1071::skewness(df[[col]], na.rm = TRUE)
      
      # Check if the variable is skewed
      if (abs(skewness_value) > skew_threshold) {
        
        # Calculate the 99.5th percentile
        quantile_value <- quantile(df[[col]], threshold, na.rm = TRUE)
        
        # Truncate the variable
        df_truncated[[col]] <- ifelse(df[[col]] > quantile_value, quantile_value, df[[col]])
        
        # Print message indicating that the column was truncated
        message(paste("Truncated column:", col, "| Skewness:", round(skewness_value, 2)))
      }
  }
  
  return(df_truncated)
}

# Function to center continuous variables
center_cont_variable <- function(var, design) {
  var <- design$variables[[var]]
  weighted_mean <- survey::svymean(~var, design = design)[1]
  centered_var <- var - weighted_mean
  return(centered_var)
}

# Function to center categorical variables using dummy variables (excluding reference category)
center_cat_variable <- function(var, design) {
  var_data <- design$variables[[var]]  # Extract the variable
  
  # Calculate weighted frequencies using dynamically created formula
  freq_table <- survey::svytable(as.formula(paste0("~", var)), design = design)
  
  # Weighted mean (proportions for each category)
  weighted_mean <- freq_table / sum(freq_table)  # Proportion for each category
  
  # Exclude the lowest-numbered category (reference)
  categories <- levels(var_data)[-1]  # Exclude the first level
  
  # Create centered dummy variables for remaining categories
  centered_dummies <- sapply(categories, function(category) {
    dummy <- as.numeric(var_data == category)  # Create dummy for the current category
    centered_dummy <- dummy - weighted_mean[category]  # Center the dummy variable
    return(centered_dummy)
  })
  
  # Ensure output is a matrix (even if there is only one category left)
  if (is.vector(centered_dummies)) {
    centered_dummies <- matrix(centered_dummies, ncol = 1)
  }
  
  # Name columns for clarity
  colnames(centered_dummies) <- paste0(var, "_", categories)
  return(centered_dummies)  # Return as a matrix
}

# Function to fit crude models and return ORs and CIs for all levels of a predictor
fit_crude_model <- function(predictor, design) {
  formula <- as.formula(paste("highbp14090_adj ~", predictor))
  model <- survey::svyglm(formula, design = design, family = quasibinomial())
  
  # Extract coefficients and calculate ORs
  coef_est <- coef(model)
  or <- round(exp(coef_est), 2)
  
  # Calculate confidence intervals
  conf_int <- exp(confint(model))
  
  # Create a data frame with the results
  # We use [-1] to exclude the intercept
  results <- data.frame(
    Variable = predictor,
    Level = names(coef_est)[-1],
    OR = or[-1],  # Exclude the intercept
    CI_Lower = round(conf_int[-1, 1], 2),
    CI_Upper = round(conf_int[-1, 2], 2)
  )
  
  results <- results %>%
    dplyr::mutate(Level = recode(Level,
                                 "ckd" = "Chronic kidney disease",
                                 "clc_age" = "Age",
                                 "diabx" = "Diabetes",
                                 "edudr04edudr04_1" = "High school graduate only",
                                 "edudr04edudr04_2" = "Did not graduate high school",
                                 "fmh_15" = "Family history for hypertension",
                                 "gendmhi" = "Poor or fair mental health",
                                 "gen_025" = "Quite a bit or extremely stressed",
                                 "gen_045" = "Weak sense of belonging",
                                 "hwmdbmi" = "Body mass index",
                                 "low_drink_score1low_drink_score1_2" = "Former drinker",
                                 "low_drink_score1low_drink_score1_3" = "Light drinker",
                                 "low_drink_score1low_drink_score1_4" = "Moderate to heavy drinker",
                                 "marriedmarried_2" = "Widowed, separated, or divorced",
                                 "marriedmarried_3" = "Single",
                                 "minperweek" = "Minutes of exercise per week",
                                 "slp_11" = "Sleep duration",
                                 "smokesmoke_1" = "Former smoker",
                                 "smokesmoke_2" = "Current smoker",
                                 "totalfv" = "Daily fruit and vegetable consumption",
                                 "whr" = "Waist-to-height ratio",
                                 "working" = "Does not have a job"))
  
  return(results)
}

# Function to create weighted density plots
create_weighted_density_plots <- function(
    data, 
    grouping_var, 
    weight_var, 
    vars_to_plot, 
    group_labels = c("Group 1", "Group 2"), 
    group_colors = c("blue", "red"), 
    plot_titles = NULL, 
    x_labels = NULL
) {
  
  # Validate inputs
  if (!grouping_var %in% names(data)) stop("Grouping variable not found in data.")
  if (!weight_var %in% names(data)) stop("Weight variable not found in data.")
  if (is.null(plot_titles)) plot_titles <- vars_to_plot
  if (is.null(x_labels)) x_labels <- vars_to_plot
  if (length(plot_titles) != length(vars_to_plot)) stop("Length of plot_titles must match vars_to_plot.")
  if (length(x_labels) != length(vars_to_plot)) stop("Length of x_labels must match vars_to_plot.")
  
  # Get unique grouping levels
  group_levels <- unique(data[[grouping_var]])
  if (length(group_levels) != 2) stop("Grouping variable must have exactly two levels.")
  
  # Split data into groups
  group1_data <- subset(data, data[[grouping_var]] == group_levels[1])
  group2_data <- subset(data, data[[grouping_var]] == group_levels[2])
  
  # Create survey designs
  group1_svy <- survey::svydesign(ids = ~1, data = group1_data, weights = group1_data[[weight_var]])
  group2_svy <- survey::svydesign(ids = ~1, data = group2_data, weights = group2_data[[weight_var]])
  
  # Iterate through variables to plot
  for (i in seq_along(vars_to_plot)) {
    variable <- vars_to_plot[i]
    plot_title <- plot_titles[i]
    x_label <- x_labels[i]
    
    # Check if the variable exists in the data
    if (!variable %in% names(data)) {
      warning(paste("Variable", variable, "not found in data. Skipping..."))
      next
    }
    
    # Calculate density estimates
    group1_density <- survey::svysmooth(as.formula(paste0("~", variable)), design = group1_svy)
    group2_density <- survey::svysmooth(as.formula(paste0("~", variable)), design = group2_svy)
    
    # Generate the plot
    plot(
      group1_density[[variable]]$x, group1_density[[variable]]$y, type = "l", col = group_colors[1],
      main = plot_title, xlab = x_label, ylab = "Density"
    )
    lines(group2_density[[variable]]$x, group2_density[[variable]]$y, col = group_colors[2])
    legend(
      "topright", legend = group_labels, col = group_colors, lty = 1,
      inset = 0.02, bg = 'white'
    )
  }
}

# Function to get weighted quantiles
get_weighted_quantiles <- function(variable, design, probs) {
  svyquantile(as.formula(paste0("~", variable)), design = design, quantiles = probs, na.rm = TRUE)
}