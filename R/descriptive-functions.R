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

# Function to get weighted descriptives
summarize_weighted <- function(variables, design, data) {
  results <- data.frame()
  
  for (var in variables) {
    f <- as.formula(paste("~", var))
    
    # Weighted mean and variance (for SD)
    mean_val <- as.numeric(survey::svymean(f, design, na.rm = TRUE))
    var_val  <- as.numeric(survey::svyvar(f, design, na.rm = TRUE))
    sd_val   <- sqrt(var_val)
    
    # Weighted quantiles
    percentiles <- survey::svyquantile(
      f, design,
      c(0.05, 0.10, 0.20, 0.25, 0.30, 0.40,
        0.50, 0.60, 0.75, 0.80, 0.90, 0.95),
      na.rm = TRUE,
      ci = FALSE
    )
    percentiles <- unlist(percentiles)
    
    # Unweighted min/max
    min_val <- suppressWarnings(min(data[[var]], na.rm = TRUE))
    max_val <- suppressWarnings(max(data[[var]], na.rm = TRUE))
    
    # Combine all results
    results <- rbind(
      results,
      data.frame(
        variable = var,
        mean = mean_val,
        sd = sd_val,
        min = min_val,
        max = max_val,
        p5  = percentiles[1],
        p10 = percentiles[2],
        p20 = percentiles[3],
        p25 = percentiles[4],
        p30 = percentiles[5],
        p40 = percentiles[6],
        p50 = percentiles[7],
        p60 = percentiles[8],
        p75 = percentiles[9],
        p80 = percentiles[10],
        p90 = percentiles[11],
        p95 = percentiles[12]
      )
    )
  }
  
  return(results)
}