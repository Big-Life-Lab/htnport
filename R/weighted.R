# Create a vector of packages to import
packages_to_import <- c(
  "arsenal",
  "dplyr",
  "ggplot2",
  "haven",
  "kableExtra",
  "knitr",
  "sjPlot",
  "survey",
  "srvyr",
  "stringr",
  "tibble",
  "tidyr"
)

# Create a vector of all packages that are currently installed
installed_packages <- installed.packages()[,"Package"]

# Create a vector of any packages to be imported that are not installed
uninstalled_packages <- packages_to_import[! packages_to_import %in% installed_packages]

# If any packages to import are not installed, install them
if(length(uninstalled_packages) > 0) {
  
  install.packages(uninstalled_packages)
  
}

# Iterate through the vector of packages to import and load them into the Global Environment
import_packages <- lapply(
  X = packages_to_import,
  FUN = library,
  character.only = TRUE
)

# Import user-defined functions

# concordance()
# This function renders an HTML table of concordance values (e.g., Somers' D, Goodman's & Kruskal's Gamma, Kendal's Tau-c, C area under the Receiver Operating characteristic curve) that mimic SAS output
concordance <- function(svylreg) {
  
  # Throw an error message if any required arguments are not set
  validate_required_arguments <- sapply(
    X = c(
      "svylreg"
    ),
    FUN = function(x) {
      
      if(is.null(get(x))) {
        
        stop(
          paste0(
            "The ",
            x,
            " argument cannot be NULL."
          )
        )
        
      }
      
    }
  )
  
  data <-  cbind(
    svylreg$y,
    svylreg$fitted.values
  )
  ones <- data[data[, 1] == 1,]
  zeros <-  data[data[, 1] == 0,]
  events <- dim(ones)[1]
  nonevents <- dim(zeros)[1]
  pairs <-events * nonevents
  n <- events + nonevents
  m <- min(events, nonevents)
  
  cc <- matrix(0, nonevents, events)
  dc <- matrix(0, nonevents, events)
  tt <- matrix(0, nonevents, events)
  
  for (j in 1:nonevents) {
    
    for (i in 1:events) {
      
      if (ones[i, 2] > zeros[j, 2]) {
        
        cc[j, i]<- 1
        
      } else if (ones[i,2] < zeros[j,2]) {
        
        dc[j, i] <- 1
        
      } else if (ones[i, 2] == zeros[j, 2])  {
        
        tt[j, i] <- 1
        
      }
    }
    
  }
  
  # Build a table
  tab <- tibble(
    Statistic = c(
      "Percent concordant",
      "Percent discordant",
      "Percent tied",
      "Pairs",
      "Somers' D",
      "Gamma",
      "Tau-a",
      "c"
    ),
    Value = c(
      (sum(cc) / pairs) * 100,
      (sum(dc) / pairs) * 100,
      (sum(tt) / pairs) * 100,
      pairs,
      (sum(cc) - sum(dc)) / (sum(cc) + sum(dc) + sum(tt)),
      (sum(cc) - sum(dc)) / (sum(cc) + sum(dc)),
      2 *(sum(cc) - sum(dc)) / (n * n * (m - 1) / m),
      ((sum(cc) - sum(dc)) / (sum(cc) + sum(dc) + sum(tt)) + 1) / 2
    )
  )
  
  # Format vectors
  tab <- tab %>%
    mutate(
      Value = case_when(
        Statistic %in% c(
          "Percent concordant",
          "Percent discordant",
          "Percent tied"
        ) ~ format(
          x = round(
            x = Value,
            digits = 1
          ),
          nsmall = 1,
          scientific = FALSE
        ),
        Statistic == "Pairs" ~ format(
          x = round(
            x = Value,
            digits = 0
          ),
          big.mark = ",",
          nsmall = 0,
          scientific = FALSE
        ),
        TRUE ~ format(
          x = round(
            x = Value,
            digits = 3
          ),
          nsmall = 3,
          scientific = FALSE
        )
      )
    )
  
  # Render table
  kbl(
    x = tab,
    align = c("l", "r"),
    caption = "Association of predicted probabilities and observed responses"
  ) %>%
    kable_styling(
      bootstrap_options = c(
        "hover",
        "striped"
      )
    )
  
}

# get_chi_squared_test()
# This function renders an HTML table of Chi-squared test statistics using svychisq()
get_chi_squared_test <- function(formula = NULL, stat = "F", survey_design = NULL, na.rm = TRUE) {
  
  # Throw an error message if any required arguments are not set
  validate_required_arguments <- sapply(
    X = c(
      "formula",
      "stat",
      "survey_design"
    ),
    FUN = function(x) {
      
      if(is.null(get(x))) {
        
        stop(
          paste0(
            "The ",
            x,
            " argument cannot be NULL."
          )
        )
        
      }
      
    }
  )
  
  # Perform Chi-squared test of association using Chisq statistic
  x2_test <- svychisq(
    formula = formula,
    statistic = stat,
    design = survey_design,
    na.rm = na.rm
  )
  
  # Set label and statistic values based on stat argument value
  if(stat %in% c("Chisq")) {
    
    label_values <- c(
      names(x2_test$statistic),
      "Num DF",
      "Pr > F"
    )
    
    stat_values <- c(
      format(
        x = round(
          x = x2_test$statistic,
          digits = 4
        ),
        nsmall = 4
      ),
      format(
        x = round(
          x = x2_test$parameter["df"],
          digits = 2
        ),
        nsmall = 2
      ),
      ifelse(
        test = x2_test$p.value < 0.0001,
        yes = "< 0.0001",
        no = format(
          x = round(
            x = x2_test$p.value,
            digits = 4
          ),
          nsmall = "4"
        )
      )
    )
    
  } else if(stat %in% c("F")) {
    
    label_values <- c(
      names(x2_test$statistic),
      "Num DF",
      "Den DF",
      "Pr > F"
    )
    
    stat_values <- c(
      format(
        x = round(
          x = x2_test$statistic,
          digits = 4
        ),
        nsmall = 4
      ),
      format(
        x = round(
          x = x2_test$parameter["ndf"],
          digits = 2
        ),
        nsmall = 2
      ),
      format(
        x = round(
          x = x2_test$parameter["ddf"],
          digits = 2
        ),
        nsmall = 2
      ),
      ifelse(
        test = x2_test$p.value < 0.0001,
        yes = "< 0.0001",
        no = format(
          x = round(
            x = x2_test$p.value,
            digits = 4
          ),
          nsmall = "4"
        )
      )
    )
    
  }
  
  # Render table
  kbl(
    tibble(
      Label = label_values,
      Value = stat_values
    ),
    align = c("l", "r"),
    col.names = NULL,
    caption = paste0(
      x2_test$method,
      " (complete cases = ",
      sum(complete.cases(survey_design$variables)),
      "; incomplete cases = ",
      sum(! complete.cases(survey_design$variables)),
      ")"
    )
  ) %>%
    kable_styling(
      bootstrap_options = c(
        "hover",
        "striped"
      )
    )
  
}

# get_column1_risks()
# This function renders an HTML table of column 1 risks that mimics a SAS output
get_column1_risks <- function(formula = NULL, by = NULL, survey_design = NULL) {
  
  # Throw an error message if any required arguments are not set
  validate_required_arguments <- sapply(
    X = c(
      "formula",
      "by",
      "survey_design"
    ),
    FUN = function(x) {
      
      if(is.null(get(x))) {
        
        stop(
          paste0(
            "The ",
            x,
            " argument cannot be NULL."
          )
        )
        
      }
      
    }
  )
  
  # Get formula name and by name
  formula_name <- paste0(sub("~", "", formula), collapse = "")
  by_name <- paste0(sub("~", "", by), collapse = "")
  
  # Get cross tab
  cross_tab <- get_cross_tab(
    formula = formula,
    by = by,
    survey_design = survey_design,
    html = FALSE
  )
  
  # Get first factor level from the variable in the formula
  first_factor_level <- cross_tab %>%
    select(formula_name) %>%
    slice(1) %>%
    unlist %>%
    unname
  
  # Subset cross tab on first factor level
  cross_tab <- cross_tab %>%
    filter(!! sym(formula_name) == first_factor_level)
  
  # Retain select vectors
  vectors_to_keep <- c(
    by_name,
    "Row percent",
    "Row percent (SE)",
    "CI for row percent (lower)",
    "CI for row percent (upper)"
  )
  
  cross_tab <- cross_tab[vectors_to_keep]
  
  # Divide select vectors by 100
  cross_tab[-1] <- lapply(
    X = cross_tab[-1],
    FUN = function(x) {
      
      x / 100
      
    }
  )
  
  # Rename vectors
  names(cross_tab) <- c(
    "Risk",
    "Estimate",
    "Standard error",
    "95% CI (lower)",
    "95% CI (upper)"
  )
  
  # Update values in the first vector
  cross_tab$Risk <- paste("Row", cross_tab$Risk)
  
  # Compute a total row
  survey_stats_prop <- svyby(
    formula = formula,
    by = ~ factor(first_factor_level),
    design = survey_design,
    FUN = svymean,
    vartype = c("se","ci","cv"),
    keep.var = TRUE,
    keep.names = FALSE,
    na.rm = TRUE
  ) %>%
    pivot_longer(
      cols = names(.)[-1]
    )
  
  # Bind the total row to cross_tab
  cross_tab <- bind_rows(
    cross_tab,
    tibble(
      Risk = "Total",
      Estimate = survey_stats_prop$value[1],
      "Standard error" = survey_stats_prop$value[3],
      "95% CI (lower)" = survey_stats_prop$value[5],
      "95% CI (upper)" = survey_stats_prop$value[7]
    )
  )
  
  # Add a difference row
  
  # Begin by fitting a glm model
  model <- svyglm(
    formula = formula(paste0(formula_name, " ~ ", by_name)),
    design = survey_design,
    family = quasibinomial(link = "identity"),
    na.action = na.omit
  )
  
  # Get model coefficients
  model_coefficients <- as_tibble(summary(model)$coefficients)
  estimate <- model_coefficients$Estimate[length(model_coefficients$Estimate)]
  standard_error <- model_coefficients$`Std. Error`[length(model_coefficients$`Std. Error`)]
  
  # Get confidence intervals
  confidence_intervals <- as_tibble(confint(model))
  lower_bound <- unname(
    unlist(
      confidence_intervals[nrow(confidence_intervals), 1]
    )
  )
  upper_bound <- unname(
    unlist(
      confidence_intervals[nrow(confidence_intervals), 2]
    )
  )
  
  # Bind the results to cross_tab
  cross_tab <- bind_rows(
    cross_tab,
    tibble(
      Risk = "Difference (Row 1 - Row2)",
      Estimate = estimate,
      "Standard error" = standard_error,
      "95% CI (lower)" = lower_bound,
      "95% CI (upper)" = upper_bound
    )
  )
  
  # Round everything to four decimals
  cross_tab[-1] <- lapply(
    X = cross_tab[-1],
    FUN = function(x) {
      
      round(x, 4)
      
    }
  )
  
  # Compute complete cases
  complete_cases <- survey_design$variables %>%
    filter(complete.cases(.)) %>%
    nrow
  
  # Compute missing cases
  missing_cases <- survey_design$variables %>%
    filter(! complete.cases(.)) %>%
    nrow
  
  # Render table
  kbl(
    x = cross_tab,
    align = c("l", rep("r", length(names(cross_tab)) - 1)),
    caption = paste0(
      "Column 1 risks (complete cases = ",
      complete_cases,
      ")"
    )
  ) %>%
    kable_styling(
      bootstrap_options = c(
        "hover",
        "striped"
      )
    )
  
}

# get_cross_tab()
# This function renders an HTML table of survey statistics using svyby()
get_cross_tab <- function(formula = NULL, by = NULL, survey_design = NULL, html = TRUE, select = NULL) {
  
  # Throw an error message if any required arguments are not set
  validate_required_arguments <- sapply(
    X = c(
      "formula",
      "by",
      "survey_design"
    ),
    FUN = function(x) {
      
      if(is.null(get(x))) {
        
        stop(
          paste0(
            "The ",
            x,
            " argument cannot be NULL."
          )
        )
        
      }
      
    }
  )
  
  # Get formula and by names
  formula_name <- paste0(sub("~", "", formula), collapse = "")
  by_name <- paste0(sub("~", "", by), collapse = "")
  
  # formula name levels
  formula_name_levels <- survey_design$variables %>%
    select(!! sym(formula_name)) %>%
    na.omit %>%
    unique %>%
    unname %>%
    unlist %>%
    as.character %>%
    sort
  
  # Build a tibble of select survey statistics
  t <- as.data.frame.matrix(
    x = table(survey_design$variables)
  )
  
  # Convert row names to a vector and rename
  t <- bind_cols(
    by_variable = row.names(t),
    t
  ) %>% rename(
    !! by_name := by_variable
  )
  
  # Pivot the tibble longer and rename vectors
  t <- t %>%
    pivot_longer(
      cols = names(t)[-1]
    ) %>%
    rename(
      !! formula_name := name,
      Frequency = value
    ) %>%
    mutate(Frequency = format(Frequency, big.mark = ","))
  
  # Add frequency and proportion statistics
  t <- t %>%
    bind_cols(
      svyby(
        formula = formula,
        by = by,
        design = survey_design,
        FUN = svytotal,
        vartype = c("se","ci","cv"),
        method = "logit",
        keep.var = TRUE,
        keep.names = FALSE,
        na.rm = TRUE
      ) %>%
        pivot_longer(
          cols = names(.)[-1]
        ) %>%
        rowwise() %>%
        mutate(
          level = unname(
            unlist(
              across(
                .cols = name,
                .fns = ~ formula_name_levels[str_ends(.x, formula_name_levels)]
              )
            )
          ),
          name = unname(
            unlist(
              across(
                .cols = name,
                .fns = ~ substr(.x, 1, nchar(.x) - length(level))
              )
            )
          )
        ) %>%
        arrange(names(.)[1], names(.)[4]) %>%
        pivot_wider(
          id_cols = c(by_name, "level"),
          names_from = "name",
          values_from = "value"
        ) %>%
        rename(
          `Weighted frequency` = all_of(formula_name),
          `Weighted frequency (SE)` = se,
          `CI for percent (lower)` = names(.)[str_starts(names(.), "ci_l.")],
          `CI for percent (upper)` = names(.)[str_starts(names(.), "ci_u.")],
          `CV for percent` = names(.)[str_starts(names(.), "cv.")]
        ) %>%
        mutate(
          `Weighted frequency (SE)` = format(round(`Weighted frequency (SE)`, 0), big.mark = ","),
          `CI for percent (lower)` = round(`CI for percent (lower)` / sum(`Weighted frequency`) * 100, 4),
          `CI for percent (upper)` = round(`CI for percent (upper)` / sum(`Weighted frequency`) * 100, 4),
          `CV for percent` = round(`CV for percent`, 4),
          `Weighted frequency` = format(round(`Weighted frequency`, 0), big.mark = ",")
        ) %>%
        select(
          -all_of(
            c(
              by_name,
              "level"
            )
          )
        ),
      svyby(
        formula = formula,
        by = by,
        design = survey_design,
        FUN = svymean,
        vartype = c("se","ci","cv"),
        keep.var = TRUE,
        keep.names = FALSE,
        na.rm = TRUE
      ) %>%
        pivot_longer(
          cols = names(.)[-1]
        ) %>%
        rowwise() %>%
        mutate(
          level = unname(
            unlist(
              across(
                .cols = name,
                .fns = ~ formula_name_levels[str_ends(.x, formula_name_levels)]
              )
            )
          ),
          name = unname(
            unlist(
              across(
                .cols = name,
                .fns = ~ substr(.x, 1, nchar(.x) - length(level))
              )
            )
          )
        ) %>%
        arrange(names(.)[1], names(.)[4]) %>%
        pivot_wider(
          id_cols = c(by_name, "level"),
          names_from = "name",
          values_from = "value"
        ) %>%
        rename(
          `Row percent` = all_of(formula_name),
          `Row percent (SE)` = se,
          `CI for row percent (lower)` = names(.)[str_starts(names(.), "ci_l.")],
          `CI for row percent (upper)` = names(.)[str_starts(names(.), "ci_u.")],
          `CV for row percent` = names(.)[str_starts(names(.), "cv.")]
        ) %>%
        mutate(
          across(
            .cols = c(
              "Row percent",
              "Row percent (SE)",
              "CI for row percent (lower)",
              "CI for row percent (upper)"
            ),
            .fns = ~ round(.x * 100, 4)
          ),
          `CV for row percent` = round(`CV for row percent`, 4)
        ) %>%
        select(
          -all_of(
            c(
              by_name,
              "level"
            )
          )
        )
    )
  
  # Compute complete cases
  complete_cases <- survey_design$variables %>%
    filter(complete.cases(.)) %>%
    nrow
  
  # Compute missing cases
  missing_cases <- survey_design$variables %>%
    filter(! complete.cases(.)) %>%
    nrow
  
  # If the select argument is set, subset t
  if(length(select) > 0) {
    
    t <- t[select]
    font_size <- NULL
    
  } else {
    
    font_size <- 9
    
  }
  
  # If the html argument is set to TRUE, render an HTML table
  if(isTRUE(html)) {
    
    # Build table
    tab <- kbl(
      x = t %>%
        select(-by_name),
      align = rep("r", length(names(t))),
      caption = paste0(
        "Table of ",
        formula_name,
        " by ",
        by_name,
        " (complete cases = ",
        complete_cases,
        "; incomplete cases = ",
        missing_cases,
        ")"
      )
    ) %>%
      kable_styling(
        bootstrap_options = c(
          "hover",
          "striped"
        ),
        font_size = font_size
      )
    
    # Group rows
    
    # Get unique values in the by variable
    by_values <- unique(unlist(t[by_name]))
    
    # Iterate unique values
    for(x in by_values) {
      
      # Get row numbers
      row_numbers <- t %>%
        mutate(row_number = row_number()) %>%
        filter(!! sym(by_name) == x) %>%
        select(row_number) %>%
        unname() %>%
        unlist()
      
      # Add grouping row
      tab <- tab %>%
        pack_rows(
          group_label = paste0(by_name, " = ", x),
          start_row = min(row_numbers),
          end_row = max(row_numbers)
        )
      
    }
    
    # Render table
    return(tab)
    
    # Else, return a data frame
  } else {
    
    return(t)
    
  }
  
}

# get_quantiles()
# This function returns a tibble of quantile statistics using svyquantile()
get_quantiles <- function(survey_design = NULL, formula = NULL, label = NULL, filter_variable = NULL, filter_value = NULL, quantiles = NULL) {
  
  # Throw an error message if any required arguments are not set
  validate_required_arguments <- sapply(
    X = c(
      "survey_design",
      "formula",
      "label",
      "filter_variable",
      "filter_value",
      "quantiles"
    ),
    FUN = function(x) {
      
      if(is.null(get(x))) {
        
        stop(
          paste0(
            "The ",
            x,
            " argument cannot be NULL."
          )
        )
        
      }
      
    }
  )
  
  # Compute survey quantiles
  output <- as_tibble(
    svyquantile(
      x = formula,
      design = survey_design %>%
        filter(eval(parse(text = filter_variable)) == filter_value),
      quantiles = quantiles,
      interval.type = "quantile",
      ties = "rounded",
      na.rm = TRUE
    )[[1]]
  ) %>%
    mutate(
      Variable = paste0(sub("~", "", formula), collapse = ""),
      Label = label,
      !! filter_variable := filter_value,
      Percentile = quantiles * 100
    ) %>%
    rename(
      Estimate = quantile,
      `Standard error` = se.97.5,
      `95% CI (lower)` = ci.2.5,
      `95% CI (upper)` = ci.97.5
    ) %>%
    mutate(
      across(
        .cols = c(
          Estimate,
          `Standard error`,
          `95% CI (lower)`,
          `95% CI (upper)`
        ),
        .fns = ~ round(.x, 4)
      )
    ) %>%
    relocate(Variable, Label, !! filter_variable, Percentile, Estimate, `Standard error`)
  
  # Return output
  return(output)
  
}

# get_survey_design_summary()
# This function renders an HTML table of survey design summary statistics that mimics a SAS survey design summary
get_survey_design_summary <- function(survey_design = NULL, type = NULL) {
  
  # Throw an error message if any required arguments are not set
  validate_required_arguments <- sapply(
    X = c(
      "survey_design",
      "type"
    ),
    FUN = function(x) {
      
      if(is.null(get(x))) {
        
        stop(
          paste0(
            "The ",
            x,
            " argument cannot be NULL."
          )
        )
        
      }
      
    }
  )
  
  # Summarize the survey design
  summary <- summary(survey_design)
  
  # If the type argument is set to "data summary"
  if(type %in% c("data summary")) {
    
    # Build a tibble of select summary statistics
    t <- tibble(
      Variable = c(
        "Number of observations",
        "Sum of weights"
      ),
      Summary = c(
        nrow(summary$variables),
        sum(summary$pweights)
      )
    )
    
    # Format numbers
    t$Summary <- format(
      x = round(
        x = t$Summary,
        digits = 0
      ),
      nsmall = 0,
      big.mark = ","
    )
    
    # Render table
    kbl(
      x = t,
      align = c("l", "r"),
      col.names = NULL
    ) %>%
      kable_styling(
        bootstrap_options = c(
          "hover",
          "striped"
        )
      ) %>%
      add_header_above(c("Data summary" = 2))
    
    # If the type argument is set to "variance estimation"
  } else if(type %in% c("variance estimation")) {
    
    # Build a tibble of select summary statistics
    t <- tibble(
      Variable = c(
        "Method",
        "Number of replicates"
      ),
      Summary = c(
        summary$type,
        ncol(summary$repweights)
      )
    )
    
    # Render table
    kbl(
      x = t,
      align = c("l", "r"),
      col.names = NULL
    ) %>%
      kable_styling(
        bootstrap_options = c(
          "hover",
          "striped"
        )
      ) %>%
      add_header_above(c("Variance estimation" = 2))
    
  }
  
}

# get_wald_test()
# This function renders an HTML table of Wald test statistics using regTermTest()
get_wald_test <- function(model = NULL, test_terms = NULL, df = NULL) {
  
  # Throw an error message if any required arguments are not set
  validate_required_arguments <- sapply(
    X = c(
      "model",
      "test_terms",
      "df"
    ),
    FUN = function(x) {
      
      if(is.null(get(x))) {
        
        stop(
          paste0(
            "The ",
            x,
            " argument cannot be NULL."
          )
        )
        
      }
      
    }
  )
  
  # Run a Wald test
  wald_test <- regTermTest(
    model = model,
    test.terms = formula(test_terms),
    df = df
  )
  
  # Build a table of the results
  tab <- tibble(
    Label = c(
      "F",
      "Num DF",
      "Den DF",
      "Pr > F"
    ),
    Value = c(
      format(
        x = round(
          x = wald_test$Ftest,
          digits = 4
        ),
        big.mark = ","
      ),
      format(
        x = round(
          x = wald_test$df,
          digits = 2
        ),
        nsmall = 2
      ),
      format(
        x = round(
          x = wald_test$ddf,
          digits = 2
        ),
        nsmall = 2
      ),
      ifelse(
        test = wald_test$p < 0.0001,
        yes = "< 0.0001",
        no = format(
          x = round(
            x = wald_test$p,
            digits = 4
          ),
          nsmall = 4
        )
      )
    )
  )
  
  # Render table
  kbl(
    x = tab,
    align = c("l", "r"),
    col.names = NULL,
    caption = paste0(
      "Wald test",
      " (complete cases = ",
      sum(complete.cases(survey_design$variables)),
      "; incomplete cases = ",
      sum(! complete.cases(survey_design$variables)),
      ")"
    )
  ) %>%
    kable_styling(
      bootstrap_options = c(
        "hover",
        "striped"
      )
    )
  
}

# Import data
# Import SAS data files
c2 <- read_sas(
  data_file = "data/sas data synthetic/cycle2_synthetic.sas7bdat"
)

c3clc <- read_sas(
  data_file = "data/sas data synthetic/cycle3_clc_synthetic.sas7bdat"
)

c3el <- read_sas(
  data_file = "data/sas data synthetic/cycle3_el_synthetic.sas7bdat"
)

c3hhd <- read_sas(
  data_file = "data/sas data synthetic/cycle3_hhd_synthetic.sas7bdat"
)

c3fast <- read_sas(
  data_file = "data/sas data synthetic/cycle3_fasted_synthetic.sas7bdat"
)

bswc2 <- read_sas(
  data_file = "data/sas data synthetic/bootstrap_synthetic_c2.sas7bdat"
)

bswc3 <- read_sas(
  data_file = "data/sas data synthetic/bootstrap_synthetic_c3.sas7bdat"
)

bswc2c3 <- read_sas(
  data_file = "data/sas data synthetic/bootstrap_synthetic_c2_c3.sas7bdat"
)

bswc3fast <- read_sas(
  data_file = "data/sas data synthetic/bootstrap_synthetic_fasted_c3.sas7bdat"
)

# Step 1
# Step 1a

# Create a dataset that includes all variables from the fasted sub-sample data file, the fasted sub-sample bootstrap weights and also the age, sex and BMI variables from the full sample clinic data file for respondents that were part of the fasted sub-sample.

# Perform multiple left joins to merge data from the c3fast, bswc3fast and c3clc data frames
fasted <- left_join(
  x = c3fast,
  y = bswc3fast %>%
    select(-WGT_FAST),
  by = "CLINICID"
) %>%
  left_join(
    x = .,
    y = c3clc %>%
      select(
        CLINICID,
        CLC_SEX,
        CLC_AGE,
        HWMDBMIA
      ),
    by = "CLINICID"
  )

# Step 1b

# Create a dataset that includes all variables from the full sample household data file, clinic and environmental lab data files, and the full sample bootstrap weights data file.

# Perform multiple left joins to merge data from the c3hhd, c3clc, c3el and bswc3 data frames
c3full <- left_join(
  x = c3hhd,
  y = c3clc %>%
    select(-WGT_FULL),
  by = "CLINICID"
) %>%
  left_join(
    x = .,
    y = c3el %>%
      select(-WGT_FULL),
    by = "CLINICID"
  ) %>%
  left_join(
    x = .,
    y = bswc3 %>%
      select(-WGT_FULL),
    by = "CLINICID"
  )

# Step 2

# Create derived variables used in the hands-on examples.

# Derive agegrp (age group at clinic visit)
c3full <- c3full %>%
  mutate(
    agegrp = factor(
      x = case_when(
        CLC_AGE >= 3 & CLC_AGE <= 5 ~ 1,
        CLC_AGE >= 6 & CLC_AGE <= 11 ~ 2,
        CLC_AGE >= 12 & CLC_AGE <= 19 ~ 3,
        CLC_AGE >= 20 & CLC_AGE <= 39 ~ 4,
        CLC_AGE >= 40 & CLC_AGE <= 59 ~ 5,
        CLC_AGE >= 60 & CLC_AGE <= 79 ~ 6,
      ),
      levels = 1:6
    )
  )

# Derive Hsys
c3full <- c3full %>%
  mutate(
    Hsys = case_when(
      BPMDPBPS >= 0 & BPMDPBPS < 140 ~ 2,
      BPMDPBPS >= 140 & BPMDPBPS < 996 ~ 1,
      BPMDPBPS >= 996 ~ NA,
      TRUE ~ 1 # Assign 1 if no other condition met
    )
  )

# Derive Hdias
c3full <- c3full %>%
  mutate(
    Hdias = case_when(
      BPMDPBPD >= 0 & BPMDPBPD < 90 ~ 2,
      BPMDPBPD >= 90 & BPMDPBPD < 996 ~ 1,
      BPMDPBPD >= 996 ~ NA,
      TRUE ~ 1 # Assign 1 if no other condition met
    )
  )

# Derive highBP (1 = high blood pressure, 2 = not high blood pressure)
c3full <- c3full %>%
  mutate(
    highBP = case_when(
      Hsys == 1 | Hdias == 1 | CCC_32 == 1 ~ 1,
      is.na(Hsys) & is.na(Hdias) & CCC_32 != 1 ~ NA,
      TRUE ~ 2 # Assign 2 if no other condition met
    )
  )

# Derive highBPreg
c3full <- c3full %>%
  mutate(
    highBPreg = factor(
      x = case_when(
        highBP == 1 ~ 1,
        highBP == 2 ~ 0,
        TRUE ~ NA # Assign NA if no other condition met
      ),
      levels = 0:1
    )
  )

# Derive BMI
c3full <- c3full %>%
  mutate(
    BMI = case_when(
      HWMDBMI >= 99.96 ~ NA,
      TRUE ~ HWMDBMI # Retain BMI value if no other condition met
    )
  )

# Derive BMIcat3
c3full <- c3full %>%
  mutate(
    BMIcat3 = factor(
      x = case_when(
        CLC_AGE %in% 20:80 & HWMDBMIA %in% 1:2 ~ 1,
        CLC_AGE %in% 20:80 & HWMDBMIA %in% 3 ~ 2,
        CLC_AGE %in% 20:80 & HWMDBMIA %in% 4:6 ~ 3,
        TRUE ~ NA # Assign NA if no other condition met
      ),
      levels = 1:3
    )
  )

# Derive sex
c3full$sex <- factor(
  x = c3full$CLC_SEX,
  levels = 1:2
)

# Derive age
c3full$age <- c3full$CLC_AGE

# Derive sleepprob
c3full <- c3full %>%
  mutate(
    sleepprob = factor(
      x = case_when(
        SLP_12 %in% 1:2 ~ 1,
        SLP_12 %in% 3 ~ 2,
        SLP_12 %in% 4 ~ 3,
        SLP_12 %in% 5 ~ 4,
        TRUE ~ NA # Assign NA if no other condition met
      ),
      levels = 1:4
    )
  )

# Derive sleephrs
c3full <- c3full %>%
  mutate(
    sleephrs = case_when(
      SLP_11 >= 99.7 ~ NA,
      TRUE ~ SLP_11 # Assign SLP_11 value if no other condition met
    )
  )

# Full example of imputation and conversion

# STEP A: Convert all 95s to one half of the LOD values
# STEP B: Convert the reserve codes equal to "valid skip" or "not stated" to blanks
# STEP C: Convert all values to conventional units
# STEP D: Get the natural log of the conventional units

# Steps A-D

# Derive Bmercury, Bmercury_CU and Bmercury_CU_log
c3full <- c3full %>%
  mutate(
    Bmercury = case_when(
      LAB_BHG == 999.5 ~ 2.1 / 2,
      LAB_BHG >  999.5 ~ NA,
      TRUE ~ LAB_BHG # Assign LAB_BHG value if no other condition met
    ),
    Bmercury_CU = Bmercury * 0.2006,
    Bmercury_CU_log = log(Bmercury_CU)
  )

# Step 3
# Step 3a: estimate proportions

# Estimate the distribution of sleeping trouble across sex.

# Specify the survey design (balanced repeated replication for estimates)
survey_design <- c3full %>%
  as_survey_rep(
    variables = c(
      "sex",
      "sleepprob"
    ),
    weights = WGT_FULL,
    repweights = starts_with("bsw"),
    type = "BRR",
    mse = TRUE
  )

# Set degrees of freedom to 11 (16 sites - 5 Regions)
survey_design$degf <- 11

# Data summary

# Render data summary
get_survey_design_summary(
  survey_design = survey_design,
  type = "data summary"
)

# Variance estimation

# Render variance estimation
get_survey_design_summary(
  survey_design = survey_design,
  type = "variance estimation"
)

# Frequency statistics

# Render frequency statistics
get_cross_tab(
  formula = ~ sleepprob,
  by = ~ sex,
  survey_design = survey_design
)

# Tests of association

# First-order correction

# Perform Chi-squared test of association using Chisq statistic
get_chi_squared_test(
  formula = ~ sleepprob + sex,
  stat = "Chisq",
  survey_design = survey_design,
  na.rm = TRUE
)

# Note: the alternative statistic ("Chisq") adjusts the Pearson Chi-squared statistic by a design effect estimate and then compares it to the Chi-squared distribution it would have under simple random sampling.

# Note: the p-values are computed with a Satterthwaite approximation to the distribution and with denominator degrees of freedom as recommended by Thomas and Rao (1990).

# Second-order correction

# Perform Chi-squared test of association using F statistic
get_chi_squared_test(
  formula = ~ sleepprob + sex,
  stat = "F",
  survey_design = survey_design,
  na.rm = TRUE
)

# Note: “F”, the default test option, is the Rao-Scott second-order correction.

# Step 3b: testing differences between proportions

# Testing differences in proportions between males and females for each category of sleeping trouble.

# sleepprob equals 1

# Code sleepprob1 into a dummy variable
c3full <- c3full %>%
  mutate(
    sleepprob1 = factor(
      x = case_when(
        sleepprob == 1 ~ 1,
        sleepprob != 1 ~ 2
      ),
      levels = 1:2
    )
  )

# Specify survey design
survey_design <- c3full %>%
  as_survey_rep(
    variables = c("sex", "sleepprob1"),
    weights = WGT_FULL,
    repweights = starts_with("bsw"),
    type = "BRR",
    mse = TRUE
  )

# Set degrees of freedom to 11 (16 sites - 5 Regions)
survey_design$degf <- 11

# Data summary

# Render data summary
get_survey_design_summary(
  survey_design = survey_design,
  type = "data summary"
)

# Variance estimation

# Render variance estimation
get_survey_design_summary(
  survey_design = survey_design,
  type = "variance estimation"
)

# Frequency statistics

# Render frequency statistics
get_cross_tab(
  formula = ~ sleepprob1,
  by = ~ sex,
  survey_design = survey_design,
  select = c(
    "sex",
    "sleepprob1",
    "Weighted frequency",
    "Weighted frequency (SE)",
    "Row percent",
    "Row percent (SE)"
  )
)

# Column 1 risks

# Render column 1 risk statistics
get_column1_risks(
  formula = ~ sleepprob1,
  by = ~ sex,
  survey_design = survey_design
)

# sleepprob equals 2

# Code sleepprob2 into a dummy variable
c3full <- c3full %>%
  mutate(
    sleepprob2 = factor(
      x = case_when(
        sleepprob == 2 ~ 1,
        sleepprob != 2 ~ 2
      ),
      levels = 1:2
    )
  )

# Specify survey design
survey_design <- c3full %>%
  as_survey_rep(
    variables = c("sex", "sleepprob2"),
    weights = WGT_FULL,
    repweights = starts_with("bsw"),
    type = "BRR",
    mse = TRUE
  )

# Set degrees of freedom to 11 (16 sites - 5 Regions)
survey_design$degf <- 11

# Data summary

# Render data summary
get_survey_design_summary(
  survey_design = survey_design,
  type = "data summary"
)

# Variance estimation

# Render variance estimation
get_survey_design_summary(
  survey_design = survey_design,
  type = "variance estimation"
)

# Frequency statistics

# Render frequency statistics
get_cross_tab(
  formula = ~ sleepprob2,
  by = ~ sex,
  survey_design = survey_design,
  select = c(
    "sex",
    "sleepprob2",
    "Weighted frequency",
    "Weighted frequency (SE)",
    "Row percent",
    "Row percent (SE)"
  )
)

# Column 1 risks

# Render column 1 risk statistics
get_column1_risks(
  formula = ~ sleepprob2,
  by = ~ sex,
  survey_design = survey_design
)

# sleepprob equals 3

# Code sleepprob3 into a dummy variable
c3full <- c3full %>%
  mutate(
    sleepprob3 = factor(
      x = case_when(
        sleepprob == 3 ~ 1,
        sleepprob != 3 ~ 2
      ),
      levels = 1:2
    )
  )

# Specify survey design
survey_design <- c3full %>%
  as_survey_rep(
    variables = c("sex", "sleepprob3"),
    weights = WGT_FULL,
    repweights = starts_with("bsw"),
    type = "BRR",
    mse = TRUE
  )

# Set degrees of freedom to 11 (16 sites - 5 Regions)
survey_design$degf <- 11

# Data summary

# Render data summary
get_survey_design_summary(
  survey_design = survey_design,
  type = "data summary"
)

# Variance estimation

# Render variance estimation
get_survey_design_summary(
  survey_design = survey_design,
  type = "variance estimation"
)

# Frequency statistics

# Render frequency statistics
get_cross_tab(
  formula = ~ sleepprob3,
  by = ~ sex,
  survey_design = survey_design,
  select = c(
    "sex",
    "sleepprob3",
    "Weighted frequency",
    "Weighted frequency (SE)",
    "Row percent",
    "Row percent (SE)"
  )
)

# Column 1 risks

# Render column 1 risk statistics
get_column1_risks(
  formula = ~ sleepprob3,
  by = ~ sex,
  survey_design = survey_design
)

# sleepprob equals 4

# Code sleepprob4 into a dummy variable
c3full <- c3full %>%
  mutate(
    sleepprob4 = factor(
      x = case_when(
        sleepprob == 4 ~ 1,
        sleepprob != 4 ~ 2
      ),
      levels = 1:2
    )
  )

# Specify survey design
survey_design <- c3full %>%
  as_survey_rep(
    variables = c("sex", "sleepprob4"),
    weights = WGT_FULL,
    repweights = starts_with("bsw"),
    type = "BRR",
    mse = TRUE
  )

# Set degrees of freedom to 11 (16 sites - 5 Regions)
survey_design$degf <- 11

# Data summary

# Render data summary
get_survey_design_summary(
  survey_design = survey_design,
  type = "data summary"
)

# Variance estimation

# Render variance estimation
get_survey_design_summary(
  survey_design = survey_design,
  type = "variance estimation"
)

# Frequency statistics

# Render frequency statistics
get_cross_tab(
  formula = ~ sleepprob4,
  by = ~ sex,
  survey_design = survey_design,
  select = c(
    "sex",
    "sleepprob4",
    "Weighted frequency",
    "Weighted frequency (SE)",
    "Row percent",
    "Row percent (SE)"
  )
)

# Column 1 risks

# Render column 1 risk statistics
get_column1_risks(
  formula = ~ sleepprob4,
  by = ~ sex,
  survey_design = survey_design
)

# Step 4
# Step 4a: estimate arithmetic means

# Estimate the average number of hours spent sleeping across age groups.

# Specify survey design
survey_design <- c3full %>%
  as_survey_rep(
    variables = c("agegrp", "sleephrs"),
    weights = WGT_FULL,
    repweights = starts_with("bsw"),
    type = "BRR",
    mse = TRUE
  )

# Set degrees of freedom to 11 (16 sites - 5 Regions)
survey_design$degf <- 11

# Compute survey statistics
sleephbyage <- svyby(
  formula = ~ sleephrs,
  by = ~ agegrp,
  design = survey_design,
  FUN = svymean,
  vartype = c("se","ci","cv"),
  keep.var = TRUE,
  keep.names = FALSE,
  na.rm = TRUE
)

# Add a sample size vector
sleephbyage <- sleephbyage %>%
  bind_cols(
    survey_design$variables %>%
      na.omit %>%
      count(agegrp) %>%
      select(n)
  ) %>%
  mutate(n = format(n, big.mark = ",")) %>%
  relocate(n, .after = agegrp)

# Add other vectors
sleephbyage <- sleephbyage %>%
  mutate(
    Variable = "sleephrs",
    Label = "Number of hours of sleep in 24 hr period"
  ) %>%
  relocate(Variable, Label, .after = agegrp)

# Round select vectors to 4 digits
sleephbyage <- sleephbyage %>%
  mutate(
    across(
      .cols = c(
        sleephrs,
        se,
        ci_l,
        ci_u,
        cv.sleephrs
      ),
      .fns = ~ round(.x, 4)
    )
  )

# Rename select vectors
sleephbyage <- sleephbyage %>%
  rename(
    N = n,
    Mean = sleephrs,
    `Std Error of Mean` = se,
    `95% CI for mean (lower)` = ci_l,
    `95% CI for mean (upper)` = ci_u,
    `Coeff of variation` = cv.sleephrs
  )

# Render table
kbl(
  x = sleephbyage,
  caption = "Statistics for agegrp domains"
) %>%
  kable_styling(
    bootstrap_options = c(
      "striped",
      "hover"
    ),
    font_size = 9
  )

# Step 4b: testing differences between arithmetic means

# Tests whether the average number of hours spent sleeping by 6- to 11-year-olds is statistically different from that of each of the other age groups.

# Generalized linear model

# Fit a survey-weighted generalized linear model
# The svyglm function uses MLE so it doesn't produce a coefficient of determination as calculated by OLS regression

# Add labels to the agegrp factor and re-level
survey_design$variables <- survey_design$variables %>%
  mutate(
    agegrp = factor(
      x = agegrp,
      levels = c(2:1, 3:6),
      labels = c(
        "6- to 11-year-olds",
        "3- to 5-year-olds",
        "12- to 19-year-olds",
        "20- to 39-year-olds",
        "40- to 59-year-olds",
        "60- to 80-year-olds"
      )
    )
  )

# Fit model
g2vsall <- svyglm(
  formula = sleephrs ~ agegrp,
  design = na.omit(survey_design)
)

# Render model summary stats
tab_model(
  model = g2vsall,
  show.df = TRUE,
  show.se = TRUE,
  show.stat = TRUE,
  show.aic = TRUE,
  show.obs = TRUE,
  digits = 4,
  digits.p = 4,
  pred.labels = c(
    "(Intercept)",
    paste0(
      "Age group ",
      c(1, 3:6),
      " (ref: age group 2)"
    )
  ),
  dv.labels = Reduce(paste, deparse(g2vsall$formula)),
  string.est = "Estimate",
  string.se = "SE",
  string.df = "Residual df",
  string.stat = "t value",
  col.order = c(
    "est",
    "se",
    "ci",
    "df.error",
    "stat",
    "p"
  )
)

# Wald test

# Render Wald test statistics
get_wald_test(
  model = g2vsall,
  test_terms = ~ agegrp,
  df = 11
)

# Step 5
# Step 5a: histogram

# Produces a weighted histogram of the distribution of total blood mercury concentrations across the population.

# Specify survey design
survey_design <- c3full %>%
  mutate(
    prop_weight = WGT_FULL / sum(WGT_FULL)
  ) %>%
  as_survey_rep(
    variables = Bmercury_CU,
    weights = prop_weight,
    repweights = starts_with("bsw"),
    type = "BRR",
    mse = TRUE
  )

# Set degrees of freedom to 11 (16 sites - 5 Regions)
survey_design$degf <- 11

# Render weighted histogram
ggplot(
  data = survey_design$variables %>% na.omit(),
  mapping = aes(
    x = Bmercury_CU
  )
) +
  geom_histogram(
    aes(
      y = after_stat(count / sum(count)),
    ),
    binwidth = 0.5,
    fill = "#6F7EB3",
    color = "black"
  ) +
  scale_x_continuous(
    breaks = seq(from = 0, to = 20, by = 1)
  ) +
  labs(
    title = "Histogram of blood total mercury concentrations (synthetic data)",
    x = "Blood total mercury in ug/L - < LOD has been imputed - conventional units",
    y = "Relative weight sum"
  ) +
  theme_minimal()

# Step 5b: estimate geometric mean and confidence intervals

# Geometric mean

# Specify survey design
survey_design <- c3full %>%
  as_survey_rep(
    variables = c(
      "Bmercury_CU",
      "Bmercury_CU_log",
      "agegrp"
    ),
    weights = WGT_FULL,
    repweights = starts_with("bsw"),
    type = "BRR",
    mse = TRUE
  )

# Set degrees of freedom to 11 (16 sites - 5 Regions)
survey_design$degf <- 11

# Compute survey statistics
survey_stats <- svymean(
  x = ~ Bmercury_CU_log,
  design = survey_design,
  na.rm = TRUE
)

# Create a table
tab <- tibble(
  Variable = "Bmercury_CU",
  Label = "Blood total mercury in ug/L - < LOD has been imputed - Conventional units"
) %>%
  bind_cols(
    as_tibble(exp(survey_stats)),
    as_tibble(exp(confint(survey_stats)))
  ) %>%
  rename(
    `Geometric mean` = mean,
    `Std error` = SE,
    `95% CI (lower)` = `2.5 %`,
    `95% CI (upper)` = `97.5 %`
  ) %>%
  mutate(
    across(
      .cols = c(
        `Geometric mean`,
        `Std error`,
        `95% CI (lower)`,
        `95% CI (upper)`
      ),
      .fns = ~ round(.x, 4)
    )
  )

# Render table
kbl(
  x = tab,
  caption = paste0(
    "Geometric means",
    " (",
    "complete cases: ",
    sum(complete.cases(survey_design$variables)),
    "; incomplete cases: ",
    sum(! complete.cases(survey_design$variables)),
    ")"
  )
) %>%
  kable_styling(
    bootstrap_options = c(
      "striped",
      "hover",
      "condensed"
    )
  )

# Domain analysis

# Compute survey statistics
hgage <- svyby(
  formula = ~ Bmercury_CU + Bmercury_CU_log,
  by = ~ agegrp,
  design = survey_design,
  FUN = svymean,
  vartype = c("se","cv"),
  keep.var = TRUE,
  na.rm = TRUE
)

# Build table of statistics
tab <- tibble(
  agegrp = hgage$agegrp,
  Variable = "Bmercury_CU",
  Label = "Blood total mercury in ug/L - < LOD has been imputed - Conventional units",
  N = survey_design$variables %>%
    na.omit %>%
    select(agegrp) %>%
    table %>%
    unlist %>%
    unname %>%
    format(
      x = .,
      big.mark = ","
    ),
  Mean = format(
    x = round(
      x = hgage$Bmercury_CU,
      digits = 4
    ),
    nsmall = 4
  ),
  `Std error of mean` = format(
    x = round(
      x = hgage$se1,
      digits = 4
    ),
    nsmall = 4
  ),
  `95% CI (lower)` = format(
    x = round(
      x = confint(hgage)[str_detect(row.names(confint(hgage)), "\\bBmercury_CU\\b"), 1] %>%
        unname,
      digits = 4
    ),
    nsmall = 4
  ),
  `95% CI (upper)` = format(
    x = round(
      x = confint(hgage)[str_detect(row.names(confint(hgage)), "\\bBmercury_CU\\b"), 2] %>%
        unname,
      digits = 4
    ),
    nsmall = 4
  ),
  `Coefficient of variation` = format(
    x = round(
      x = hgage$cv.Bmercury_CU,
      digits = 4
    ),
    nsmall = 4
  )
)

# Render table
kbl(
  x = tab,
  align = c(
    "c",
    rep("l", 2),
    rep("r", 6)
  ),
  caption = "Domain analysis: age group at clinic visit"
) %>%
  kable_styling(
    bootstrap_options = c(
      "striped",
      "hover",
      "condensed"
    ),
    font_size = 9
  )

# Step 5c: testing differences between geometric means

# Tests whether the geometric mean concentration of total blood mercury for 3- to 5-year-olds is statistically different from each of the older age groups.

# Generalized linear model

# Re-level the agegrp factor
survey_design$variables$agegrp <- relevel(
  x = survey_design$variables$agegrp,
  ref = 1
)

# Fit a survey-weighted generalized linear model
hgage <- svyglm(
  formula = Bmercury_CU_log ~ agegrp,
  design = na.omit(survey_design) %>%
    na.omit
)

# Render model summary
tab_model(
  model = hgage,
  show.df = TRUE,
  show.se = TRUE,
  show.stat = TRUE,
  show.aic = TRUE,
  show.obs = TRUE,
  digits = 4,
  digits.p = 4,
  pred.labels = c(
    "(Intercept)",
    paste0(
      "Age group ",
      2:6,
      " (ref: age group 1)"
    )
  ),
  dv.labels = Reduce(paste, deparse(hgage$formula)),
  string.est = "Estimate",
  string.se = "SE",
  string.df = "Residual df",
  string.stat = "t value",
  col.order = c(
    "est",
    "se",
    "ci",
    "df.error",
    "stat",
    "p"
  )
)

# Wald test

# Render Wald test statistics
get_wald_test(
  model = hgage,
  test_terms = ~ agegrp,
  df = 11
)

# Multiple test correction

# False discovery rate, the expected proportion of false discoveries among the rejected hypotheses.
# The false discovery rate is a less stringent condition than the family-wise error rate, so these methods are more powerful than the others.

# Create a vector of coefficients
coefficients <- as_tibble(coef(summary(hgage))) %>%
  select(`Pr(>|t|)`) %>%
  unlist

# Add names to coefficients
names(coefficients) <- names(hgage$coefficients)

# Render adjusted p-values
kbl(
  x = p.adjust(
    p = coefficients[-1],
    method = "fdr"
  ) %>%
    as.data.frame %>%
    rownames_to_column(., "agegrp") %>%
    rename(`Adjusted p value` = 2),
  caption = "Multiple test correction"
) %>%
  kable_styling(
    bootstrap_options = c(
      "striped",
      "hover",
      "condensed"
    )
  )

# Step 6: percentiles

# Estimates the percentile distribution of blood total mercury across age groups.

# Compute percentiles
tab <- bind_rows(
  lapply(
    X = levels(survey_design$variables$agegrp),
    FUN = function(x) {
      get_quantiles(
        survey_design = survey_design,
        formula = ~ Bmercury_CU,
        label = "Blood total mercury in ug/L - < LOD has been imputed - Conventional units",
        filter_variable = "agegrp",
        filter_value = x,
        quantiles = c(0.25, 0.5, 0.75, 0.95)
      )
    }
  )
)

# Identify select element values to change
elements <- -c(
  seq(
    from = 1,
    to = nrow(tab),
    by = nrow(tab) / length(unique(tab$agegrp))
  )
)

# Change select element values to empty values
tab$Variable[elements] <- ""
tab$Label[elements] <- ""

# Render percentiles
kbl(
  x = tab %>%
    select(-agegrp),
  align = c(
    rep("l", 2),
    rep("r", 6)
  ),
  caption = "Quantiles for Bmercury_CU ~ agegrp"
) %>%
  pack_rows(
    index =
      table(
        paste0(
          "agegrp = ",
          tab$agegrp
        )
      )
  ) %>%
  kable_styling(
    bootstrap_options = c(
      "striped",
      "hover"
    )
  )

# Step 7
# Step 7a: logistic regression

# Design-based regression inference is estimating a population summary statistic, not a model parameter. Generalizing to other populations relies on assumptions about the stability of the data-generating process. Depending on the application, more assumptions may be required:

# Out-of-sample prediction requires accurate approximation to conditional distribution of Y|X.
# For large surveys, bias is likely more important than variance, so ignoring weights is risky. But for smaller surveys, ignoring weights may improve mean squared error noticeably.

# Response profile

# Specify survey design
survey_design <- c3full %>%
  as_survey_rep(
    variables = c(
      "highBPreg",
      "sex",
      "age",
      "BMIcat3",
      "agegrp"
    ),
    weights = WGT_FULL,
    repweights = starts_with("bsw"),
    type = "BRR"
  )

# Set degrees of freedom to 11 (16 sites - 5 Regions)
survey_design$degf <- 11

# Build table
tab <- tibble(
  "Ordered value" = 1:length(levels(survey_design$variables$highBPreg)),
  highBPreg = levels(survey_design$variables$highBPreg),
  "Total frequency" = survey_design$variables %>%
    na.omit %>%
    select(highBPreg) %>%
    table %>%
    unlist %>%
    unname,
  "Total weight" = svytotal(
    x = ~ highBPreg,
    design = survey_design,
    na.rm = TRUE
  ) %>% as.data.frame %>%
    select(total) %>%
    unlist %>%
    unname
) %>%
  mutate(
    `Total frequency` = format(
      x = `Total frequency`,
      big.mark = ","
    ),
    `Total weight` = format(
      x = `Total weight`,
      big.mark = ","
    )
  )

# Render table
kbl(
  x = tab,
  align = c(
    rep("c", 2),
    rep("r", 2)
  ),
  caption = "Response profile"
) %>%
  kable_styling(
    bootstrap_options = c(
      "striped",
      "hover",
      "condensed"
    )
  )

# Type 3 analysis of effects

# Fit a survey-weighted generalized linear model
HBPmodel <- svyglm(
  formula = highBPreg ~ BMIcat3 + sex + age,
  design = subset(
    x = na.omit(survey_design),
    subset = as.numeric(agegrp) > 3
  ),
  family = quasibinomial
)

# Build ANOVA table
tab <- anova(
  object = HBPmodel,
  test = "Chisq",
  method = "Wald"
)

# Iterate the table and pull out select statistics
tab <- tibble(
  Effects = unlist(
    lapply(
      X = tab,
      FUN = function(x) {
        
        x$test.terms
        
      }
    )
  ),
  "F value" = unlist(
    lapply(
      X = tab,
      FUN = function(x) {
        
        format(
          x = round(
            x = x$chisq,
            digits = 2
          ),
          nsmall = 2
        )
        
      }
    )
  ),
  "Num DF" = unlist(
    lapply(
      X = tab,
      FUN = function(x) {
        
        x$df
        
      }
    )
  ),
  "Den DF" = survey_design$degf,
  "Pr > F" = unlist(
    lapply(
      X = tab,
      FUN = function(x) {
        
        ifelse(
          test = x$p < 0.0001,
          yes = "< 0.0001",
          no = format(
            x = round(
              x = x$p,
              digits = 4
            ),
            nsmall = 4
          )
        )
        
      }
    )
  )
)

# Render the table
kbl(
  x = tab,
  caption = "Type 3 Analysis of Effects",
  align = c(
    "l",
    rep("r", ncol(tab) - 1)
  )
) %>%
  kable_styling(
    bootstrap_options = c(
      "striped",
      "hover",
      "condensed"
    )
  )

# Odds ratio estimates

# Render model summary stats
tab_model(
  model = HBPmodel,
  show.df = TRUE,
  show.se = TRUE,
  show.stat = TRUE,
  show.aic = TRUE,
  show.loglik = TRUE,
  show.dev = TRUE,
  show.obs = TRUE,
  digits = 4,
  digits.p = 4,
  pred.labels = c(
    "(Intercept)",
    "BMIcat3 (level 2 vs. 1)",
    "BMIcat3 (level 3 vs. 1)",
    "sex (level 2 vs. 1)",
    "age"
  ),
  dv.labels = Reduce(paste, deparse(HBPmodel$formula)),
  string.est = "Estimate",
  string.se = "SE",
  string.df = "Residual df",
  string.stat = "t value",
  col.order = c(
    "est",
    "se",
    "ci",
    "df.error",
    "stat",
    "p"
  )
)

# Association of predicted probabilities and observed responses

# Render table
concordance(HBPmodel)

# Step 7b: linear regression

# Estimated regression coefficients

# Make sure it is not an ordered factor or gml() otherwise R will attempt lqc fitting
c3full$sleepprob <- factor(
  x = c3full$sleepprob,
  levels = 4:1,
  ordered = FALSE
)

c3full$sex <- factor(
  x = c3full$sex,
  levels = 2:1,
  ordered = FALSE
)

# Specify survey design
survey_design <- c3full %>%
  as_survey_rep(
    variables = c(
      "BMI",
      "sex",
      "age",
      "agegrp",
      "sleepprob"
    ),
    weights = WGT_FULL,
    repweights = starts_with("bsw"),
    type = "BRR"
  )

# Set degrees of freedom to 11 (16 sites - 5 Regions)
survey_design$degf <- 11

# We are only looking at adults
mysubset <- survey_design %>%
  filter(agegrp %in% 4:6)

# We should verify first that BMI is normally distributed. But that is not necessarily true for the synthetic data

# Fit a survey-weighted generalized linear model
BMImodel <- svyglm(
  formula = BMI ~ age + sex + sleepprob,
  design = na.omit(mysubset),
  family = gaussian
)

# Render model summary stats
tab_model(
  model = BMImodel,
  show.df = TRUE,
  show.se = TRUE,
  show.stat = TRUE,
  show.aic = TRUE,
  show.loglik = TRUE,
  show.dev = TRUE,
  show.obs = TRUE,
  digits = 4,
  digits.p = 4,
  pred.labels = c(
    "(Intercept)",
    "age",
    "sex (1 vs. 2)",
    paste0(
      "sleepprob (",
      3:1,
      " vs. 4)"
    )
  ),
  dv.labels = Reduce(paste, deparse(BMImodel$formula)),
  string.est = "Estimate",
  string.se = "SE",
  string.df = "Residual df",
  string.stat = "t value",
  col.order = c(
    "est",
    "se",
    "ci",
    "df.error",
    "stat",
    "p"
  )
)

# Tests of model effects

# BMI ~ age + sex + sleepprob

# Render Wald test statistics
get_wald_test(
  model = BMImodel,
  test_terms = BMI ~ age + sex + sleepprob,
  df = 11
)

# BMI ~ age

# Render Wald test statistics
get_wald_test(
  model = BMImodel,
  test_terms = BMI ~ age,
  df = 11
)

# BMI ~ sex

# Render Wald test statistics
get_wald_test(
  model = BMImodel,
  test_terms = BMI ~ sex,
  df = 11
)

# BMI ~ sleepprob

# Render Wald test statistics
get_wald_test(
  model = BMImodel,
  test_terms = BMI ~ sleepprob,
  df = 11
)