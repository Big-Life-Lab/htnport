impute_variables <- function(
  imputation_dataset,
  outcomes,
  predictors
) {
  num_multiple_imputations <- 1
  
  # The prediction matrix to use for the mice::mice function call
  imp_pred_matrix <-
    create_mice_imp_pred_matrix(outcomes, predictors)
  
  # Run the imputation
  imputed_data <- mice::mice(
    prepare_imputation_dataset(imputation_dataset),
    m = num_multiple_imputations,
    maxit = 1,
    predictorMatrix = imp_pred_matrix,
    blocks = unique(c(predictors, outcomes)),
    nnet.MaxNWts = 4000
  )
  
  # Return the last imputed dataset
  return(mice::complete(imputed_data, num_multiple_imputations))
}

#' Constructs the matrix to use with the predictionMatrix parameter in the
#' mice::mice function
#'
#' @param outcome string The name of variable to be imputed
#' @param predictors vector Names of the variables to impute the outcome
#' parameter with
#'
#' @return
#' @export
#'
#' @examples
create_mice_imp_pred_matrix <- function(outcomes, predictors) {
  all_vars <- unique(c(predictors, outcomes))
  num_vars_in_col <- length(all_vars)
  num_vars_in_row <- length(all_vars)
  # The matrix properties are:
  # Only one row to denote the outcome variable
  # A number of columns equal to the predictor variables plus the outcome
  # Add ones for element in the matrix. We will set the element that corresponds
  # to the outcome column to 0 further ahead
  imp_pred_matrix <- matrix(
    # The number of elements are
    rep(1, num_vars_in_col * num_vars_in_row),
    byrow = TRUE,
    nrow = num_vars_in_row,
    ncol = num_vars_in_col
  )
  colnames(imp_pred_matrix) <- unique(c(predictors, outcomes))
  rownames(imp_pred_matrix) <- unique(c(predictors, outcomes))
  
  # Do this so we don't use the outcome to impute itself
  for(current_var in all_vars) {
    imp_pred_matrix[current_var, current_var] <- 0
  }
  
  return(imp_pred_matrix)
}

prepare_imputation_dataset <- function(imputation_dataset) {
  factor_variables <- colnames(
    imputation_dataset %>%
      dplyr::select(where(is.factor))
  )
  prepared_dataset <- imputation_dataset %>% 
    dplyr::mutate(dplyr::across(
      is.factor,
      ~ ifelse(.x %in% c("NA(a)", "NA(b)", "NA(c)"), NA, .x)
    )) %>%
    dplyr::mutate(dplyr::across(
      factor_variables,
      as.factor
    ))
  
  # For runs in the dev environment this makes sure that categorical
  # variables with one NA value have at least 10 values
  # Fixes an issue with running imputation with the MICE library where
  # an error, "dims does not match length of object" was occurring when
  # a categorical variable has only one NA value
  # DO NOT DO THIS IN THE PROD RUN
  # We do this in dev with the assumption that in the prod run with the whole
  # data there will be more NA values
  if(Sys.getenv("R_CONFIG_ACTIVE") == "dev" || 
     Sys.getenv("R_CONFIG_ACTIVE") == "") {
    for(column_name in colnames(prepared_dataset)) {
      if(is.factor(prepared_dataset[[column_name]]) & 
         sum(is_na(prepared_dataset[[column_name]])) == 1) {
        column <- prepared_dataset[[column_name]]
        column[sample(seq(1, nrow(prepared_dataset)), 10)] <- NA
        prepared_dataset[[column_name]] <- column
      }
    }
  }
  
  return(prepared_dataset)
}