get_row_for_variable <- function(variable, variables_sheet) {
  return(
    variables_sheet[variables_sheet$variable == variable, ]
  )
}

is_continuous_variable <- function(variables_sheet_row) {
  return(variables_sheet_row[1, "variableType"] == "Continuous")
}

is_categorical_variable <- function(variables_sheet_row) {
  return(variables_sheet_row[1, "variableType"] == "Categorical")
}

is_value_na <- function(value) {
    return(value=='N/A')
}
