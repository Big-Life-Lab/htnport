get_unique_rec_end_rows <- function(
  variable_details_sheet,
  for_variable,
  include_NA = FALSE
) {
  all_unique_rec_end_rows <- variable_details_sheet %>%
    dplyr::filter(variable == for_variable) %>%
    dplyr::distinct(recEnd, .keep_all = TRUE) %>%
    dplyr::filter(!grepl("Func::", recEnd))
  
  if(include_NA == TRUE) {
    return(all_unique_rec_end_rows)
  }
  
  return(
    all_unique_rec_end_rows %>%
      dplyr::filter(recEnd != "NA::a" & recEnd != "NA::b")
  )
}

is_categorical <- function(variable, variable_details_sheet) {
  return("cat" %in% variable_details_sheet[variable_details_sheet$variable == variable, ]$typeEnd)
}

get_variable_type <- function(variable, variable_details_sheet) {
  return(get_variable_rows(variable, variable_details_sheet)[1, "typeEnd"])
}

get_variable_rows <- function(variable, variable_details_sheet) {
  return(variable_details_sheet[variable_details_sheet$variable == variable, ])
}