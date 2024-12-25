### Datasets and other structure description.

#' df
#' @format simply a generic dataset
"qadf"


#' df_row --> changed rows in respect to df.
#' @format has half rows but the same columns of df
"qadf_row"


#' cancer_list <- df transformed in list with added a list inside of it.
#' @format list version of df with a list nested into it.
"cancer_list"


#' default_alert_messages --> list with the default messages raised by the checking functions.
#' @format list of character vector or rarely list (for function that raise different version of checks)
"default_alert_messages"


#' Dataframe for check on double dataframes
#' @format Subset of df with only visit_number, sex and age columns.
"qadf1"


#' Dataframe for check on double dataframes
#' @format Subset of df with only visit_number, bmi and height columns.
"qadf2"
