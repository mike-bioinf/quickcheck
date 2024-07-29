### list with the default alert messages of the checking functions.


default_alert_messages <- list(
  check_args_primitive_types = c("The following {qty(err_args)} argument{?s} {?is/are} of {col_red('wrong type')}: ",
                                 "{col_magenta(err_args)}"),
  check_args_classes = c("The following {qty(err_args)} argument{?s} {?doesn't/don't} have the {col_red('expected class')}:",
                         "{col_magenta(err_args)}"),
  check_empty_vec = "There are empty values in {vec_arg}.",
  check_na_vec = "There are NAs in {vec_arg}.",
  check_duplicate_vec = c("The following {qty(length(dup_values))} value{?s} {?is/are} duplicated in {vec_arg}",
                          "{cli::col_magenta(dup_values)}"),
  check_number_values = "{expected_number_levels} level{?s} expected but {length(unique_levels)} detected.",
  check_presence_values = c("The following {qty(missing_values)} value{?s} {?is/are} missing in {vec_arg}:",
                            "{col_magenta(missing_values)}"),
  check_unique_values = c("The following {qty(err_value)} value{?s} {?is/are} present {col_red('multiple times')} in {vec_arg}: ",
                          "{col_magenta(err_value)}"),
  check_length_vecs = "{vec1_arg} and {vec2_arg} have {col_red('different length')}",
  check_equality_vecs = "{vec1_arg} and {vec2_arg} {col_red('are not equal')}",
  check_unordered_equality_vecs = c(
    "{col_red('Detected differences')}",
    "The following {qty(length(missing12))} value{?s} {?is/are} present in {vec1_arg} but missing in {vec2_arg}:",
    "{unique(missing12)}",
    "\n",
    "The following {qty(length(missing21))} value{?s} {?is/are} present in {vec2_arg} but missing in {vec1_arg}:",
    "{unique(missing21)}"
  ),
  check_columns_presence = c("The following {qty(missing_values)} column{?s} {?is/are} {col_red('missing')} in {vec_arg}:",
                               "{col_magenta(missing_values)}"),
  check_columns_key = c("The following values occur {col_red('multiple times')} for the following columns:",
                          "{vec_arg} --> {col_magenta(err_value)}"),
  check_columns_levels = c("The following levels are {col_red('missing')} from the reported columns:",
                             "{vec_arg} --> {col_magenta(missing_values)}"),
  check_columns_predicate = c("The predicate function {col_red('returned FALSE')} for the following {qty(false_cols)} column{?s}:",
                              "{col_magenta(false_cols)}"),
  check_columns_na = c("The following columns {cli::col_red('present NAs')}:", "{vec_arg}"),
  check_nrow_dfs = "{col_red('Different number')} of rows between {df1_arg} and {df2_arg}",
  check_uniform_list = "The list is {cli::col_red('not uniform')}.",
  check_types_list = list(
    version1 = c("{cli::col_red('Not all elements')} are of the expected type.",
                  "i" = "{cli::col_blue('Set or fill')} the missing list element names for a more informative alert."),
    version2 = c("The following {qty(errors)} element{?s} {?is/are} not of the expected type:", "{cli::col_magenta(errors)}")
  )
)

usethis::use_data(default_alert_messages, overwrite = TRUE)
