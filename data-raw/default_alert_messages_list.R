### list with the default alert messages of the checking functions.


default_alert_messages <- list(
  check_args_primitive_types = c(
    "The following {qty(err_args)} argument{?s} {?is/are} of {col_red('wrong type')}: ",
    "{col_magenta(err_args)}"),
  check_args_classes = c(
    "The following {qty(err_args)} argument{?s} {?doesn't/don't} have the {col_red('expected class')}:",
    "{col_magenta(err_args)}"),
  check_numeric_args = c(
    "The following {qty(err_args)} argument{?s} {?is/are} {col_red('not numeric')}:",
    "{col_magenta(err_args)}."),
  check_integerish_args = c(
    "The following {qty(err_args)} argument{?s} {?is/are} {col_red('not integer-like')}:",
    "{col_magenta(names(err_args))}."),
  check_empty_vec =
    "There are empty values in {vec_arg}.",
  check_na_vec =
    "There are NAs in {vec_arg}.",
  check_duplicate_vec = c(
    "The following {qty(length(dup_values))} value{?s} {?is/are} duplicated in {vec_arg}",
    "{cli::col_magenta(dup_values)}"),
  check_length_vec = c(
    "Vector of length {exact_len} expected, {length(vec)} detected.",
    "Vector of minumum length {min_len} expected, {length(vec)} detected.",
    "Vector of maximimum length {max_len} expected, {length(vec)} detected."),
  check_presence_vec = c(
    "The following {qty(missing_values)} value{?s} {?is/are} missing in {vec_arg}:",
    "{col_magenta(missing_values)}"),
  check_sorted_vec =
    "{vec_arg} is not sorted.",
  check_length_vecs =
    "{vec1_arg} and {vec2_arg} have {col_red('different length')}",
  check_identical_vecs =
    "{vec1_arg} and {vec2_arg} are {cli::col_red('not identical')}.",
  check_equality_vecs =
    "{vec1_arg} and {vec2_arg} {col_red('are not equal')}",
  check_unordered_equality_vecs = c(
    "{col_red('Detected differences')}",
    "The following {qty(length(missing12))} value{?s} {?is/are} present in {vec1_arg} but missing in {vec2_arg}:",
    "{unique(missing12)}",
    "The following {qty(length(missing21))} value{?s} {?is/are} present in {vec2_arg} but missing in {vec1_arg}:",
    "{unique(missing21)}"),
  check_columns_presence = c(
    "The following {qty(missing_values)} column{?s} {?is/are} {col_red('missing')} in {vec_arg}:",
    "{col_magenta(missing_values)}"),
  check_columns_key = c(
    "The following values occur {col_red('multiple times')} for the following columns:",
    "{vec_arg} --> {col_magenta(dup_values)}"),
  check_columns_levels = c(
    "The following levels are {col_red('missing')} from the reported columns:",
    "{vec_arg} --> {col_magenta(missing_values)}"),
  check_columns_na = c(
    "The following columns {cli::col_red('present NAs')}:",
    "{vec_arg}"),
  check_columns_predicate = c(
    "The predicate function {col_red('returned FALSE')} for the following {qty(false_cols)} column{?s}:",
    "{col_magenta(false_cols)}"),
  check_empty_df =
    "{df_arg} is empty.",
  check_nrow_dfs =
    "{col_red('Different number')} of rows between {df1_arg} and {df2_arg}",
  check_uniform_list =
    "The list is {cli::col_red('not uniform')}.",
  check_predicate_list = list(
    version1 = c("{cli::col_red('Not all elements')} satisfy the predicate.",
                  "{cli::col_blue('Set or fill')} the missing list element names for a more informative alert."),
    version2 = c("The following {qty(errors)} element{?s} {?doesn't/don't} satisfy the predicate:", "{cli::col_magenta(errors)}")
  ),
  check_names_list =
    "The provided list present {cli::col_red('missing element names')}."
)

usethis::use_data(default_alert_messages, overwrite = TRUE)
