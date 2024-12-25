# List with the default alert messages of the checking functions.
# Legend:
# The "||" term denotes optional parts that are present when specific condition are met (example some arguments are TRUE).
# The "$$" term denotes variable and not "usable" pieces of alert.



default_alert_messages <- list(
  check_args_primitive_types = c(
    "The following {cli::qty(err_args)} argument{?s} {?is/are} of {cli::col_red('wrong type')}: ",
    "{cli::col_magenta(err_args)}"),
  check_args_incompatible =
    "Incompatible arguments detected: {incompatible_args}",
  check_args_classes = c(
    "The following {cli::qty(err_args)} argument{?s} {?doesn't/don't} have the {cli::col_red('expected class')}:",
    "{cli::col_magenta(err_args)}"),
  check_numeric_args = c(
    "The following {cli::qty(err_args)} argument{?s} {?is/are} {cli::col_red('not numeric')}:",
    "{cli::col_magenta(err_args)}."),
  check_integerish_args = c(
    "The following {cli::qty(err_args)} argument{?s} {?is/are} {cli::col_red('not integer-like')}:",
    "{cli::col_magenta(names(err_args))}."),
  check_empty_vec =
    "{vec_arg} is or contains an empty entity.",
  check_na_vec =
    "There are NAs in {vec_arg}.",
  check_duplicate_vec = c(
    "The following {cli::qty(length(dup_values))} value{?s} {?is/are} duplicated in {vec_arg}",
    "{cli::col_magenta(dup_values)}"),
  check_length_vec = c(
    "{vec_arg} of length {exact_len} expected, {length(vec)} detected.",
    "{vec_arg} of minimum length {min_len} expected, {length(vec)} detected.",
    "{vec_arg} of maximum length {max_len} expected, {length(vec)} detected."
    ),
  check_presence_vec = c(
    "The following {cli::qty(length(missing_values))} value{?s} {?is/are} missing in {vec_arg}:",
    "{cli::col_magenta(missing_values)}"),
  check_sorted_vec =
    "{vec_arg} is not sorted.",
  check_predicate_vec = c(
    version1 = c(
      "{cli::col_red('Not all elements')} |NOT| satisfy the predicate in {vec_arg}.",
      "{cli::col_blue('Set or fill')} the missing element names for a more informative alert."
      ),
    version2 = c(
      "The following {cli::qty(length(errors))} element{?s} {?doesn't/don't} satisfy the |inverse of the| predicate in {vec_arg}:",
      "{cli::col_magenta(errors)}"
      )
    ),
  check_length_vecs =
    "{vec1_arg} and {vec2_arg} have {cli::col_red('different length')}",
  check_identical_vecs =
    "{vec1_arg} and {vec2_arg} are {cli::col_red('not identical')}.",
  check_equality_vecs =
    "{vec1_arg} and {vec2_arg} {cli::col_red('are not equal')}",
  check_matching_vecs = c(
    "{cli::col_red('Detected differences')}",
    "The following {cli::qty(length(missing12))} value{?s} {?is/are} present in {vec1_arg} but missing in {vec2_arg}:",
    "{unique(missing12)}",
    "The following {cli::qty(length(missing21))} value{?s} {?is/are} present in {vec2_arg} but missing in {vec1_arg}:",
    "{unique(missing21)}"),
  check_columns_presence = c(
    "The following {cli::qty(length(missing_values))} column{?s} {?is/are} {col_red('missing')} in {vec_arg}:",
    "{cli::col_magenta(missing_values)}"),
  check_columns_key = c(
    "The following values occur {cli::col_red('multiple times')} for the following columns:",
    "$col$ --> {cli::col_magenta(dup_values)}"),
  check_columns_levels = c(
    "The following levels are {cli::col_red('missing')} from the reported columns:",
    " $col$ --> {cli::col_magenta(missing_values)}"),
  check_columns_na = c(
    "The following columns {cli::col_red('present NAs')}:",
    "$cols$"),
  check_columns_predicate = c(
    "The |inverse of the| predicate function {cli::col_red('returned FALSE')} for the following {cli::qty(length(false_cols))} column{?s}:",
    "{cli::col_magenta(false_cols)}"),
  check_columns_number = c(
    "{df_arg} of length {exact_len} expected, {length(df)} detected.",
    "{df_arg} of minimum length {min_len} expected, {length(df)} detected.",
    "{df_arg} of maximum length {max_len} expected, {length(df)} detected."
    ),
  check_empty_df =
    "{df_arg} is empty.",
  check_nrow_dfs =
    "{cli::col_red('Different number')} of rows between {df1_arg} and {df2_arg}",
  check_columns_copresence = c(
    "The following columns are present in both dataframes:",
    "{cli::col_red(duplicated_cols)}."
    ),
  check_presence_dfs = c(
    "The following {cli::qty(length(number_missing2))} value{?s} {?is/are} {cli::col_red('missing')} in {df2_arg}:",
    "{missing_values2}",
    "The following {cli::qty(length(number_missing1))} value{?s} {?is/are} {cli::col_red('missing')} in {df1_arg}:",
    "{missing_values1}"
    ),
  check_uniform_list =
    "{xarg} is {cli::col_red('not uniform')}.",
  check_predicate_list = list(
    version1 = c(
      "{cli::col_red('Not all elements')} |NOT| satisfy the predicate in {xarg}.",
      "{cli::col_blue('Set or fill')} the missing element names for a more informative alert."
      ),
    version2 = c(
      "The following {cli::qty(length(errors))} element{?s} {?doesn't/don't} satisfy the |inverse of the| predicate in {xarg}:",
      "{cli::col_magenta(errors)}"
      )
    ),
  check_names_list =
    "{xarg} present {cli::col_red('missing element names')}.",
  check_length_list = c(
    "{xarg} of length {exact_len} expected, {length(x)} detected.",
    "{xarg} of minimum length {min_len} expected, {length(x)} detected.",
    "{xarg} of maximum length {max_len} expected, {length(x)} detected."
  )
)


usethis::use_data(default_alert_messages, overwrite = TRUE)
