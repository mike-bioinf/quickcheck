#' Make test function from check_function
#' @param check_fun String reporting the name of the check function to convert.
#' @param env Environment set in the function object (for scoping purposes).
#'  Default the caller env that is the package NAMESPACE at building time.
#' @return The test function.
make_test_function <- function(check_fun, env = parent.frame()){
  test_fun <- function() NULL
  params <- formals(check_fun)
  formals(test_fun) <- params
  collapsed_params <- paste0(names(params), collapse = ", ")
  fun_body <- paste0("impose_logical_behavior", "(", check_fun, "(", collapsed_params, "))")
  body(test_fun) <- parse(text = paste0("{ ", fun_body, " }"))
  environment(test_fun) <- env
  return(test_fun)
}



### Single vector --------------------------------------------------------------------------------------

#' @rdname check_vector
#' @export
test_vector <- make_test_function("check_vector")


#' @rdname check_names
#' @export
test_names <- make_test_function("check_names")


#' @rdname check_atomic_vec
#' @export
test_atomic_vec <- make_test_function("check_atomic_vec")


#' @rdname check_empty_vec
#' @export
test_empty_vec <- make_test_function("check_empty_vec")


#' @rdname check_na_vec
#' @export
test_na_vec <- make_test_function("check_na_vec")


#' @rdname check_duplicate_vec
#' @export
test_duplicate_vec <- make_test_function("check_duplicate_vec")


#' @rdname check_length_vec
#' @export
test_length_vec <- make_test_function("check_length_vec")


#' @rdname check_presence_vec
#' @export
test_presence_vec <- make_test_function("check_presence_vec")


#' @rdname check_sorted_vec
#' @export
test_sorted_vec <- make_test_function("check_sorted_vec")


#' @rdname check_predicate_vec
#' @export
test_predicate_vec <- make_test_function("check_predicate_vec")


#' @rdname check_absence_vec
#' @export
test_absence_vec <- make_test_function("check_absence_vec")



### Single dataframe ------------------------------------------------------------------------

#' @rdname check_columns_presence
#' @export
test_columns_presence <- make_test_function("check_columns_presence")


#' @rdname check_columns_key
#' @export
test_columns_key <- make_test_function("check_columns_key")


#' @rdname check_columns_levels
#' @export
test_columns_levels <- make_test_function("check_columns_levels")


#' @rdname check_columns_na
#' @export
test_columns_na <- make_test_function("check_columns_na")


#' @rdname check_columns_predicate
#' @export
test_columns_predicate <- make_test_function("check_columns_predicate")


#' @rdname check_columns_number
#' @export
test_columns_number <- make_test_function("check_columns_number")


#' @rdname check_empty_df
#' @export
test_empty_df <- make_test_function("check_empty_df")



### Single list ---------------------------------------------------------------------

#' @rdname check_list
#' @export
test_list <- make_test_function("check_list")


#' @rdname check_empty_list
#' @export
test_empty_list <- make_test_function("check_empty_list")


#' @rdname check_uniform_list
#' @export
test_uniform_list <- make_test_function("check_uniform_list")

#' @rdname check_predicate_list
#' @export
test_predicate_list <- make_test_function("check_predicate_list")


#' @rdname check_length_list
#' @export
test_length_list <- make_test_function("check_length_list")



### Double vectors -------------------------------------------------------------------

#' @rdname check_length_vecs
#' @export
test_lenght_vecs <- make_test_function("check_length_vecs")


#' @rdname check_identical_vecs
#' @export
test_identical_vecs <- make_test_function("check_identical_vecs")


#' @rdname check_equality_vecs
#' @export
test_equality_vecs <- make_test_function("check_equality_vecs")


#' @rdname check_matching_vecs
#' @export
test_matching_vecs <- make_test_function("check_matching_vecs")



### Double Dataframes ------------------------------------------------------------------

#' @rdname check_nrow_dfs
#' @export
test_nrow_dfs <- make_test_function("check_nrow_dfs")


#' @rdname check_columns_copresence
#' @export
test_columns_copresence <- make_test_function("check_columns_copresence")


#' @rdname check_presence_dfs
#' @export
test_presence_dfs <- make_test_function("check_presence_dfs")
