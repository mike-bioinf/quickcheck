### Testing null argument of checking class arguments functions


test_that("null argument of check_args_* funcs works", {
  custom_func <- function(num = NULL, char = NULL){
    check_args_classes("char", "character", null = TRUE)
    check_args_primitive_types("char", "character", null = TRUE)
    check_numeric_args("num", null = TRUE)
    invisible(NULL)
  }
  expect_no_error(object = custom_func(num = 1, char = "abc"))
})

