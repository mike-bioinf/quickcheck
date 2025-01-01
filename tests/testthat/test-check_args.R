### Testing check_args functions


test_that("Incorrect input arguments are signaled by check_arg_funcs", {
  f <- function(x = NULL, y = NULL) check_args_incompatible(args = c("a", "y"))
  expect_snapshot_error(f())
})



test_that("check_args_incompatible works as expected", {
  f <- function(x = NULL, y = NULL) check_args_incompatible(args = c("x", "y"))
  expect_snapshot_error(f(1, 2))
})


test_that("check_args raise errors for incorrect inputs", {
  f <- function(x, y) check_args(args = c("x", "y"), expected_types = "numeric", recycle_types = 2, with = "cla")
  expect_error(f(2, 2), regexp = "`with` must be one of")
  f <- function(x, y) check_args(args = c("x", "y"), expected_types = "numeric", with = "class")
  expect_error(f("2", "2"),regexp = "args and expected_types have different lengths.")
  f <- function(x, y) check_args(args = c("x", "y"), expected_types = "numeric", with = "class", null = c(T, F, T))
  expect_error(f(1, 1), regexp = "null of length 2 expected, 3 detected.")
  f <- function(x, y) check_args(args = c("x", "y"), expected_types = "numeric", with = "class", flag = c(T, F, T))
  expect_error(f(1, 1), regexp = "flag of length 2 expected, 3 detected.")
})



test_that("check_args works as exepcted", {
  obj <- 1
  class(obj) <- c("custom_class", "numeric")
  obj2 <- 2
  class(obj2) <- c("c1", "c2", "numeric")

  f <- function(x, y, z = 1, p = obj){
    check_args(
      args = c("x", "y", "z", "p"),
      expected_types = c("character", "numeric", "integerish", "custom_class"),
      null = TRUE,
      flag = TRUE,
      with = c("class", "class", "check_integerish", "class")
    )
  }

  expect_no_error(f(x = "home", y = 2))
  expect_no_error(f(x = NULL, y = 1))
  expect_no_error(f(x = NULL, y = NULL, z = 2))
  expect_snapshot_error(f(x = NULL, y = NULL, z = 2.1))
  expect_snapshot_error(f(x = 1, y = "string"))
  expect_snapshot_error(f(x = c("s1", "s2"), y = 1))
  expect_snapshot_error(f(x = NULL, y = NULL, z = NULL, p = 1))
  expect_snapshot_error(f(x = NULL, y = NULL, z = NULL, p = obj2))
})



test_that("check_args evalution frame mechanisms works as expected", {
  what <- "args"
  f <- function(x, y){
    check_args(
      args = c("x", "y"),
      expected_types = c("character", "numeric"),
      with = "class",
      alert_message = "Errors in inputs {what}",
      n_evaluation_frame = 1,
      header = NULL
    )
  }
  expect_error(f(x = 1, y = 1), regexp = "Errors in inputs args")
})



### DEPRECATED ARGS FUNCS
test_that("null argument of check_args_* funcs works", {
  custom_func <- function(num = NULL, char = NULL){
    check_args_classes("char", "character", null = TRUE)
    check_args_primitive_types("char", "character", null = TRUE)
    check_numeric_args("num", null = TRUE)
    invisible(NULL)
  }
  expect_no_error(object = custom_func(num = 1, char = "abc"))
})

