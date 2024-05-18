### test if format_cli_list format list in chracter vector messages as expected

cli::test_that_cli(
  desc = "format_cli_list format list in character vector messages as expected",
  code = {expect_snapshot(
    x = {
      l <- list("this is the error header", fruits = c("banana", "apple"), "second element without name")
      mess <- format_cli_list(l, header_sign = "x")
      alert_generator.character(type = "error", alert_message = mess, eval_env = 1)
    },
    error = T
  )},
  configs = "ansi"
)
