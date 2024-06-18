# format_cli_list format list in character vector messages as expected [ansi]

    Code
      l <- list("this is the error header", fruits = c("banana", "apple"),
      "second element without name")
      mess <- format_cli_list(l, header_sign = "x")
      alert_generator.character(type = "error", alert_message = mess, eval_env = 1)
    Condition
      [1m[33mError[39m in `alert_generator.character()`:[22m
      [1m[22m[31mx[39m this is the error header
      [35mfruits[39m: banana, apple
      [35m2[39m: second element without name
