# Testing the functionality of the selection evalution frame mechanism [plain]

    Code
      check_key(df = df, key = "sex", raise = "message", alert_message = "{key} glue substitution is evaluated correctly as sex",
        alert_eval_env = "current_frame")
    Message
      i sex glue substitution is evaluated correctly as sex

# Testing the functionality of the selection evalution frame mechanism [ansi]

    Code
      check_key(df = df, key = "sex", raise = "message", alert_message = "{key} glue substitution is evaluated correctly as sex",
        alert_eval_env = "current_frame")
    Message
      [1m[22m[36mi[39m sex glue substitution is evaluated correctly as sex

# Testing the functionality of the selection evalution frame mechanism [unicode]

    Code
      check_key(df = df, key = "sex", raise = "message", alert_message = "{key} glue substitution is evaluated correctly as sex",
        alert_eval_env = "current_frame")
    Message
      ℹ sex glue substitution is evaluated correctly as sex

# Testing the functionality of the selection evalution frame mechanism [fancy]

    Code
      check_key(df = df, key = "sex", raise = "message", alert_message = "{key} glue substitution is evaluated correctly as sex",
        alert_eval_env = "current_frame")
    Message
      [1m[22m[36mℹ[39m sex glue substitution is evaluated correctly as sex

