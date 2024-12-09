# Evalution frame mechanisms works as intended [ansi]

    Code
      check_presence_vec(vec = df[["sex"]], values = c("maschio"), raise = "message",
      alert_message = "{missing_values} glue substitution is evaluated correctly as maschio.",
      header = NULL)
    Message
      [1m[22m[36mi[39m maschio glue substitution is evaluated correctly as maschio.

# Evaluation frame mechanisms works as intended (part2) [ansi]

    Code
      f <- (function() {
        frame <- "frame_down"
        check_presence_vec(vec = df$sex, values = "maschio", raise = "message",
        header = NULL, alert_message = "the glue expression is correctly evaluated in {frame}/frame_up.",
        n_evaluation_frame = 2)
      })
      f()
    Message
      [1m[22m[36mi[39m the glue expression is correctly evaluated in frame_up/frame_up.

