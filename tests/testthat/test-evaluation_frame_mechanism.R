### Test if the evaluation frame mechanism for the alert message is working.


cli::test_that_cli("Evalution frame mechanisms works as intended", configs = "ansi", code = {
  expect_snapshot(
    {
      check_presence_values(
        vec = df[["sex"]],
        values = c("maschio"),
        raise = "message",
        alert_message = "{missing_values} glue substitution is evaluated correctly as maschio.",
        header = NULL
      )
    }
  )}
)



cli::test_that_cli("Evaluation frame mechanisms works as intended (part2)", configs = "ansi", code = {
  frame <- "frame_up"
  expect_snapshot(
    {
      f <- function(){
        frame <- "frame_down"
        check_presence_values(
          vec = df$sex,
          values = "maschio",
          raise = "message",
          header = NULL,
          alert_message = "the glue expression is correctly evaluated in {frame}/frame_up.",
          n.evaluation_frame = 2
        )
      }
      f()
    }
  )}
)
