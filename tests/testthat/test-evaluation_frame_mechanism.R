### test if the picking evaluation frame mechanism for the alert message is working.

cli::test_that_cli(desc = "Testing the functionality of the selection evalution frame mechanism",{
  expect_snapshot(
    {
      check_presence_values(
        vec = df[["sex"]],
        values = c("maschio"),
        raise = "message",
        alert_message = "{missing_values} glue substitution is evaluated correctly as maschio",
        header = NULL
      )
    }
  )},
  configs = "ansi"
)
