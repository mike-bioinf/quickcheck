### test if the picking evaluation frame mechanism for the alert message is working

cli::test_that_cli(desc = "Testing the functionality of the selection evalution frame mechanism",{
  expect_snapshot(
    {
      check_key(
        df = df,
        key = "sex",
        raise = "message",
        alert_message = "{vec_arg} glue substitution is evaluated correctly as sex"
      )
    }
  )},
  configs = "ansi"
)
