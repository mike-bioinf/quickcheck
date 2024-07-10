# impose_accumulation_behavior accumulate alerts

    x this is the header.
    1. sex --> femmina
    2. visit_number --> visit1 and vis3

# impose_accumulation_behavior launch unexpected error alert

    x An unexpected alert is been raised:
    `type` must be one of "error", "warning", "message", or "accumulate_message", not "messagessss".

# impose_accumulation_behavior launch one time the provided alert message

    Code
      check_columns_key(df, c("sex", "visit_number"), raise = "warning",
      alert_message = "Just one line !!!")
    Condition
      Warning:
      ! Just one line !!!

