# impose_accumulation_behavior accumulate alerts

    x this is the header.
    1. sex --> femmina
    2. visit_number --> visit1 and vis3

# impose_accumulation_behavior launch unexpected error alert

    x An unexpected alert is been raised:
    `type` must be one of "error", "warning", or "message", not "messagessss".

# impose_accumulation_behavior launch one time the provided alert message

    Code
      check_columns_key(df = qadf, columns = c("sex", "visit_number"), raise = "warning",
      alert_message = "Just one line !!!", header = NULL)
    Condition
      Warning:
      ! Just one line !!!

# The header option works correctly with impose_accumulation behavior

    Code
      check_columns_key(df = qadf, columns = c("sex", "visit_number"), raise = "warning",
      header = "CUSTOM HEADER")
    Condition
      Warning:
      ! CUSTOM HEADER
      1. sex --> male
      2. visit_number --> 1, 2, and 3

