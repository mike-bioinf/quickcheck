# impose_accumulation_behavior accumulate quickalert

    x this is the header.
    1: sex --> femmina
    2: visit_number --> visit1 and vis3

# impose_accumulation_behavior launch unexpected error alert

    x An unexpected alert is been raised:
    `type` must be one of "error", "warning", "message", or "accumulate_message", not "messagessss".

# impose_additional_alert works as intended

    Code
      impose_additional_alert(expr = check_columns_key(df, c("sex", "visit_number"),
      raise = "message"), message = c(i = "additional_part"), margin = 1, raise = "message")
    Message <quickalert>
      i additional_part
      i The following values occur multiple times for the following columns: 1: sex --> male 2: visit_number --> 1, 2, and 3

---

    Code
      impose_additional_alert(expr = check_columns_key(df, c("sex", "visit_number"),
      raise = "warning"), message = "additional_part", margin = 2, raise = "warning")
    Condition <quickalert>
      Warning:
      ! The following values occur multiple times for the following columns: 1: sex --> male 2: visit_number --> 1, 2, and 3
      additional_part

