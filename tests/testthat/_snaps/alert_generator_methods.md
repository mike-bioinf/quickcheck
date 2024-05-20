# List messages are formatted correctly [ansi]

    Code
      check_columns_levels(df, columns = c("visit_number", "ppi_treatment"),
      col_levels = list(visit_number = c("v1", "2", "3"), ppi_treatment = c("yes",
        "no_treatment")))
    Condition
      [1m[33mError[39m in `impose_accumulation_behavior()`:[22m
      [1m[22m[31mx[39m The following levels are [31mmissing[39m from the reported columns:
      [35m1[39m: visit_number --> [35mv1[39m
      [35m2[39m: ppi_treatment --> [35myes[39m

