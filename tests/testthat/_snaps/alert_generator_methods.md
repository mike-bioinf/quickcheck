# List messages are formatted correctly [ansi]

    Code
      check_columns_levels(df, columns = c("visit_number", "ppi_treatment"),
      col_levels = list(visit_number = c("v1", "2", "3"), ppi_treatment = c("yes",
        "no_treatment")))
    Condition
      [1m[33mError[39m in `check_columns_levels()`:[22m
      [1m[22m[31mx[39m The following levels are missing:
      [35mvisit_number[39m: v1
      [35mppi_treatment[39m: yes

