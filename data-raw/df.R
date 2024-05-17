## code to prepare 'df'

df <- readxl::read_xlsx("data-raw/metadata_cart_cleaned.xlsx")
df <- df[1:10, 1:20]
usethis::use_data(df, overwrite = TRUE)

## same columns but different_rows
df_row <- df[1:5, ]
usethis::use_data(df_row, overwrite = TRUE)
