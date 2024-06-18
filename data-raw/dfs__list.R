## code to prepare 'df'

df <- readxl::read_xlsx("data-raw/metadata_cart_cleaned.xlsx")
df <- df[1:10, 1:20]
usethis::use_data(df, overwrite = TRUE)

## same columns but different_rows
df_row <- df[1:5, ]
usethis::use_data(df_row, overwrite = TRUE)


## f transformed in list with added a list inside of it.
cancer_list <- as.list(df)
cancer_list[[1]] <- list(id = paste0("patient", 1:20), code = sample(letters, 20, replace = F))
usethis::use_data(cancer_list, overwrite = T)
