## code to prepare 'df'

qadf <- readxl::read_xlsx("data-raw/metadata_cart_cleaned.xlsx")
qadf <- qadf[1:10, 1:20]
usethis::use_data(qadf, overwrite = TRUE)

## same columns but different_rows
qadf_row <- qadf[1:5, ]
usethis::use_data(qadf_row, overwrite = TRUE)


## f transformed in list with added a list inside of it.
cancer_list <- as.list(qadf)
cancer_list[[1]] <- list(id = paste0("patient", 1:20), code = sample(letters, 20, replace = F))
usethis::use_data(cancer_list, overwrite = TRUE)


## 2 dfs for check on 2 dfs
qadf1 <- dplyr::select(qadf, visit_number, sex, age)
qadf2 <- dplyr::select(qadf, visit_number, bmi, height)
usethis::use_data(qadf1, overwrite = TRUE)
usethis::use_data(qadf2, overwrite = TRUE)
