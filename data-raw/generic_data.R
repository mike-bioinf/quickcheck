## code to prepare 'generic_df'

generic_df <- readxl::read_xlsx("data-raw/metadata_cart_cleaned.xlsx")
generic_df <- generic_data[1:10, 1:20]
usethis::use_data(generic_df, overwrite = TRUE)
