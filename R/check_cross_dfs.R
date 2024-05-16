# Checking functions that works on 2 dataframes.



#' Checks equal number of rows between two dataframes
#' @param df1 first dataframe.
#' @param df2 second dataframe.
#' @param df1_arg string specifying how to address df1 in the raised messages (default "df1").
#' @param df2_arg string specifying how to address df2 in the raised messages (default "df2")
#' @inheritParams check_columns_presence
#' @return NULL or side effects
#' @export
check_nrow <- function(df1, df2, df1_arg = "df1", df2_arg = "df2", raise = "error", alert_message = NULL){
  check_required_all()
  check_primitive_types(args = c("df1_arg", "df2_arg", "raise"), arg_types = c("character"), numeric_corrispondence = 3)
  if(nrow(df1) != nrow(df2)){
    if(is.null(alert_message)){
      alert_message <- c("{col_red('Different number')} of rows between {df1_arg} and {df2_arg}")
    }
    alert_generator(raise, alert_message)
  }
  return(NULL)
}

























#' The function initially checks for the presence of sampleid_col in both dfs raising
#' error (if missing in data) and warning (if missing in metadata), and then proceed to
#' reorder metadata rows if necessary according to the data ids.
#' @param data dataframe that set the order with a column that can act as key.
#' @param metadata dataframe that contain the same key and which order is corrected if necessary.
#' @param sampleid_col character reporting the key column name.
#' @param data_arg string used for data argument in alerts.
#' @param metadata_arg string used for metadata argument in alerts.
#'
#' @return The reordered metadata
reorder_ids_metadata <- function(data, metadata, sampleid_col, data_arg = "data", metadata_arg = "metadata"){
  check_required_all()
  check_columns_presence(df = data, columns = sampleid_col, df_arg = data_arg)
  check_key(df = data, key = sampleid_col)

  if(!any(sampleid_col %in% colnames(metadata))){
    cli_warn(c("!" = "{sampleid_col} not found in {metadata_arg}, no ordering check is done between {data_arg} and {metadata_arg}'s rows"))
  } else if(!all(data[[sampleid_col]] %in% metadata[[sampleid_col]])){
    cli_abort(c("x" = "{data_arg} and {metadata_arg} sample ids are different."))
  } else if(!all(data[[sampleid_col]] == metadata[[sampleid_col]])){
    metadata <- metadata[match(data[[sampleid_col]], metadata[[sampleid_col]]), ]
    cli_warn(c("!" = "{data_arg} and {metadata_arg} rows in different order based on {col_red(sampleid_col)}: {metadata_arg} rows are reordered"))
  }
  return(metadata)
}




