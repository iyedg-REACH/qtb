apply_cleaning_log <- function(df,
                               df_value_col,
                               cleaning_log_df,
                               cleaning_log_value_col,
                               by = NULL,
                               ...) {
  clean_subset <- df |>
    inner_join(cleaning_log_df, by = by, ...) |>
    select(-{{ df_value_col }}) |>
    dplyr::rename(
      "{{df_value_col}}" := rlang::as_name(rlang::enquo(cleaning_log_value_col))
    ) |>
    select(-"old.value")

  untouched_subset <- df |>
    dplyr::anti_join(cleaning_log_df, by = by, ...)

  return(dplyr::bind_rows(clean_subset, untouched_subset))
}
