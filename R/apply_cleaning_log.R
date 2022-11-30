#' Apply a cleaning log to a data frame
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param df
#' @param df_values_col
#' @param cleaning_log_df
#' @param cleaning_log_values_col
#' @param by
#' @param ...
#'
#' @return
#' @export
apply_cleaning_log <- function(df,
                               df_values_col,
                               cleaning_log_df,
                               cleaning_log_values_col,
                               by = NULL,
                               ...) {
  clean_subset <- df |>
    dplyr::inner_join(cleaning_log_df, by = by, ...) |>
    select(-{{ df_values_col }}) |>
    dplyr::rename(
      "{{df_values_col}}" := rlang::as_name(rlang::enquo(cleaning_log_values_col))
    ) |>
    select(dplyr::all_of(names(df)))

  untouched_subset <- df |>
    dplyr::anti_join(cleaning_log_df, by = by, ...)

  return(dplyr::bind_rows(clean_subset, untouched_subset))
}
