#' Get item columns from survey
#'
#' @param df
#' @param col_name
#'
#' @return
#' @export
get_item_columns <- function(df, col_name) {
  regex <- r"((?<=q_).*(?=_price))"
  item <- stringr::str_extract(col_name, regex)
  colnames_regex <- glue::glue(r"((^q_{item}.+$)|(sell_{item}$))")
  df_colnames <- names(df)
  matched_colnames <- df_colnames[grepl(colnames_regex, df_colnames)]
  dplyr::select(df, c("_uuid", dplyr::any_of(matched_colnames))) |>
    janitor::remove_empty("rows")
}
