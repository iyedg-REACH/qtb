#' Compute Median Prices per Administrative Level
#'
#' @param df Monthly JMMI clean dataset
#' @param admin_level_col a column name indicating a Libyan Administrative Level
#'
#' @return A dataframe with median prices for each item in the MEB per Administrative Level
#'
#' @export
#'
#' @examples
#' admin_level_medians(jmmi_2022_feb, q_region)
#' admin_level_medians(jmmi_2022_feb, q_district)
#' admin_level_medians(jmmi_2022_feb, q_municipality)
admin_level_medians <- function(df,
                                admin_level_col) {
  df |>
    select({{ admin_level_col }}, pull(meb_weights, .data[["item"]])) |>
    pivot_longer(-{{ admin_level_col }}, names_to = "item", values_to = "price") |>
    group_by({{ admin_level_col }}, .data[["item"]]) |>
    dplyr::summarise(median_item_price = median(.data[["price"]], na.rm = TRUE)) |>
    dplyr::ungroup()
}
