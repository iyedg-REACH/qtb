#' Compute the Cost of the MEB in Libya
#'
#' @param df Monthly JMMI clean dataset
#' @param admin_level_col a column name indicating a Libyan Administrative Level
#' @param item_group Optional filter to limit the computation to a specific item group in the Minimum Expenditure Basket
#'
#' @return A dataframe of the cost of MEB per Administrative Level
#' @export
#'
#' @examples
#' compute_meb_cost(jmmi_2022_feb, q_region, "food")
#' compute_meb_cost(jmmi_2022_feb, q_district, "hygiene")
#' compute_meb_cost(jmmi_2022_feb, q_municipality)
compute_meb_cost <- function(df, admin_level_col, item_group = NULL) {
  medians_df <- admin_level_medians(
    df,
    admin_level_col = {{ admin_level_col }},
    item_group = item_group
  )

  weighted_df <- medians_df |>
    dplyr::inner_join(meb_weights, by = "item") |>
    mutate(
      weighted_median_item_price = .data[["median_item_price"]] * .data[["weight"]]
    )

  weighted_df |>
    filter(stringr::str_detect(item, "q_fuel_", negate = TRUE)) |>
    dplyr::group_by({{ admin_level_col }}) |>
    dplyr::summarise(meb_cost = sum(.data[["weighted_median_item_price"]], na.rm = TRUE)) |>
    dplyr::ungroup()
}
