#' Compute the Cost of the MEB in Libya
#'
#' @param df Monthly JMMI clean dataset
#' @param admin_level an administrative level in Libya, can be one of "municipality", "district", "region", or "overall"
#'
#' @return A dataframe of the cost of MEB per Administrative Level
#' @export
#'
#' @examples
#' compute_meb_cost(jmmi_2022_feb, "region")
#' compute_meb_cost(jmmi_2022_feb, "district")
#' compute_meb_cost(jmmi_2022_feb, "municipality")
compute_meb_cost <- function(df, admin_level = "municipality") {
  admin_level_col <- switch(admin_level,
    "municipality" = rlang::sym("q_municipality"),
    "district" = rlang::sym("q_district"),
    "region" = rlang::sym("q_region")
  )

  medians_df <- admin_level_medians(
    df,
    admin_level
  )

  weighted_df <- medians_df |>
    inner_join(meb_weights, by = "item") |>
    mutate(
      weighted_median_item_price = .data[["median_item_price"]] * .data[["weight"]]
    )

  weighted_df |>
    filter(stringr::str_detect(item, "q_fuel_", negate = TRUE)) |>
    dplyr::group_by({{ admin_level_col }}, group) |>
    dplyr::summarise(meb_cost = sum(.data[["weighted_median_item_price"]], na.rm = TRUE)) |>
    dplyr::ungroup()
}
