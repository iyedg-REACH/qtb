#' Compute Median Prices per Administrative Level
#'
#' @param df Monthly JMMI clean dataset
#' @param admin_level_col a column name indicating a Libyan Administrative Level
#' @param item_group Optional filter to limit the computation to a specific item group in the Minimum Expenditure Basket
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
                                admin_level_col,
                                item_group = NULL) {
  # if (interactive()) {
  #   devtools::load_all()
  # }
  # df <- jmmi_2022_feb
  # admin_level_col <- rlang::sym("q_municipality")
  # item_group <- NULL

  if (!is.null(item_group)) {
    valid_groups <- dplyr::distinct(meb_weights, .data[["group"]]) |> dplyr::pull(.data[["group"]])
    assertthat::assert_that(item_group %in% valid_groups,
      msg = paste0(item_group, " is not in ", paste(valid_groups, collapse = ", "))
    )
    meb_weights <- meb_weights |> filter(.data[["group"]] == item_group)
  }

  medians_df <- df |>
    select({{ admin_level_col }}, pull(meb_weights, .data[["item"]])) |>
    pivot_longer(-{{ admin_level_col }}, names_to = "item", values_to = "price") |>
    filter(item != "cooking_fuel_price_per_11kg") |>
    group_by({{ admin_level_col }}, .data[["item"]]) |>
    dplyr::summarise(median_item_price = median(.data[["price"]], na.rm = TRUE)) |>
    dplyr::ungroup()

  if (item_group == "cooking_fuel" || is.null(item_group)) {
    medians_df <- medians_df |>
      pivot_wider(id_cols = {{ admin_level_col }}, names_from = item, values_from = median_item_price) |>
      dplyr::rowwise() |>
      dplyr::mutate(cooking_fuel_price_per_11kg = median(c(
        q_fuel_public_price_per_11kg,
        q_fuel_private_price_per_11kg
      ), na.rm = TRUE)) |>
      dplyr::ungroup() |>
      pivot_longer(-{{ admin_level_col }}, names_to = "item", values_to = "median_item_price")
  }
  return(medians_df)
}
