#' Compute Median Prices per Administrative Level
#'
#' @param df Monthly JMMI clean dataset
#' @param admin_level_col a column name indicating a Libyan Administrative Level
#' @param item_group item_group
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
  if (!is.null(item_group)) {
    valid_groups <- dplyr::distinct(meb_weights, .data[["group"]]) |> dplyr::pull(.data[["group"]])
    assertthat::assert_that(item_group %in% valid_groups,
      msg = paste0(item_group, " is not in ", paste(valid_groups, collapse = ", "))
    )
    meb_weights <- meb_weights |> filter(.data[["group"]] == item_group)
  }

  df |>
    select({{ admin_level_col }}, pull(meb_weights, .data[["item"]])) |>
    pivot_longer(-{{ admin_level_col }}, names_to = "item", values_to = "price") |>
    group_by({{ admin_level_col }}, .data[["item"]]) |>
    dplyr::summarise(median_item_price = median(.data[["price"]], na.rm = TRUE)) |>
    dplyr::ungroup()
}
