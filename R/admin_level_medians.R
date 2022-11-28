#' Compute Median Prices per Administrative Level
#'
#' @param df Monthly JMMI clean dataset
#' @param admin_level an administrative level in Libya, can be one of "municipality", "district", "region", or "overall"
#'
#' @return A dataframe with median prices for each item in the MEB per Administrative Level
#'
#' @export
#'
#' @examples
#' admin_level_medians(jmmi_2022_feb, "region")
#' admin_level_medians(jmmi_2022_feb, "district")
#' admin_level_medians(jmmi_2022_feb, "municipality")
admin_level_medians <- function(df,
                                admin_level = "municipality") {
  admin_levels <- c("municipality", "district", "region", "overall")
  assertthat::assert_that(
    admin_level %in% admin_levels,
    msg = paste0("Invalid admin level, must be one of ", paste0(admin_levels, collapse = ", "))
  )

  admin_level_col <- switch(admin_level,
    "municipality" = rlang::sym("q_municipality"),
    "district" = rlang::sym("q_district"),
    "region" = rlang::sym("q_region")
  )

  ## The medians for municipalities and districts are computed based on
  ## the non-summarized data. The medians for regions are computed based on
  ## municipalities' medians

  if (admin_level %in% c("municipality", "district")) {
    medians_df <- df |>
      select({{ admin_level_col }}, pull(meb_weights, .data[["item"]])) |>
      pivot_longer(-{{ admin_level_col }}, names_to = "item", values_to = "price") |>
      group_by({{ admin_level_col }}, .data[["item"]]) |>
      summarise(median_item_price = median(.data[["price"]], na.rm = TRUE)) |>
      ungroup() |>
      pivot_wider(id_cols = {{ admin_level_col }}, names_from = "item", values_from = "median_item_price") |>
      dplyr::rowwise() |>
      mutate(cooking_fuel_price_per_11kg = median(c(
        q_fuel_public_price_per_11kg,
        q_fuel_private_price_per_11kg
      ), na.rm = TRUE)) |>
      ungroup() |>
      pivot_longer(-{{ admin_level_col }}, names_to = "item", values_to = "median_item_price")
  } else if (admin_level == "region") {
    ignored_municipalities <- c(
      "Abusliem",
      "Ain Zara",
      "Hai Alandalus",
      "Suq Aljumaa",
      "Tajoura",
      "Tripoli Center"
    )

    # A recursive call to get municipality medians
    base_medians_df <- admin_level_medians(df, admin_level = "municipality") |>
      filter(!q_municipality %in% ignored_municipalities) |>
      inner_join(lby_municipalities, by = c("q_municipality" = "municipality_name_en")) |>
      inner_join(lby_districts) |>
      inner_join(lby_regions) |>
      select(-ends_with("_id"), -ends_with("_ar")) |>
      dplyr::rename(q_region = region_name_en)

    tripoli_medians <- admin_level_medians(jmmi_2022_feb, "district") |>
      dplyr::filter(q_district == "Tripoli") |>
      dplyr::rename(q_municipality = q_district) |>
      mutate(district_name_en = "Tripoli", q_region = "West (Tripolitania)")

    medians_df <- dplyr::bind_rows(base_medians_df, tripoli_medians) |>
      group_by(q_region, item) |>
      summarise(median_item_price = median(median_item_price, na.rm = TRUE)) |>
      ungroup()
  } else if (admin_level == "overall") {
    ignored_municipalities <- c(
      "Abusliem",
      "Ain Zara",
      "Hai Alandalus",
      "Suq Aljumaa",
      "Tajoura",
      "Tripoli Center"
    )

    # A recursive call to get municipality medians
    base_medians_df <- admin_level_medians(df, admin_level = "municipality") |>
      filter(!q_municipality %in% ignored_municipalities)

    tripoli_medians <- admin_level_medians(df, "district") |>
      dplyr::filter(q_district == "Tripoli") |>
      dplyr::rename(q_municipality = q_district)

    medians_df <- dplyr::bind_rows(base_medians_df, tripoli_medians) |>
      group_by(item) |>
      summarise(median_item_price = median(median_item_price, na.rm = TRUE)) |>
      ungroup()
  }

  return(medians_df)
}
