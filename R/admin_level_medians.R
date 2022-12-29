#' Compute Median Prices per Administrative Level
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param df Monthly JMMI clean dataset
#' @param admin_level an administrative level in Libya, can be one of "municipality", "district", "region", or "overall"
#' @param precision The number of decimal places to keep when rounding results
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
                                admin_level = "municipality",
                                precision = 3) {
  admin_levels <- c("municipality", "district", "region", "overall")
  assertthat::assert_that(
    admin_level %in% admin_levels,
    msg = paste0("Invalid admin level, must be one of ", paste0(admin_levels, collapse = ", "))
  )

  admin_level_col <- switch(admin_level,
    "municipality" = rlang::sym("q_municipality"),
    "district" = rlang::sym("q_district"),
    "region" = rlang::sym("q_region"),
    "overall" = rlang::sym("q_country")
  )

  ## The medians for municipalities and districts are computed based on
  ## the non-summarized data. The medians for regions and overall medians
  ## are computed based on municipalities' medians

  if (admin_level %in% c("municipality", "district")) {
    medians_df <- df |>
      select({{ admin_level_col }}, pull(qtb::meb_weights, "item")) |>
      pivot_longer(-{{ admin_level_col }}, names_to = "item", values_to = "price") |>
      group_by({{ admin_level_col }}, .data[["item"]]) |>
      summarise(median_item_price = median(.data[["price"]], na.rm = TRUE)) |>
      ungroup()
  } else {
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
      filter(!.data[["q_municipality"]] %in% ignored_municipalities) |>
      inner_join(qtb::lby_municipalities, by = c("q_municipality" = "municipality_name_en")) |>
      inner_join(qtb::lby_districts, by = c("region_id", "district_id")) |>
      inner_join(qtb::lby_regions, by = c("region_id")) |>
      select(-dplyr::ends_with("_id"), -dplyr::ends_with("_ar")) |>
      dplyr::rename(
        q_region = "region_name_en",
        q_district = "district_name_en"
      ) # region is added in case the admin level is by region

    tripoli_medians <- admin_level_medians(df, "district") |>
      dplyr::filter(.data[["q_district"]] == "Tripoli") |>
      dplyr::rename(q_municipality = "q_district") |>
      mutate(q_district = "Tripoli", q_region = "West (Tripolitania)")

    base_medians_df <- dplyr::bind_rows(base_medians_df, tripoli_medians) |>
      mutate(q_country = "Libya")

    medians_df <- base_medians_df |>
      group_by({{ admin_level_col }}, .data[["item"]]) |>
      summarise(median_item_price = median(.data[["median_item_price"]], na.rm = TRUE)) |>
      ungroup()
  }

  medians_df |>
    pivot_wider(
      id_cols = {{ admin_level_col }},
      names_from = "item",
      values_from = "median_item_price"
    ) |>
    dplyr::rowwise() |>
    mutate(
      cooking_fuel_price_per_11kg = median(
        c(.data[["q_fuel_public_price_per_11kg"]], .data[["q_fuel_private_price_per_11kg"]]),
        na.rm = TRUE
      )
    ) |>
    pivot_longer(
      cols = -{{ admin_level_col }},
      names_to = "item",
      values_to = "median_item_price"
    ) |>
    mutate(median_item_price = round(median_item_price, digits = precision))
}
