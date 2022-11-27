test_that(
  "computed medians match expected medians",
  {
    municipality_computed_medians <- admin_level_medians(jmmi_2022_feb, "municipality") |>
      dplyr::mutate(q_municipality = tolower(q_municipality)) |>
      dplyr::select(dplyr::all_of(names(city_medians))) |>
      dplyr::arrange(q_municipality, item)

    municipality_expected_medians <- city_medians |>
      dplyr::mutate(q_municipality = tolower(q_municipality)) |>
      dplyr::filter(!(grepl("median", q_municipality) | grepl("tripoli$", q_municipality))) |>
      dplyr::arrange(q_municipality, item)

    expect_equal(
      municipality_computed_medians,
      municipality_expected_medians
    )

    tripoli_computed_medians <- admin_level_medians(jmmi_2022_feb, "district") |>
      dplyr::filter(q_district == "Tripoli") |>
      dplyr::rename(q_municipality = q_district) |>
      dplyr::select(dplyr::all_of(names(city_medians))) |>
      dplyr::arrange(q_municipality, item)

    # This test contains manual corrections
    tripoli_expected_medians <- city_medians |>
      dplyr::filter(grepl("Tripoli$", q_municipality)) |>
      dplyr::arrange(q_municipality, item) |>
      dplyr::mutate(
        median_item_price = dplyr::case_when(
          item == "q_lsoap_price_per_kilo" ~ 4.5,
          item == "q_shampoo_price_per_250ml" ~ 5.875,
          item == "q_toothbrush_price_per_brush" ~ 3.225,
          TRUE ~ median_item_price
        )
      )

    expect_equal(
      tripoli_computed_medians,
      tripoli_expected_medians
    )

    region_computed_medians <- admin_level_medians(jmmi_2022_feb, "region") |>
      dplyr::rename(q_municipality = q_region) |>
      mutate(q_municipality = stringr::str_extract(q_municipality, "^\\w+")) |>
      dplyr::select(dplyr::all_of(names(city_medians))) |>
      dplyr::arrange(q_municipality, item)


    region_expected_medians <- city_medians |>
      filter(grepl("edian", q_municipality)) |>
      mutate(q_municipality = stringr::str_extract(q_municipality, "\\w+$")) |>
      dplyr::arrange(q_municipality, item) |>
      dplyr::mutate(
        median_item_price = dplyr::case_when(
          item == "q_lsoap_price_per_kilo" & q_municipality == "West" ~ 5.5,
          item == "q_shampoo_price_per_250ml" & q_municipality == "West" ~ 5.365,
          item == "q_toothbrush_price_per_brush" & q_municipality == "West" ~ 2.125,
          TRUE ~ median_item_price
        )
      )

    expect_equal(
      region_computed_medians,
      region_expected_medians
    )
  }
)
