test_that(
  "computed medians match expected medians",
  {
    municipality_computed_medians <- admin_level_medians(jmmi_2022_feb, "municipality", precision = 6) |>
      dplyr::mutate(q_municipality = tolower(q_municipality)) |>
      dplyr::select(q_municipality, item, median_item_price) |>
      dplyr::arrange(q_municipality, item)

    municipality_expected_medians <- city_medians_feb |>
      dplyr::mutate(q_municipality = tolower(q_municipality)) |>
      dplyr::filter(!(grepl("median", q_municipality) | grepl("tripoli$", q_municipality))) |>
      dplyr::select(q_municipality, item, median_item_price) |>
      dplyr::arrange(q_municipality, item)

    expect_equal(
      municipality_computed_medians,
      municipality_expected_medians
    )

    tripoli_computed_medians <- admin_level_medians(jmmi_2022_feb, "district", precision = 6) |>
      dplyr::filter(q_district == "Tripoli") |>
      dplyr::rename(q_municipality = q_district) |>
      dplyr::select(q_municipality, item, median_item_price) |>
      dplyr::arrange(q_municipality, item)

    tripoli_expected_medians <- city_medians_feb |>
      dplyr::filter(grepl("Tripoli$", q_municipality)) |>
      dplyr::select(q_municipality, item, median_item_price) |>
      dplyr::arrange(q_municipality, item)

    expect_equal(
      tripoli_computed_medians,
      tripoli_expected_medians
    )

    region_computed_medians <- admin_level_medians(jmmi_2022_feb, "region", precision = 6) |>
      dplyr::rename(q_municipality = q_region) |>
      mutate(q_municipality = stringr::str_extract(q_municipality, "^\\w+")) |>
      dplyr::select(q_municipality, item, median_item_price) |>
      dplyr::arrange(q_municipality, item)

    region_expected_medians <- city_medians_feb |>
      filter(grepl("edian", q_municipality)) |>
      mutate(q_municipality = stringr::str_extract(q_municipality, "\\w+$")) |>
      dplyr::select(q_municipality, item, median_item_price) |>
      dplyr::arrange(q_municipality, item)

    expect_equal(
      region_computed_medians,
      region_expected_medians
    )

    overall_computed_medians <- admin_level_medians(jmmi_2022_feb, admin_level = "overall", precision = 6) |>
      select(any_of(names(city_medians_feb))) |>
      dplyr::select(item, median_item_price) |>
      dplyr::arrange(item)

    overall_expected_medians <- city_medians_feb |>
      filter(grepl("verall", q_municipality)) |>
      dplyr::select(item, median_item_price) |>
      dplyr::arrange(item)

    expect_equal(
      overall_computed_medians,
      overall_expected_medians
    )
  }
)
