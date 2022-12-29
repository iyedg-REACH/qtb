test_that("meb computation with an item group matches expected", {
  expected_meb_cost <- meb_costs_feb |>
    mutate(meb = dplyr::case_when(
      grepl("food", meb) ~ "food",
      grepl("nfi_", meb) ~ "hygiene",
      grepl("fuel_", meb) ~ "cooking_fuel",
      TRUE ~ meb
    )) |>
    dplyr::rename(
      group = meb
    )

  region_computed_meb_cost <- compute_meb_cost(jmmi_2022_feb, "region", precision = 5) |>
    filter(!group %in% c("gasoline", "pharmaceutical")) |>
    mutate(q_region = stringr::str_extract(q_region, "^\\w+")) |>
    dplyr::arrange(q_region, group)

  region_expected_meb_cost <- expected_meb_cost |>
    filter(grepl("edian", q_municipality), group != "meb_cost") |>
    dplyr::rename(q_region = q_municipality, meb_cost = cost) |>
    mutate(q_region = stringr::str_extract(q_region, "\\w+$")) |>
    dplyr::arrange(q_region, group)

  expect_equal(
    region_computed_meb_cost,
    region_expected_meb_cost
  )

  tripoli_computed_meb_cost <- compute_meb_cost(jmmi_2022_feb, "district", precision = 5) |>
    filter(!group %in% c("gasoline", "pharmaceutical"), q_district == "Tripoli") |>
    mutate(q_district = tolower(q_district)) |>
    dplyr::arrange(q_district, group)


  tripoli_expected_meb_cost <- expected_meb_cost |>
    filter(
      group != "meb_cost",
      q_municipality == "Tripoli"
    ) |>
    mutate(q_municipality = tolower(q_municipality)) |>
    dplyr::arrange(q_municipality, group) |>
    dplyr::rename(meb_cost = cost, q_district = q_municipality)

  expect_equal(
    tripoli_computed_meb_cost,
    tripoli_expected_meb_cost
  )

  municipalities_computed_meb_cost <- compute_meb_cost(jmmi_2022_feb, "municipality", precision = 6) |>
    filter(!group %in% c("gasoline", "pharmaceutical")) |>
    mutate(q_municipality = tolower(q_municipality)) |>
    dplyr::arrange(q_municipality, group)

  municipalities_expected_meb_cost <- expected_meb_cost |>
    filter(
      !grepl("median", tolower(q_municipality)),
      group != "meb_cost",
      q_municipality != "Tripoli"
    ) |>
    mutate(q_municipality = tolower(q_municipality)) |>
    dplyr::arrange(q_municipality, group) |>
    dplyr::rename(meb_cost = cost)

  expect_equal(
    municipalities_computed_meb_cost,
    municipalities_expected_meb_cost
  )
})
