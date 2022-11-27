test_that("median calculations match expected", {
  # TODO:refactor into DRY tests

  expected <- city_medians_food |>
    dplyr::filter(
      !stringr::str_detect(stringr::str_to_lower(q_municipality), "median"),
      q_municipality != "Tripoli" # Ignoring Tripoli as it is the composite value for the municipalities of the Tripoli District
    ) |>
    dplyr::mutate(q_municipality = stringr::str_to_lower(q_municipality)) |>
    dplyr::arrange(q_municipality, item, median_item_price)

  result <- admin_level_medians(
    df = jmmi_2022_feb,
    admin_level_col = q_municipality,
    item_group = "food"
  ) |>
    dplyr::mutate(q_municipality = stringr::str_to_lower(q_municipality)) |>
    dplyr::arrange(q_municipality, item, median_item_price)

  expect_equal(object = result, expected = expected)

  ## Hygiene
  expected <- city_medians_hygiene |>
    dplyr::filter(
      !stringr::str_detect(stringr::str_to_lower(q_municipality), "median"),
      q_municipality != "Tripoli" # Ignoring Tripoli as it is the composite value for the municipalities of the Tripoli District
    ) |>
    dplyr::mutate(q_municipality = stringr::str_to_lower(q_municipality)) |>
    dplyr::arrange(q_municipality, item, median_item_price)

  result <- admin_level_medians(
    df = jmmi_2022_feb,
    admin_level_col = q_municipality,
    item_group = "hygiene"
  ) |>
    dplyr::mutate(q_municipality = stringr::str_to_lower(q_municipality)) |>
    dplyr::arrange(q_municipality, item, median_item_price)

  expect_equal(object = result, expected = expected)


  ## gasoline
  expected <- city_medians_gasoline |>
    dplyr::filter(
      !stringr::str_detect(stringr::str_to_lower(q_municipality), "median"),
      q_municipality != "Tripoli" # Ignoring Tripoli as it is the composite value for the municipalities of the Tripoli District
    ) |>
    dplyr::mutate(q_municipality = stringr::str_to_lower(q_municipality)) |>
    dplyr::arrange(q_municipality, item, median_item_price)

  result <- admin_level_medians(
    df = jmmi_2022_feb,
    admin_level_col = q_municipality,
    item_group = "gasoline"
  ) |>
    dplyr::mutate(q_municipality = stringr::str_to_lower(q_municipality)) |>
    dplyr::arrange(q_municipality, item, median_item_price)

  expect_equal(object = result, expected = expected)


  ## cooking fuel
  expected <- city_medians_cooking_fuel |>
    dplyr::filter(
      !stringr::str_detect(stringr::str_to_lower(q_municipality), "median"),
      q_municipality != "Tripoli", # Ignoring Tripoli as it is the composite value for the municipalities of the Tripoli District
    ) |>
    dplyr::mutate(q_municipality = stringr::str_to_lower(q_municipality)) |>
    dplyr::arrange(q_municipality, item, median_item_price)

  result <- admin_level_medians(
    df = jmmi_2022_feb,
    admin_level_col = q_municipality,
    item_group = "cooking_fuel"
  ) |>
    dplyr::mutate(q_municipality = stringr::str_to_lower(q_municipality)) |>
    dplyr::arrange(q_municipality, item, median_item_price)

  expect_equal(object = result, expected = expected)
})
