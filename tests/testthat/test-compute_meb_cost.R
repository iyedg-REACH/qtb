get_expected_item_group <- function(item_group) {
  if (item_group == "food") {
    meb_group <- "food_meb_cost"
  } else if (item_group == "hygiene") {
    meb_group <- "nfi_meb_cost"
  } else if (item_group == "cooking_fuel") {
    meb_group <- "fuel_meb_cost"
  }

  meb_costs |>
    dplyr::mutate(q_municipality = stringr::str_to_lower(q_municipality)) |>
    dplyr::filter(
      meb == meb_group,
      q_municipality != "tripoli", # Tripoli is ignore because it is a composite of municipalities
      stringr::str_detect(
        q_municipality,
        "median",
        negate = TRUE
      )
    ) |>
    dplyr::select(-all_of("meb")) |>
    dplyr::rename(meb_cost = cost) |>
    dplyr::arrange(q_municipality, meb_cost)
}

prepare_result <- function(df) {
  df |>
    dplyr::mutate(q_municipality = stringr::str_to_lower(q_municipality)) |>
    dplyr::arrange(q_municipality, meb_cost)
}

test_that("meb computation with an item group matches expected", {
  expected_food_group <- get_expected_item_group("food")

  food_result <- compute_meb_cost(
    df = jmmi_2022_feb,
    admin_level_col = q_municipality,
    item_group = "food"
  ) |>
    prepare_result()

  expect_equal(food_result, expected_food_group)

  expected_hygiene_group <- get_expected_item_group("hygiene")


  hygiene_result <- compute_meb_cost(
    df = jmmi_2022_feb,
    admin_level_col = q_municipality,
    item_group = "hygiene"
  ) |>
    prepare_result()

  expect_equal(hygiene_result, expected_hygiene_group)

  expected_fuel_group <- get_expected_item_group("cooking_fuel")

  fuel_result <- compute_meb_cost(
    df = jmmi_2022_feb,
    admin_level_col = q_municipality,
    item_group = "cooking_fuel"
  ) |>
    prepare_result()

  expect_equal(fuel_result, expected_fuel_group)
})
