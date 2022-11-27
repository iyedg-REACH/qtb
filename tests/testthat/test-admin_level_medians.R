test_that("raises error with invalid item group", {
  expected_error <- "foodz is not in food, hygiene, cooking_fuel, pharmaceutical, gasoline"
  expect_error(admin_level_medians(jmmi_2022_feb, q_region, "foodz"), expected_error)
})

test_that("does not raise error with valid item group", {
  expect_error(admin_level_medians(jmmi_2022_feb, q_region, "hygiene"), NA)
})


test_that("return all groups when none specified", {
  all_groups_medians <- admin_level_medians(jmmi_2022_feb, q_region)

  food_medians <- admin_level_medians(jmmi_2022_feb, q_region, "food")
  food_items <- meb_weights |>
    dplyr::filter(.data[["group"]] == "food") |>
    dplyr::pull(.data[["item"]])

  expect_equal(
    all_groups_medians |> dplyr::filter(.data[["item"]] %in% food_items),
    food_medians
  )

  pharmaceutical_medians <- admin_level_medians(jmmi_2022_feb, q_region, "pharmaceutical")
  pharmaceutical_items <- meb_weights |>
    dplyr::filter(.data[["group"]] == "pharmaceutical") |>
    dplyr::pull(.data[["item"]])

  expect_equal(
    all_groups_medians |> dplyr::filter(.data[["item"]] %in% pharmaceutical_items),
    pharmaceutical_medians
  )

  cooking_fuel_medians <- admin_level_medians(jmmi_2022_feb, q_region, "cooking_fuel")
  cooking_fuel_items <- meb_weights |>
    dplyr::filter(.data[["group"]] == "cooking_fuel") |>
    dplyr::pull(.data[["item"]])

  expect_equal(
    all_groups_medians |> dplyr::filter(.data[["item"]] %in% cooking_fuel_items),
    cooking_fuel_medians
  )

  expect_setequal(
    all_groups_medians |> dplyr::distinct(.data[["item"]]) |> dplyr::pull(.data[["item"]]),
    meb_weights |> dplyr::distinct(.data[["item"]]) |> dplyr::pull(.data[["item"]])
  )
})
