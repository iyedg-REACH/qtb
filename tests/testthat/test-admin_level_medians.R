test_that("raises error with invalid item group", {
  expected_error <- "foodz is not in food, hygiene, cooking_fuel, pharmaceutical, gasoline"
  expect_error(admin_level_medians(jmmi_2022_feb, q_region, "foodz"), expected_error)
})

test_that("does not raise error with valid item group", {
})
