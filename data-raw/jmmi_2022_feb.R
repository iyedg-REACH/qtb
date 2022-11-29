## code to prepare `jmmi_2022_feb` dataset goes here

URL <- "https://www.impact-repository.org/document/reach/a5964fda/reach_lby_dataset_joint_market_monitoring_initiative_jmmi_February_2022.xlsx"
jmmi_2022_feb_file <- tempfile(fileext = ".xlsx")

httr::GET(URL, httr::write_disk(jmmi_2022_feb_file, overwrite = TRUE))
jmmi_2022_feb <- openxlsx::read.xlsx(jmmi_2022_feb_file,
  sheet = "Clean Data",
  startRow = 2
) |>
  tibble::as_tibble()

usethis::use_data(jmmi_2022_feb, overwrite = TRUE)

##### Internal Data

get_city_medians <- function(rows, cols, item_group) {
  openxlsx::read.xlsx(jmmi_2022_feb_file,
    sheet = "City Medians",
    rows = rows, cols = cols,
    na.strings = c("NA")
  ) |>
    dplyr::rename(q_municipality = .data[["X1"]]) |>
    tidyr::pivot_longer(
      cols = -.data[["q_municipality"]],
      names_to = "item",
      values_to = "median_item_price", values_transform = as.numeric
    ) |>
    dplyr::mutate(group = item_group) |>
    tibble::as_tibble()
}

city_medians_food <- get_city_medians(rows = 6:50, cols = 1:25, item_group = "food")
city_medians_hygiene <- get_city_medians(rows = 58:102, cols = 1:12, item_group = "hygiene") |>
  dplyr::mutate(
    median_item_price = dplyr::case_when(
      item == "q_lsoap_price_per_kilo" & q_municipality == "Tripoli" ~ 4.5,
      item == "q_shampoo_price_per_250ml" & q_municipality == "Tripoli" ~ 5.875,
      item == "q_toothbrush_price_per_brush" & q_municipality == "Tripoli" ~ 3.225,
      item == "q_lsoap_price_per_kilo" & q_municipality == "Median West" ~ 5.5,
      item == "q_shampoo_price_per_250ml" & q_municipality == "Median West" ~ 5.365,
      item == "q_toothbrush_price_per_brush" & q_municipality == "Median West" ~ 2.125,
      item == "q_lsoap_price_per_kilo" & q_municipality == "MEDIAN Overall" ~ 6.5,
      item == "q_shampoo_price_per_250ml" & q_municipality == "MEDIAN Overall" ~ 5.9375,
      item == "q_toothbrush_price_per_brush" & q_municipality == "MEDIAN Overall" ~ 2.375,
      TRUE ~ median_item_price
    )
  )

city_medians_cooking_fuel <- get_city_medians(rows = 110:154, cols = 1:4, item_group = "cooking_fuel") |>
  tidyr::pivot_wider(id_cols = q_municipality, names_from = item, values_from = median_item_price) |>
  dplyr::rowwise() |>
  dplyr::mutate(cooking_fuel_price_per_11kg = median(c(
    q_fuel_public_price_per_11kg, q_fuel_private_price_per_11kg
  ), na.rm = TRUE)) |>
  dplyr::ungroup() |>
  tidyr::pivot_longer(
    cols = -q_municipality,
    names_to = "item",
    values_to = "median_item_price", values_transform = as.numeric
  ) |>
  dplyr::mutate(group = "cooking_fuel") |>
  tibble::as_tibble()

city_medians_pharmaceutical <- get_city_medians(rows = 162:206, cols = 1:6, item_group = "pharmaceutical")
city_medians_gasoline <- get_city_medians(rows = 214:258, cols = 1:3, item_group = "gasoline")

city_medians <- dplyr::bind_rows(
  city_medians_food,
  city_medians_hygiene,
  city_medians_cooking_fuel,
  city_medians_pharmaceutical,
  city_medians_gasoline
)

meb_costs <- openxlsx::read.xlsx(jmmi_2022_feb_file,
  sheet = "Cost of MEB",
  na.strings = c("NA")
) |>
  dplyr::as_tibble() |>
  janitor::clean_names() |>
  dplyr::rename(
    q_municipality = city,
    meb_cost = cost_of_all_meb_key_elements_in_lyd,
    food_meb_cost = cost_of_food_portion_of_meb_in_lyd,
    nfi_meb_cost = cost_of_nfi_portion_of_meb_in_lyd,
    fuel_meb_cost = cost_of_fuel_portion_of_meb_in_lyd
  ) |>
  dplyr::select(
    q_municipality,
    meb_cost,
    food_meb_cost,
    nfi_meb_cost,
    fuel_meb_cost
  ) |>
  tidyr::pivot_longer(-q_municipality, names_to = "meb", values_to = "cost")

usethis::use_data(
  meb_costs,
  city_medians,
  internal = TRUE,
  overwrite = TRUE
)
