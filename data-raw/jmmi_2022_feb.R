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

get_city_medians <- function(rows, cols) {
  openxlsx::read.xlsx(jmmi_2022_feb_file,
    sheet = "City Medians",
    rows = rows, cols = cols,
    na.strings = c("NA")
  ) |>
    dplyr::rename(q_municipality = X1) |>
    tidyr::pivot_longer(
      cols = -q_municipality,
      names_to = "item",
      values_to = "median_item_price", values_transform = as.numeric
    ) |>
    tibble::as_tibble()
}

city_medians_food <- get_city_medians(rows = 6:50, cols = 1:25)
city_medians_hygiene <- get_city_medians(rows = 58:102, cols = 1:12)
city_medians_cooking_fuel <- get_city_medians(rows = 110:154, cols = 1:4)
city_medians_pharmaceutical <- get_city_medians(rows = 162:206, cols = 1:6)
city_medians_gasoline <- get_city_medians(rows = 214:258, cols = 1:3)

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
  select(
    q_municipality,
    meb_cost,
    food_meb_cost,
    nfi_meb_cost,
    fuel_meb_cost
  ) |>
  tidyr::pivot_longer(-q_municipality, names_to = "meb", values_to = "cost")

usethis::use_data(
  city_medians_food,
  city_medians_hygiene,
  city_medians_cooking_fuel,
  city_medians_pharmaceutical,
  city_medians_gasoline,
  meb_costs,
  internal = TRUE,
  overwrite = TRUE
)
