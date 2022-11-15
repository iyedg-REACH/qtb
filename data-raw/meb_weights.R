meb_weights <- readr::read_csv(
  here::here("data-raw/meb_weights.csv"),
  show_col_types = FALSE
)

usethis::use_data(meb_weights, overwrite = TRUE)
